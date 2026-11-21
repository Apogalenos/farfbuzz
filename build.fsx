// Farfbuzz build script: `dotnet fsi build.fsx [stage ...]`. With no stage, runs `default`.

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions

/// <summary>The repository root, where this script lives.</summary>
let root = __SOURCE_DIRECTORY__

/// <summary>The runtime identifier of the machine running the script, for example <c>osx-x64</c>.</summary>
let hostRid = RuntimeInformation.RuntimeIdentifier

/// <summary>What a step runs.</summary>
type Action =
    /// <summary>Runs a program and requires exit code zero.</summary>
    | Run of program: string * arguments: string list * directory: string
    /// <summary>Runs a program, requires exit code zero and checks its output for offending lines.</summary>
    | RunChecked of program: string * arguments: string list * offending: (string -> bool)
    /// <summary>Runs a program and only reports its exit code.</summary>
    | Probe of program: string * arguments: string list * directory: string
    /// <summary>Runs a computation in the script itself, returning an exit code.</summary>
    | Inline of (unit -> int)

/// <summary>One step of a stage.</summary>
type Step = { Title: string; Action: Action }

/// <summary>Builds a step that runs the dotnet host in the repository root.</summary>
/// <param name="title">What the step does.</param>
/// <param name="arguments">The dotnet arguments.</param>
/// <returns>The step.</returns>
let dotnet title arguments = { Title = title; Action = Run ("dotnet", arguments, root) }

/// <summary>Runs an Expecto test executable already built in Release.</summary>
/// <param name="project">The project folder under the repository root.</param>
/// <param name="extra">Extra Expecto arguments.</param>
/// <returns>The step.</returns>
let expecto project extra =
    dotnet $"run {project}" ([ "run"; "--project"; project; "-c"; "Release"; "--no-build"; "--"; "--summary" ] @ extra)

/// <summary>Recognises a trimming or AOT warning that does not come from FSharp.Core's reflection-based printing.</summary>
/// <param name="line">One line of publish output.</param>
/// <returns>True for a warning that must fail the build.</returns>
let isForeignTrimWarning (line: string) =
    Regex.IsMatch(line, @"warning IL\d{4}") && not (Regex.IsMatch(line, @"sformat\.fs|printf\.fs|Microsoft\.FSharp\."))

/// <summary>Publishes the smoke project for a framework, failing on any trimming or AOT warning of our own.</summary>
/// <param name="title">What the step does.</param>
/// <param name="arguments">Extra publish arguments.</param>
/// <returns>The step.</returns>
let publishSmoke title arguments =
    { Title = title
      Action = RunChecked ("dotnet", [ "publish"; "tests/Farfbuzz.Smoke"; "-c"; "Release" ] @ arguments, isForeignTrimWarning) }

/// <summary>The NativeAOT smoke binary for the host.</summary>
let nativeSmoke = Path.Combine(root, "artifacts", "publish", "Farfbuzz.Smoke", $"release_net10.0_{hostRid}", "Farfbuzz.Smoke")

/// <summary>The browser bundle of the smoke project, where <c>main.mjs</c> runs under Node.</summary>
let wasmBundle = Path.Combine(root, "artifacts", "bin", "Farfbuzz.Smoke", "release_net10.0-browser_browser-wasm", "AppBundle")

/// <summary>Runs the smoke binaries, then the probe that is allowed to overflow.</summary>
/// <param name="flavour">A label for the build being exercised.</param>
/// <param name="program">The program to run.</param>
/// <param name="prefix">Arguments placed before the smoke arguments.</param>
/// <param name="directory">The working directory.</param>
/// <returns>The gate step, the planted-failure step and the probe step.</returns>
let smokeRuns flavour program prefix directory =
    [ { Title = $"smoke gate ({flavour})"; Action = Run (program, prefix, directory) }
      { Title = $"planted failure must fail ({flavour})"
        Action = Inline (fun () ->
            let info = ProcessStartInfo(program, prefix @ [ "--plant-failure" ], WorkingDirectory = directory, RedirectStandardOutput = true)
            use proc = Process.Start info
            proc.StandardOutput.ReadToEnd() |> ignore
            proc.WaitForExit()
            if proc.ExitCode = 0 then 1 else 0) }
      { Title = $"mutual recursion probe ({flavour})"; Action = Probe (program, prefix @ [ "--probe-mutual" ], directory) } ]

/// <summary>Starts a process and waits for it.</summary>
/// <param name="program">The program.</param>
/// <param name="arguments">Its arguments.</param>
/// <param name="directory">The working directory.</param>
/// <param name="capture">Whether to capture standard output and error instead of streaming them.</param>
/// <returns>The exit code and the captured lines.</returns>
let execute program (arguments: string list) directory capture =
    let info =
        ProcessStartInfo(
            program, arguments, WorkingDirectory = directory, UseShellExecute = false,
            RedirectStandardOutput = capture, RedirectStandardError = capture)
    use proc = Process.Start info
    let output =
        match capture with
        | true ->
            let error = proc.StandardError.ReadToEndAsync()
            let standard = proc.StandardOutput.ReadToEnd()
            (standard + error.Result).Split('\n') |> Array.toList
        | false -> []
    proc.WaitForExit()
    proc.ExitCode, output

/// <summary>The folder where the oracle for this machine is built.</summary>
let oracleDirectory = Path.Combine(root, "artifacts", "oracle", hostRid)

/// <summary>The oracle library file name on this operating system.</summary>
let oracleLibrary =
    match OperatingSystem.IsWindows(), OperatingSystem.IsMacOS() with
    | true, _ -> "harfbuzz-oracle.dll"
    | false, true -> "libharfbuzz-oracle.dylib"
    | false, false -> "libharfbuzz-oracle.so"

/// <summary>The C++ compiler: <c>CXX</c> when set, otherwise <c>c++</c>.</summary>
let cxx =
    match Environment.GetEnvironmentVariable "CXX" with
    | null | "" -> "c++"
    | compiler -> compiler

/// <summary>Flags shared by every oracle compilation, mirroring upstream's meson defaults.</summary>
let commonFlags = [ "-std=c++11"; "-O2"; "-fno-exceptions"; "-fno-rtti"; "-fno-threadsafe-statics"; "-DHB_NO_FEATURES_H"; "-I"; "upstream/harfbuzz/src" ]

/// <summary>Reads the glib compiler and linker flags from pkg-config, needed by <c>hb-shape</c>.</summary>
/// <returns>The flags, or none when pkg-config or glib is missing.</returns>
let glibFlags () =
    let code, lines = execute "pkg-config" [ "--cflags"; "--libs"; "glib-2.0" ] root true
    match code with
    | 0 -> Some (lines |> List.collect (fun line -> line.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.toList))
    | _ -> None

/// <summary>Builds the HarfBuzz oracle library and <c>hb-shape</c> from the submodule for this machine.</summary>
/// <returns>Zero on success.</returns>
let buildOracle () =
    Directory.CreateDirectory oracleDirectory |> ignore
    match OperatingSystem.IsWindows() with
    | true ->
        let output = Path.Combine(oracleDirectory, oracleLibrary)
        fst (execute "cl" [ "/nologo"; "/O2"; "/LD"; "/EHs-c-"; "/GR-"; "/DHB_NO_FEATURES_H"; "/Iupstream\\harfbuzz\\src"; "tools\\oracle\\oracle.cc"; $"/Fe:{output}" ] root false)
    | false ->
        let threads = if OperatingSystem.IsLinux() then [ "-pthread" ] else []
        let library =
            execute cxx (commonFlags @ [ "-fPIC"; "-shared" ] @ threads @ [ "tools/oracle/oracle.cc"; "-o"; Path.Combine(oracleDirectory, oracleLibrary) ]) root false
        match fst library, glibFlags () with
        | 0, Some glib ->
            fst (execute cxx (commonFlags @ threads @ [ "upstream/harfbuzz/src/harfbuzz.cc"; "upstream/harfbuzz/util/hb-shape.cc"; "-o"; Path.Combine(oracleDirectory, "hb-shape") ] @ glib) root false)
        | 0, None ->
            Console.Error.WriteLine "glib-2.0 not found through pkg-config: hb-shape not built, so the run-tests.py equality check will report itself skipped."
            0
        | failure, _ -> failure

/// <summary>Builds the oracle only when its library is absent.</summary>
/// <returns>Zero on success.</returns>
let ensureOracle () =
    match File.Exists(Path.Combine(oracleDirectory, oracleLibrary)) with
    | true -> 0
    | false -> buildOracle ()

/// <summary>The NativeAOT facade probe for the host.</summary>
let nativeFacadeProbe = Path.Combine(root, "artifacts", "publish", "Farfbuzz.Facade.Probe", $"release_net10.0_{hostRid}", "Farfbuzz.Facade.Probe")

/// <summary>The browser bundle of the facade probe.</summary>
let wasmFacadeBundle = Path.Combine(root, "artifacts", "bin", "Farfbuzz.Facade.Probe", "release_net10.0-browser_browser-wasm", "AppBundle")

/// <summary>Adds the lines of a findings file to a baseline, keeping its comments first and its entries sorted; never removes one.</summary>
/// <param name="findings">The file of newly passing entries, under artifacts/conformance.</param>
/// <param name="baseline">The committed baseline.</param>
/// <returns>Zero.</returns>
let mergeBaseline (findings: string) (baseline: string) =
    let keyOf (line: string) = line.Split(' ', StringSplitOptions.RemoveEmptyEntries)[0]
    let existing = if File.Exists baseline then File.ReadAllLines baseline |> Array.toList else []
    let comments = existing |> List.filter (fun line -> line.StartsWith "#")
    let entries = existing |> List.filter (fun line -> line.Length > 0 && not (line.StartsWith "#"))
    let known = entries |> List.map keyOf |> Set.ofList
    let additions =
        match File.Exists findings with
        | false -> []
        | true -> File.ReadAllLines findings |> Array.toList |> List.filter (fun line -> line.Length > 0 && not (known.Contains (keyOf line))) |> List.distinct
    File.WriteAllLines(baseline, comments @ List.sort (entries @ additions))
    Console.WriteLine $"    {additions.Length} new entries in {Path.GetFileName baseline}"
    0

/// <summary>Each Farfbuzz module and the upstream functions it ports, for the complexity comparison (an empty list means every function of the file).</summary>
let metricsMap =
    [ "Tag.fs", [ ("src/hb-common.cc", [ "hb_tag_from_string"; "hb_tag_to_string" ]) ]
      "Common.fs",
        [ "src/hb-common.cc",
           [ "hb_direction_from_string"; "hb_direction_to_string"; "hb_language_from_string"; "hb_language_to_string"
             "hb_language_matches"; "hb_script_from_iso15924_tag"; "hb_script_from_string"
             "hb_script_get_horizontal_direction" ] ]
      "UnicodeProps.fs",
        [ "src/hb-ot-layout.hh",
           [ "_hb_glyph_info_set_unicode_props"; "_hb_glyph_info_get_general_category"; "_hb_glyph_info_set_general_category"
             "_hb_glyph_info_is_unicode_mark"; "_hb_glyph_info_get_modified_combining_class"; "_hb_glyph_info_set_modified_combining_class"
             "_hb_glyph_info_is_unicode_space"; "_hb_glyph_info_get_unicode_space_fallback_type"; "_hb_glyph_info_set_unicode_space_fallback_type"
             "_hb_glyph_info_is_variation_selector"; "_hb_glyph_info_set_variation_selector"; "_hb_glyph_info_is_default_ignorable"
             "_hb_glyph_info_clear_default_ignorable"; "_hb_glyph_info_is_hidden"; "_hb_glyph_info_unhide"
             "_hb_glyph_info_set_continuation"; "_hb_glyph_info_is_continuation"; "_hb_glyph_info_is_zwnj"; "_hb_glyph_info_is_zwj" ] ]
      "OtTag.fs",
        [ "src/hb-ot-tag.cc",
           [ "hb_ot_old_tag_from_script"; "hb_ot_old_tag_to_script"; "hb_ot_new_tag_from_script"; "hb_ot_new_tag_to_script"
             "hb_ot_all_tags_from_script"; "subtag_matches"; "lang_matches"; "bfind_tag"; "hb_ot_tags_from_language"
             "parse_private_use_subtag"; "hb_ot_tag_to_script"; "hb_ot_tags_from_script_and_language"
             "hb_ot_tag_to_language"; "hb_ot_tags_to_script_and_language" ]
          "src/hb-ot-tag-table.hh", [ "hb_ot_tags_from_complex_language"; "hb_ot_ambiguous_tag_to_language" ] ]
      "Generated/EmojiTables.fs", [ "src/hb-unicode-emoji-table.hh", [ "_hb_emoji_is_Extended_Pictographic" ] ]
      "Syntax.fs + Feature.fs",
        [ ("src/hb-common.cc",
           [ "parse_space"; "parse_char"; "parse_uint"; "parse_uint32"; "parse_bool"; "parse_feature_value_prefix"; "parse_tag"
             "parse_feature_indices"; "parse_feature_value_postfix"; "parse_one_feature"; "hb_feature_from_string"; "hb_feature_to_string" ]) ]
      "Variation.fs", [ ("src/hb-common.cc", [ "parse_variation_value"; "parse_one_variation"; "hb_variation_from_string"; "hb_variation_to_string" ]) ]
      "Generated/UcdTables.fs",
        [ "src/hb-ucd-table.hh", [ "_hb_ucd_gc"; "_hb_ucd_ccc"; "_hb_ucd_bmg"; "_hb_ucd_sc"; "_hb_ucd_dm" ] ]
      "Unicode.fs",
        [ "src/hb-ucd.cc", [ "hb_ucd_general_category"; "hb_ucd_combining_class"; "hb_ucd_mirroring"; "hb_ucd_script"; "hb_ucd_compose"; "hb_ucd_decompose" ]
          "src/hb-unicode.cc", [ "hb_unicode_east_asian_width"; "hb_unicode_decompose_compatibility"; "hb_unicode_modified_combining_class"; "hb_unicode_is_default_ignorable"; "hb_unicode_is_variation_selector"; "hb_unicode_space_fallback_type"; "hb_unicode_vertical_char_for" ] ]
      "Utf.fs",
        [ "src/hb-utf.hh",
           [ "hb_utf8_t::next"; "hb_utf8_t::prev"; "hb_utf16_t::next"; "hb_utf16_t::prev"
             "hb_utf32_t::next"; "hb_utf32_t::prev"; "hb_latin1_t::next"; "hb_latin1_t::prev" ] ]
      "GlyphRun.fs",
        [ "src/hb-buffer.cc",
           [ "hb_buffer_add_utf"; "hb_buffer_add_utf8"; "hb_buffer_add_utf16"; "hb_buffer_add_utf32"
             "hb_buffer_add_latin1"; "hb_buffer_add_codepoints"; "hb_buffer_add"
             "hb_buffer_t::reset"; "hb_buffer_t::clear"; "hb_buffer_t::guess_segment_properties"; "hb_buffer_t::enter"
             "hb_buffer_set_length"; "hb_buffer_append"; "hb_buffer_reverse"; "hb_buffer_reverse_range"
             "hb_buffer_reverse_clusters"; "hb_buffer_t::clear_positions"; "hb_segment_properties_overlay" ] ]
      "Number.fs", [ "src/hb-number.cc", []; "src/hb-number-parser.hh", [] ]
      "Serialize.fs",
        [ "src/hb-buffer-serialize.cc", [ "_hb_buffer_serialize_glyphs_text"; "hb_buffer_serialize_glyphs"; "hb_buffer_deserialize_glyphs"; "parse_int"; "parse_uint" ]
          "src/hb-buffer-deserialize-text-glyphs.hh", [] ]
      "Digest.fs", [ "src/hb-set-digest.hh", [] ]
      "Binary.fs", [ "src/hb-algs.hh", [ "hb_bsearch_impl" ] ]
      "Sanitize.fs",
        [ "src/hb-sanitize.hh",
           [ "hb_sanitize_context_t::start_processing"; "hb_sanitize_context_t::check_ops"; "hb_sanitize_context_t::check_range"
             "hb_sanitize_context_t::check_struct"; "hb_sanitize_context_t::check_array_sized"; "hb_sanitize_context_t::check_array"
             "hb_sanitize_context_t::sanitize_blob" ] ]
      "OpenType/OpenFile.fs",
        [ "src/hb-open-file.hh",
           [ "OT::OpenTypeFontFile::sanitize"; "OT::OpenTypeFontFile::get_face_count"; "OT::OpenTypeFontFile::get_face"
             "OT::OpenTypeOffsetTable::sanitize"; "OT::OpenTypeOffsetTable::get_table_tags"; "OT::OpenTypeOffsetTable::find_table_index"
             "OT::OpenTypeOffsetTable::get_table_by_tag"; "OT::TTCHeader::sanitize"; "OT::TTCHeader::get_face_count"; "OT::TTCHeader::get_face"
             "OT::TTCHeaderVersion1::sanitize"; "OT::TTCHeaderVersion1::get_face_count"; "OT::TTCHeaderVersion1::get_face"
             "OT::ResourceForkHeader::sanitize"; "OT::ResourceForkHeader::get_face_count"; "OT::ResourceForkHeader::get_face"
             "OT::ResourceMap::sanitize"; "OT::ResourceMap::get_face_count"; "OT::ResourceMap::get_face"
             "OT::ResourceTypeRecord::sanitize"; "OT::ResourceRecord::sanitize" ] ]
      "OpenType/Head.fs", [ "src/hb-ot-head-table.hh", [ "OT::head::get_upem"; "OT::head::sanitize" ] ]
      "OpenType/Maxp.fs",
        [ "src/hb-ot-maxp-table.hh",
           [ "OT::maxp_MAXP::sanitize"; "OT::maxp_MAXP::get_num_glyphs"; "OT::maxpV1Tail::sanitize"
             "OT::maxp_accelerator_t::init_table"; "OT::maxp_accelerator_t::get_num_glyphs" ] ]
      "Face.fs",
        [ "src/hb-face.cc",
           [ "hb_face_count"; "hb_face_create"; "hb_face_create_or_fail"; "hb_face_reference_table"
             "_hb_face_for_data_reference_table"; "_hb_face_for_data_get_table_tags"; "_hb_face_for_data_closure_create" ]
          "src/hb-blob.cc", [ "hb_blob_create_sub_blob" ] ]
      "OpenType/Hhea.fs", [ "src/hb-ot-hhea-table.hh", [ "OT::_hea::sanitize"; "OT::_hea::has_data" ] ]
      "OpenType/Os2.fs",
        [ "src/hb-ot-os2-table.hh",
           [ "OT::OS2::sanitize"; "OT::OS2::has_data"; "OT::OS2::use_typo_metrics"; "OT::OS2::get_font_page"
             "OT::OS2V1Tail::sanitize"; "OT::OS2V2Tail::sanitize"; "OT::OS2V5Tail::sanitize" ] ]
      "OpenType/Hmtx.fs",
        [ "src/hb-ot-hmtx-table.hh",
           [ "OT::hmtxvmtx::accelerator_t::accelerator_t"; "OT::hmtxvmtx::accelerator_t::get_num_long_metrics"
             "OT::hmtxvmtx::accelerator_t::has_data"; "OT::hmtxvmtx::accelerator_t::get_advance_without_var_unscaled"
             "OT::hmtxvmtx::accelerator_t::get_leading_bearing_without_var_unscaled"; "OT::hmtxvmtx::sanitize" ] ]
      "Var/Fixed.fs",
        [ "src/hb-open-type.hh", [ "HBFixed::to_float" ]
          "src/hb-algs.hh", [ "_hb_roundf"; "hb_min"; "hb_max"; "hb_clamp"; "hb_clamp_to" ] ]
      "Var/Fvar.fs",
        [ "src/hb-ot-var-fvar-table.hh",
            [ "OT::AxisRecord::get_axis_info"; "OT::AxisRecord::normalize_axis_value"; "OT::AxisRecord::get_coordinates"
              "OT::fvar::sanitize"; "OT::fvar::get_axis_count"; "OT::fvar::get_axis_infos"; "OT::fvar::find_axis_info"
              "OT::fvar::normalize_axis_value"; "OT::fvar::get_instance_count"; "OT::fvar::get_instance_subfamily_name_id"
              "OT::fvar::get_instance_postscript_name_id"; "OT::fvar::get_instance_coords" ]
          "src/hb-ot-var.cc",
            [ "hb_ot_var_has_data"; "hb_ot_var_get_axis_count"; "hb_ot_var_get_axis_infos"; "hb_ot_var_find_axis_info"
              "hb_ot_var_get_named_instance_count"; "hb_ot_var_named_instance_get_subfamily_name_id"
              "hb_ot_var_named_instance_get_postscript_name_id"; "hb_ot_var_named_instance_get_design_coords" ] ]
      "Var/Avar.fs",
        [ "src/hb-ot-var-avar-table.hh",
            [ "OT::SegmentMaps::map_float"; "OT::avar::sanitize"; "OT::avar::get_axis_count"; "OT::avar::map_coords_16_16" ] ]
      "Var/NormalizeCoords.fs",
        [ "src/hb-ot-var.cc", [ "hb_ot_var_normalize_coords"; "hb_ot_var_normalize_variations" ] ]
      "Var/Coords.fs",
        [ "src/hb-ot-layout-common.hh", [ "OT::VarRegionAxis::evaluate"; "OT::VarRegionList::evaluate" ] ]
      "Var/DeltaSetIndexMap.fs",
        [ "src/hb-ot-layout-common.hh",
            [ "OT::DeltaSetIndexMap::map"; "OT::DeltaSetIndexMap::get_map_count"; "OT::DeltaSetIndexMap::get_width"
              "OT::DeltaSetIndexMap::get_inner_bit_count"; "OT::DeltaSetIndexMap::sanitize" ] ]
      "Var/ItemVariationStore.fs",
        [ "src/hb-ot-layout-common.hh",
            [ "OT::VarData::_get_delta"; "OT::VarData::sanitize"; "OT::VarRegionList::sanitize"
              "OT::ItemVariationStore::get_delta"; "OT::ItemVariationStore::sanitize" ] ]
      "Var/FontVariations.fs",
        [ "src/hb-font.cc",
            [ "hb_font_set_variations"; "hb_font_set_variation"; "hb_font_set_var_coords_design"
              "hb_font_set_var_coords_normalized"; "hb_font_set_var_named_instance"; "hb_font_get_var_coords_normalized"
              "hb_font_get_var_coords_design"; "hb_font_get_var_named_instance"; "_hb_font_adopt_var_coords" ] ]
      "Var/Hvar.fs",
        [ "src/hb-ot-var-hvar-table.hh",
            [ "OT::HVARVVAR::sanitize"; "OT::HVARVVAR::has_data"; "OT::HVARVVAR::get_advance_delta_unscaled"
              "OT::VVAR::sanitize"; "OT::VVAR::get_vorg_delta_unscaled" ]
          "src/hb-ot-hmtx-table.hh",
            [ "OT::hmtx_accelerator_t::get_advance_with_var_unscaled"; "OT::vmtx_accelerator_t::get_advance_with_var_unscaled" ] ]
      "Var/Mvar.fs",
        [ "src/hb-ot-var-mvar-table.hh", [ "OT::MVAR::sanitize"; "OT::MVAR::get_var" ] ]
      "Glyph/GlyphPoints.fs",
        [ "src/OT/glyf/SimpleGlyph.hh",
            [ "OT::glyf_impl::SimpleGlyph::read_flags"; "OT::glyf_impl::SimpleGlyph::read_points"
              "OT::glyf_impl::SimpleGlyph::get_contour_points" ]
          "src/OT/glyf/CompositeGlyph.hh",
            [ "OT::glyf_impl::CompositeGlyphRecord::get_size"; "OT::glyf_impl::CompositeGlyphRecord::get_transformation" ]
          "src/OT/glyf/composite-iter.hh",
            [ "OT::glyf_impl::composite_iter_tmpl::set_current"; "OT::glyf_impl::composite_iter_tmpl::__next__" ]
          "src/OT/glyf/Glyph.hh", [ "OT::glyf_impl::Glyph::Glyph"; "OT::glyf_impl::Glyph::get_points" ]
          "src/hb-subset-plan.hh", [ "contour_point_t::transform" ] ]
      "Var/TupleVariation.fs",
        [ "src/hb-ot-var-common.hh",
            [ "OT::TupleVariationHeader::calculate_scalar"; "OT::TupleVariationData::decompile_points"
              "OT::TupleVariationData::decompile_deltas" ]
          "src/hb-open-type.hh", [ "OT::TupleValues::decompile" ] ]
      "Var/Gvar.fs",
        [ "src/hb-ot-var-gvar-table.hh",
            [ "OT::gvar_GVAR::sanitize_shallow"; "OT::gvar_GVAR::get_glyph_var_data_bytes"; "OT::gvar_GVAR::get_offset"
              "OT::gvar_GVAR::accelerator_t::infer_delta"; "OT::gvar_GVAR::accelerator_t::next_index"
              "OT::gvar_GVAR::accelerator_t::decompile_deltas_add_to_points"; "OT::gvar_GVAR::accelerator_t::apply_deltas_to_points" ] ]
      "Aat/AatLookup.fs",
        [ "src/hb-aat-layout-common.hh",
            [ "LookupFormat0::get_value"; "LookupFormat0::collect_glyphs"; "LookupFormat0::collect_glyphs_filtered"; "LookupFormat0::sanitize"
              "LookupSegmentSingle::cmp"; "LookupSegmentSingle::collect_glyphs"; "LookupSegmentSingle::collect_glyphs_filtered"; "LookupSegmentSingle::sanitize"
              "LookupFormat2::get_value"; "LookupFormat2::collect_glyphs"; "LookupFormat2::collect_glyphs_filtered"; "LookupFormat2::sanitize"
              "LookupSegmentArray::get_value"; "LookupSegmentArray::collect_glyphs"; "LookupSegmentArray::collect_glyphs_filtered"; "LookupSegmentArray::cmp"; "LookupSegmentArray::sanitize"
              "LookupFormat4::get_value"; "LookupFormat4::collect_glyphs"; "LookupFormat4::collect_glyphs_filtered"; "LookupFormat4::sanitize"
              "LookupSingle::cmp"; "LookupSingle::collect_glyphs"; "LookupSingle::collect_glyphs_filtered"; "LookupSingle::sanitize"
              "LookupFormat6::get_value"; "LookupFormat6::collect_glyphs"; "LookupFormat6::collect_glyphs_filtered"; "LookupFormat6::sanitize"
              "LookupFormat8::get_value"; "LookupFormat8::collect_glyphs"; "LookupFormat8::collect_glyphs_filtered"; "LookupFormat8::sanitize"
              "LookupFormat10::get_value_or_null"; "LookupFormat10::collect_glyphs"; "LookupFormat10::collect_glyphs_filtered"; "LookupFormat10::sanitize"
              "Lookup::get_value"; "Lookup::get_value_or_null"; "Lookup::collect_glyphs"; "Lookup::collect_glyphs_filtered"; "Lookup::get_class"; "Lookup::sanitize"
              "ClassTable::get_class"; "ClassTable::collect_glyphs"; "ClassTable::collect_glyphs_filtered"; "ClassTable::sanitize" ]
          "src/hb-open-type.hh", [ "VarSizedBinSearchArrayOf::last_is_terminator"; "VarSizedBinSearchArrayOf::get_length"; "VarSizedBinSearchArrayOf::bsearch"; "VarSizedBinSearchArrayOf::sanitize" ] ]
      "Aat/StateTable.fs",
        [ "src/hb-aat-layout-common.hh",
            [ "Entry::sanitize"; "StateTable::collect_initial_glyphs"; "StateTable::new_state"; "StateTable::get_class"; "StateTable::get_entry"; "StateTable::sanitize" ] ]
      "OpenType/Feat.fs",
        [ "src/hb-aat-layout-feat-table.hh",
            [ "FeatureName::has_data"; "FeatureName::is_exclusive"; "feat::has_data"; "feat::exposes_feature"; "feat::get_feature"; "feat::sanitize" ] ]
      "OpenType/Ltag.fs",
        [ "src/hb-aat-ltag-table.hh", [ "ltag::matches_language"; "ltag::sanitize" ] ]
      "OpenType/Morx.fs",
        [ "src/hb-aat-layout-morx-table.hh",
            [ "Feature::sanitize"; "Chain::compile_flags"; "mortmorx::sanitize"; "Chain::sanitize"
              "ChainSubtable::sanitize"; "NoncontextualSubtable::sanitize"; "SubtableGlyphCoverage::sanitize" ] ]
      "OpenType/Trak.fs",
        [ "src/hb-aat-layout-trak-table.hh",
            [ "TrackTableEntry::get_value"; "TrackTableEntry::interpolate_at"; "TrackTableEntry::get_track_value"
              "TrackData::get_tracking"; "trak::has_data"; "trak::sanitize" ] ]
      "OpenType/Stat.fs",
        [ "src/hb-ot-stat-table.hh", [ "STAT::has_data"; "STAT::sanitize"; "AxisValue::sanitize" ] ]
      "Aat/AatMap.fs",
        [ "src/hb-aat-map.cc", [ "hb_aat_map_builder_t::add_feature"; "hb_aat_map_builder_t::compile" ]
          "src/hb-aat-map.hh", [ "feature_info_t::cmp" ] ]
      "Aat/Driver.fs",
        [ "src/hb-aat-layout-common.hh", [ "StateTableDriver::drive" ] ]
      "Aat/MorxNoncontextual.fs",
        [ "src/hb-aat-layout-morx-table.hh", [ "NoncontextualSubtable::apply"; "NoncontextualSubtable::collect_initial_glyphs" ] ]
      "Aat/MorxRearrangement.fs",
        [ "src/hb-aat-layout-morx-table.hh", [ "RearrangementSubtable::transition"; "RearrangementSubtable::apply" ] ]
      "Aat/MorxContextual.fs",
        [ "src/hb-aat-layout-morx-table.hh", [ "ContextualSubtable::transition"; "ContextualSubtable::apply" ] ]
      "Aat/MorxApply.fs",
        [ "src/hb-aat-layout-morx-table.hh", [ "Chain::apply"; "mortmorx::apply" ] ]
      "OtFace.fs", [ "src/hb-ot-face.cc", [ "hb_ot_face_t::init0"; "hb_ot_face_t::fini" ] ]
      "Font.fs",
        [ "src/hb-font.hh",
           [ "hb_font_t::em_scale_x"; "hb_font_t::em_scale_y"; "hb_font_t::em_scalef_x"; "hb_font_t::em_scalef_y"
             "hb_font_t::em_fmultf"; "hb_font_t::em_fmult"; "hb_font_t::em_mult"; "hb_font_t::em_multf"
             "hb_font_t::changed"; "hb_font_t::get_h_extents_with_fallback"; "hb_font_t::get_font_h_extents"
             "hb_font_t::get_font_v_extents" ]
          "src/hb-font.cc", [ "_hb_font_create" ]
          "src/hb-ot-metrics.cc", [ "_fix_ascender_descender"; "_hb_ot_metrics_get_position_common" ] ]
      "Metrics.fs",
        [ "src/hb-ot-metrics.cc",
            [ "_fix_ascender_descender"; "_hb_ot_metrics_get_position_common"; "hb_ot_metrics_get_position"
              "hb_ot_metrics_get_position_with_fallback" ] ]
      "Aat/AatLayout.fs",
        [ "src/hb-aat-layout.cc",
            [ "hb_aat_layout_track"; "hb_aat_layout_has_substitution"; "hb_aat_layout_substitute"
              "hb_aat_layout_remove_deleted_glyphs" ]
          "src/hb-aat-layout-trak-table.hh", [ "trak::apply" ] ]
      "OpenType/CmapSubtable.fs",
        [ "src/hb-ot-cmap-table.hh",
           [ "OT::CmapSubtable::sanitize"; "OT::CmapSubtable::get_glyph"; "OT::CmapSubtable::get_glyph_variant"
             "OT::CmapSubtableFormat0::get_glyph"; "OT::CmapSubtableFormat4::get_glyph"; "OT::CmapSubtableFormat4::sanitize"
             "OT::CmapSubtableFormat14_15::sanitize"; "OT::CmapSubtableFormat14_15::get_glyph_variant" ] ]
      "OpenType/Cmap.fs",
        [ "src/hb-ot-cmap-table.hh",
           [ "OT::cmap::sanitize"; "OT::cmap::find_best_subtable"; "OT::cmap::find_subtable"
             "OT::cmap::find_variation_selector_subtable"; "OT::cmap::accelerator_t::get_nominal_glyph"
             "OT::cmap::accelerator_t::get_variation_glyph"; "OT::cmap::accelerator_t::collect_unicodes"
             "OT::cmap::accelerator_t::collect_mapping"; "OT::cmap::accelerator_t::collect_variation_selectors"
             "OT::cmap::accelerator_t::collect_variation_unicodes" ] ]
      "OpenType/Post.fs",
        [ "src/hb-ot-post-table.hh",
           [ "OT::post::sanitize"; "OT::post::accelerator_t::get_glyph_name"; "OT::post::accelerator_t::get_glyph_from_name"
             "OT::post::accelerator_t::find_glyph_name"; "OT::post::accelerator_t::get_glyph_count" ] ]
      "OpenType/CffIndex.fs",
        [ "src/hb-open-type.hh", [ "OT::CFFIndex::sanitize"; "OT::CFFIndex::operator [ ]"; "OT::CFFIndex::get_size" ] ]
      "OpenType/CffDict.fs",
        [ "src/hb-cff-interp-common.hh",
           [ "CFF::dict_interpreter_t::interpret"; "CFF::opset_t::process_op"; "CFF::interp_env_t::fetch_op" ]
          "src/hb-cff-interp-dict-common.hh", [ "CFF::dict_opset_t::parse_bcd"; "CFF::dict_opset_t::process_op" ] ]
      "OpenType/Cff1.fs",
        [ "src/hb-ot-cff1-table.hh",
           [ "OT::cff1::accelerator_templ_t::accelerator_templ_t"; "OT::cff1::accelerator_templ_t::glyph_to_sid"
             "OT::cff1::accelerator_templ_t::sid_to_glyph"; "OT::cff1::accelerator_t::get_glyph_name"
             "OT::cff1::accelerator_t::get_glyph_from_name"; "CFF::Charset::sanitize"; "CFF::Charset::get_sid"
             "CFF::Charset::get_glyph"; "CFF::Encoding::sanitize" ]
          "src/hb-ot-cff1-table.cc",
           [ "OT::cff1::lookup_standard_encoding_for_sid"; "OT::cff1::lookup_expert_charset_for_sid"
             "OT::cff1::lookup_expert_subset_charset_for_sid"; "OT::cff1::lookup_expert_charset_for_glyph"
             "OT::cff1::lookup_expert_subset_charset_for_glyph" ] ]
      "OpenType/CffCharString.fs",
        [ "src/hb-cff-interp-cs-common.hh",
           [ "CFF::cs_interp_env_t::call_subr"; "CFF::cs_interp_env_t::return_from_subr"
             "CFF::cs_interp_env_t::determine_hintmask_size"; "CFF::cs_opset_t::process_op"
             "CFF::path_procs_t::hflex"; "CFF::path_procs_t::flex"; "CFF::path_procs_t::hflex1"
             "CFF::path_procs_t::flex1"; "CFF::cs_interpreter_t::interpret" ]
          "src/hb-cff1-interp-cs.hh",
           [ "CFF::cff1_cs_opset_t::process_op"; "CFF::cff1_cs_opset_t::check_width" ] ]
      "Glyph/Cff1Extents.fs",
        [ "src/hb-ot-cff1-table.cc",
           [ "bounds_t::update"; "bounds_t::merge"; "bounds_t::offset"; "cff1_path_procs_extents_t::line"
             "cff1_path_procs_extents_t::curve"; "cff1_cs_opset_extents_t::process_seac"; "_get_bounds"
             "OT::cff1::accelerator_t::get_extents" ] ]
      "OpenType/Vorg.fs",
        [ "src/hb-ot-vorg-table.hh",
            [ "OT::VORG::has_data"; "OT::VORG::get_y_origin"; "OT::VORG::sanitize"
              "OT::VORGFormat1_2::get_y_origin"; "OT::VORGFormat1_2::sanitize" ] ]
      "OpenType/GlyfHeader.fs",
        [ "src/OT/glyf/glyf.hh",
            [ "OT::glyf_accelerator_t::has_data"; "OT::glyf_accelerator_t::get_extents"
              "OT::glyf_accelerator_t::get_extents_at"; "OT::glyf_accelerator_t::glyph_for_gid" ]
          "src/OT/glyf/GlyphHeader.hh",
            [ "OT::glyf_impl::GlyphHeader::has_data"; "OT::glyf_impl::GlyphHeader::get_extents_without_var_scaled" ] ]
      "OpenType/Sbix.fs",
        [ "src/OT/Color/sbix/sbix.hh",
            [ "OT::SBIXStrike::sanitize"; "OT::SBIXStrike::get_glyph_blob"; "OT::sbix::has_data"
              "OT::sbix::sanitize"; "OT::sbix::accelerator_t::choose_strike"; "OT::sbix::accelerator_t::get_png_extents" ] ]
      "OpenType/Cbdt.fs",
        [ "src/OT/Color/CBDT/CBDT.hh",
            [ "OT::SmallGlyphMetrics::get_extents"; "OT::IndexSubtable::sanitize"; "OT::IndexSubtableRecord::sanitize"
              "OT::IndexSubtableArray::find_table"; "OT::BitmapSizeTable::sanitize"; "OT::CBLC::sanitize"
              "OT::CBDT::sanitize"; "OT::CBDT::accelerator_t::choose_strike"; "OT::CBDT::accelerator_t::get_extents" ] ]
      "OpenType/Layout/Coverage.fs",
        [ "src/OT/Layout/Common/Coverage.hh", []
          "src/OT/Layout/Common/CoverageFormat1.hh", []
          "src/OT/Layout/Common/CoverageFormat2.hh", []
          "src/OT/Layout/Common/RangeRecord.hh", [] ]
      "OpenType/Layout/ClassDef.fs",
        [ "src/hb-ot-layout-common.hh",
          [ "ClassDefFormat1_3::get_class"; "ClassDefFormat1_3::sanitize"; "ClassDefFormat1_3::collect_class"
            "ClassDefFormat2_4::get_class"; "ClassDefFormat2_4::sanitize"; "ClassDefFormat2_4::collect_class"
            "ClassDef::get_class"; "ClassDef::sanitize"; "ClassDef::collect_class" ] ]
      "OpenType/Layout/LayoutAccel.fs",
        [ "src/hb-ot-layout-gsubgpos.hh",
          [ "hb_ot_layout_lookup_accelerator_t::create"; "hb_accelerate_subtables_context_t::dispatch" ] ]
      "OpenType/Layout/Gdef.fs",
        [ "src/OT/Layout/GDEF/GDEF.hh",
            [ "GDEF::sanitize"; "GDEF::get_glyph_props"; "GDEF::get_glyph_class"; "GDEF::get_glyphs_in_class"
              "GDEF::get_attach_points"; "GDEF::get_lig_carets"; "GDEF::mark_set_covers"; "GDEF::is_blocklisted" ] ]
      "OpenType/Layout/LayoutTable.fs",
        [ "src/hb-ot-layout-gsubgpos.hh",
            [ "GSUBGPOS::sanitize"; "GSUBGPOS::get_script_tags"; "GSUBGPOS::find_script_index"
              "GSUBGPOS::get_feature_tags"; "GSUBGPOS::find_feature_index"; "GSUBGPOS::get_lookup_count" ]
          "src/hb-ot-layout-common.hh",
            [ "Feature::sanitize"; "LangSys::sanitize"; "Script::sanitize"; "Script::find_lang_sys_index"
              "Lookup::sanitize"; "Lookup::get_props" ]
          "src/hb-ot-layout.cc",
            [ "hb_ot_layout_table_get_script_tags"; "hb_ot_layout_table_select_script"; "hb_ot_layout_script_get_language_tags"
              "hb_ot_layout_script_select_language2"; "hb_ot_layout_language_get_required_feature"; "hb_ot_layout_language_get_feature_indexes"
              "hb_ot_layout_language_get_feature_tags"; "hb_ot_layout_table_get_feature_tags"; "hb_ot_layout_table_find_feature"
              "hb_ot_layout_feature_get_lookups"; "hb_ot_layout_table_get_lookup_count"; "hb_ot_layout_collect_features_map" ] ]
      "OpenType/Layout/GsubTables.fs", [ "src/OT/Layout/GSUB/SubstLookupSubTable.hh", [ "SubstLookupSubTable::dispatch" ] ]
      "OpenType/Layout/GposTables.fs", [ "src/OT/Layout/GPOS/PosLookupSubTable.hh", [ "PosLookupSubTable::dispatch" ] ]
      "OpenType/Layout/Device.fs",
        [ "src/hb-ot-layout-common.hh",
            [ "HintingDevice::get_delta"; "HintingDevice::get_delta_pixels"; "Device::get_x_delta"; "Device::get_y_delta" ] ]
      "OpenType/Layout/KernTable.fs",
        [ "src/hb-ot-kern-table.hh",
            [ "KernOTSubTableHeader::is_horizontal"; "KernAATSubTableHeader::is_horizontal"; "KernSubTable::get_kerning"
              "KernSubTable::sanitize"; "KernSubTableFormat3::get_kerning"; "kern::has_state_machine"; "kern::has_cross_stream" ]
          "src/hb-aat-layout-kerx-table.hh",
            [ "KerxSubTableFormat0::get_kerning"; "KerxSubTableFormat2::get_kerning"; "KernPair::cmp" ] ]
      "Var/VariationDevice.fs",
        [ "src/hb-ot-layout-common.hh",
            [ "OT::VariationDevice::get_x_delta"; "OT::VariationDevice::get_y_delta"
              "OT::VariationDevice::get_x_delta_f"; "OT::VariationDevice::get_y_delta_f" ] ]
      "Layout/LayoutApi.fs",
        [ "src/hb-ot-layout.cc",
            [ "hb_ot_layout_has_glyph_classes"; "hb_ot_layout_get_glyph_class"; "hb_ot_layout_get_glyphs_in_class"
              "hb_ot_layout_get_attach_points"; "hb_ot_layout_get_ligature_carets" ] ]
      "Layout/MapBuilder.fs",
        [ "src/hb-ot-map.cc",
            [ "hb_ot_map_builder_t::hb_ot_map_builder_t"; "hb_ot_map_builder_t::add_feature"; "hb_ot_map_builder_t::add_lookups"
              "hb_ot_map_builder_t::compile"; "hb_ot_map_t::get_feature_tags" ]
          "src/hb-ot-shape.cc", [ "hb_ot_shape_collect_features" ] ]
      "Layout/Matching.fs",
        [ "src/hb-ot-layout-gsubgpos.hh",
            [ "matcher_t::init"; "matcher_t::may_skip"; "matcher_t::may_match"; "skipping_iterator_t::next"
              "skipping_iterator_t::prev"; "hb_ot_apply_context_t::check_glyph_property"; "hb_ot_apply_context_t::_set_glyph_class"
              "hb_ot_apply_context_t::random_number" ]
          "src/hb-ot-layout.hh", [ "_hb_allocate_lig_id" ]
          "src/hb-buffer.hh", [ "hb_buffer_t::next_serial" ] ]
      "Layout/Engine.fs",
        [ "src/hb-ot-layout.cc",
          [ "hb_ot_map_t::apply"; "apply_string"; "apply_forward"; "apply_backward" ] ]
      "Layout/GposApply.fs",
        [ "src/OT/Layout/GPOS/GPOS.hh",
          [ "GPOS::position_start"; "GPOS::position_finish_offsets"; "propagate_attachment_offsets" ] ]
      "Layout/Kern.fs",
        [ "src/hb-kern.hh", [ "hb_kern_machine_t::kern" ]
          "src/hb-aat-layout-kerx-table.hh", [ "KerxTable::apply" ]
          "src/hb-ot-layout.cc", [ "hb_ot_layout_kern" ] ]
      "Shape/Categorize.fs", [ "src/hb-ot-shaper.hh", [ "hb_ot_shaper_categorize" ] ]
      "Glyph/Extents.fs",
        [ "src/hb-font.hh",
            [ "hb_font_t::scale_glyph_extents"; "hb_font_t::synthetic_glyph_extents"; "hb_font_t::get_glyph_extents" ] ]
      "Glyph/Origins.fs",
        [ "src/hb-font.hh", [ "hb_font_t::apply_glyph_v_origins_with_fallback" ] ]
      "Shape/OtLayout.fs",
        [ "src/hb-ot-map.hh", [ "hb_ot_map_t::get_mask"; "hb_ot_map_t::get_1_mask"; "hb_ot_map_t::get_feature_index" ] ]
      "Shape/OtPlan.fs",
        [ "src/hb-ot-shape.cc",
           [ "_hb_apply_morx"; "hb_ot_shape_planner_t::hb_ot_shape_planner_t"; "hb_ot_shape_planner_t::compile" ] ]
      "Shape/OtProps.fs",
        [ "src/hb-ot-shape.cc", [ "hb_set_unicode_props"; "hb_synthesize_glyph_classes" ] ]
      "Shape/Normalize.fs",
        [ "src/hb-ot-shape-normalize.cc",
            [ "_hb_ot_shape_normalize"; "decompose"; "decompose_current_character"; "handle_variation_selector_cluster"
              "decompose_multi_char_cluster"; "compare_combining_class" ] ]
      "Shape/OtSubstitute.fs",
        [ "src/hb-ot-shape.cc",
            [ "hb_ot_substitute_default"; "hb_ot_substitute_plan"; "hb_ot_substitute_pre"; "hb_ot_substitute_post" ]
          "src/hb-ot-layout.cc", [ "_hb_ot_layout_set_glyph_props" ] ]
      "Shape/OtPosition.fs",
        [ "src/hb-ot-shape.cc", [ "hb_ot_position_default"; "hb_ot_position_plan"; "hb_ot_position" ] ]
      "Shape/OtShaper.fs",
        [ "src/hb-ot-shape.cc", [ "hb_ot_shape_internal"; "hb_propagate_flags" ] ]
      "Shape/FallbackShaper.fs",
        [ "src/hb-fallback-shape.cc", [ "_hb_fallback_shape" ] ]
      "Shape/Execute.fs",
        [ "src/hb-shape-plan.cc", [ "hb_shape_plan_create_cached2"; "hb_shape_plan_execute" ] ]
      "Shape/Verify.fs",
        [ "src/hb-buffer-verify.cc",
            [ "buffer_verify_monotone"; "buffer_verify_unsafe_to_break"; "buffer_verify_unsafe_to_concat"; "hb_buffer_t::verify" ]
          "src/hb-buffer.cc", [ "hb_buffer_diff" ] ]
      "Shape/Shape.fs",
        [ "src/hb-shape.cc", [ "hb_shape_full"; "hb_shape"; "hb_shape_list_shapers" ] ] ]

/// <summary>Splits one CSV line, honouring double quotes.</summary>
/// <param name="line">The line.</param>
/// <returns>The fields.</returns>
let csvFields (line: string) =
    let folder (fields: string list, current: string, quoted: bool) (c: char) =
        match c, quoted with
        | '"', _ -> (fields, current, not quoted)
        | ',', false -> (current :: fields, String.Empty, false)
        | _ -> (fields, current + string c, quoted)
    let fields, last, _ = line |> Seq.fold folder ([], String.Empty, false)
    List.rev (last :: fields)

/// <summary>Runs lizard on files and returns (function name, CCN, NLOC) for each function.</summary>
/// <param name="language">The lizard language, cpp or fsharp.</param>
/// <param name="files">The files.</param>
/// <returns>The functions.</returns>
let lizard (language: string) (files: string list) =
    let code, lines = execute (Path.Combine(root, "artifacts", "venv", "bin", "lizard")) ([ "-l"; language; "--csv" ] @ files) root true
    match code with
    | 0 ->
        lines
        |> List.filter (fun line -> line.Length > 0 && Char.IsDigit line[0])
        |> List.map csvFields
        |> List.choose (fun fields -> match fields with nloc :: ccn :: _ :: _ :: _ :: _ :: _ :: name :: _ -> Some (name, int ccn, int nloc) | _ -> None)
    | _ -> []

/// <summary>Builds the complexity table: each Farfbuzz module against the upstream functions it ports.</summary>
/// <returns>Zero once the table is written into PROGRESS.md.</returns>
let metrics () =
    let venvLizard = Path.Combine(root, "artifacts", "venv", "bin", "lizard")
    if not (File.Exists venvLizard) then
        execute "python3" [ "-m"; "venv"; Path.Combine(root, "artifacts", "venv") ] root false |> ignore
        execute (Path.Combine(root, "artifacts", "venv", "bin", "pip")) [ "install"; "-q"; "lizard==1.25.1" ] root false |> ignore
    let row (modules: string, upstream: (string * string list) list) =
        let ours = modules.Split(" + ") |> Array.toList |> List.map (fun file -> Path.Combine("src", "Farfbuzz", file)) |> lizard "fsharp"
        let theirs =
            upstream |> List.collect (fun (file, names) ->
                lizard "cpp" [ Path.Combine("upstream", "harfbuzz", file) ] |> List.filter (fun (name, _, _) -> names.IsEmpty || List.contains name names))
        let total (functions: (string * int * int) list) = functions |> List.sumBy (fun (_, ccn, _) -> ccn), functions |> List.sumBy (fun (_, _, nloc) -> nloc), functions.Length
        let ccnOurs, nlocOurs, countOurs = total ours
        let ccnTheirs, nlocTheirs, countTheirs = total theirs
        let ratio = if ccnTheirs = 0 then "—" else (float ccnOurs / float ccnTheirs).ToString("F2")
        let files = String.Join(", ", upstream |> List.map (fun (file, names) -> if names.IsEmpty then file else $"{file} ({names.Length} functions)"))
        $"| {modules} | {ccnOurs} | {nlocOurs} | {countOurs} | {files} | {ccnTheirs} | {nlocTheirs} | {countTheirs} | {ratio} |"
    let table =
        [ "| F# module | CCN | NLOC | Functions | Upstream | CCN | NLOC | Functions | CCN ratio |"
          "|---|---|---|---|---|---|---|---|---|" ] @ (metricsMap |> List.map row)
    let progress = Path.Combine(root, "PROGRESS.md")
    let text = File.ReadAllText progress
    let start = text.IndexOf "<!-- metrics:start -->"
    let stop = text.IndexOf "<!-- metrics:end -->"
    match start >= 0 && stop > start with
    | false ->
        Console.Error.WriteLine "PROGRESS.md has no metrics markers"
        1
    | true ->
        let block = "<!-- metrics:start -->\n" + String.Join("\n", table) + "\n"
        File.WriteAllText(progress, text.Substring(0, start) + block + text.Substring stop)
        table |> List.iter Console.WriteLine
        0

/// <summary>The commit the upstream submodule is pinned to (UPSTREAM.md).</summary>
let pinnedUpstream = "3df40d9b98d6b32f0e4288d4ecfbb158ddb3a372"

/// <summary>Runs git and returns its output lines, or an empty list when it fails.</summary>
/// <param name="arguments">The git arguments.</param>
/// <returns>The output lines.</returns>
let git (arguments: string list) =
    match execute "git" arguments root true with
    | 0, lines -> lines |> List.filter (fun line -> line.Length > 0)
    | _ -> []

/// <summary>Paths no slice may change after its phase starts.</summary>
let protectedPaths = [ "tests/Farfbuzz.Harness"; "tests/Farfbuzz.Diff/RunTestsEquality.fs" ]

/// <summary>Files that may only grow: existing lines are never changed or removed.</summary>
let appendOnly = [ "tools/oracle/oracle.cc"; "tests/Farfbuzz.Oracle/Native.fs"; "tests/Farfbuzz.Oracle/Shims.fs" ]

/// <summary>The files the conformance and random-text ratchets read.</summary>
let baselines = [ "tests/Farfbuzz.Tests/conformance.baseline"; "tests/Farfbuzz.Diff/random-text.baseline" ]

/// <summary>Test files allowed to skip: the ratchets themselves.</summary>
let ratchetFiles = [ "tests/Farfbuzz.Tests/ConformanceTests.fs"; "tests/Farfbuzz.Diff/RandomTextDiff.fs"; "tests/Farfbuzz.Diff/RunTestsEquality.fs"; "tests/Farfbuzz.Tests/HarnessTests.fs" ]

/// <summary>Constructs banned in src/Farfbuzz outside Sealed.fs and Generated/, with the rule each breaks.</summary>
let bannedConstructs =
    [ @"^\s*for\b.*\bdo\b|\bfor\b.*\bin\b.*\bdo\s*$", "no for loops: use self tail recursion or folds"
      @"\bwhile\b", "no while loops"
      @"\blet\s+mutable\b", "no let mutable"
      @"<-", "no assignment outside Sealed.fs"
      @"\belse\s+if\b|\belif\b", "never chain or nest if/else: pattern matching"
      @"\bsprintf\b|\bprintfn?\b|\bfailwithf\b|%A", "no printf-family formatting"
      @"\w\.\[", "F# 10 indexing: xs[i], never xs.[i]"
      @"let\s+(inline\s+)?\(\|[A-Z]\w*\|[A-Z]\w*", "no multi-case total active patterns in src/"
      @"\bArray\.(blit|fill|set)\b|\.CopyTo\(", "arrays are written only in Sealed.fs"
      @"^ {8,}let rec\b", "local let rec: use a module-level [<TailCall>] function" ]

/// <summary>The latest phase-start tag, if any.</summary>
/// <returns>The tag name.</returns>
let phaseStart () =
    git [ "tag"; "--list"; "phase-*-start"; "--sort=-creatordate" ] |> List.tryHead

/// <summary>Lines added and removed in a file since a commit, working tree included.</summary>
/// <param name="since">The commit.</param>
/// <param name="path">The file.</param>
/// <returns>The added and the removed lines.</returns>
let changes (since: string) (path: string) =
    let diff = git [ "diff"; "--unified=0"; since; "--"; path ]
    let added = diff |> List.filter (fun line -> line.StartsWith "+" && not (line.StartsWith "+++")) |> List.map (fun line -> line.Substring 1)
    let removed = diff |> List.filter (fun line -> line.StartsWith "-" && not (line.StartsWith "---")) |> List.map (fun line -> line.Substring 1)
    added, removed

/// <summary>Finds functions and members in src/Farfbuzz without an XML documentation comment above them.</summary>
/// <param name="path">The source file.</param>
/// <returns>The offending lines.</returns>
let undocumented (path: string) =
    let lines = File.ReadAllLines path
    let definition = Regex(@"^(    )?(let|and|member|static member|override|abstract|default)\b")
    let rec documented (index: int) =
        match index with
        | -1 -> false
        | _ when lines[index].TrimStart().StartsWith "[<" -> documented (index - 1)
        | _ -> lines[index].TrimStart().StartsWith "///"
    lines
    |> Array.indexed
    |> Array.filter (fun (index, line) -> definition.IsMatch line && not (documented (index - 1)))
    |> Array.map (fun (index, line) -> $"{path}:{index + 1}: missing /// documentation: {line.Trim()}")
    |> Array.toList

/// <summary>Reports a match expression inside the arms of three enclosing match expressions.</summary>
/// <returns>The offending lines.</returns>
let nestingProblems () =
    let sourceRoot = Path.Combine(root, "src", "Farfbuzz")
    Directory.EnumerateFiles(sourceRoot, "*.fs", SearchOption.AllDirectories)
    |> Seq.collect (fun path ->
        let lines = File.ReadAllLines path
        let rec scan index (stack: int list) (problems: string list) =
            match index >= lines.Length with
            | true -> problems
            | false ->
                let line = lines[index]
                let trimmed = line.TrimStart()
                let indent = line.Length - trimmed.Length
                match trimmed.Length = 0 || trimmed.StartsWith "//" with
                | true -> scan (index + 1) stack problems
                | false ->
                    let isArm = trimmed.StartsWith "|"
                    let isMatch = Regex.IsMatch(trimmed, @"^match\b")
                    let stack =
                        match isArm with
                        | true ->
                            let rec pop value =
                                match value with
                                | head :: rest when head > indent -> pop rest
                                | _ -> value
                            pop stack
                        | false ->
                            let rec pop value =
                                match value with
                                | head :: rest when head >= indent -> pop rest
                                | _ -> value
                            pop stack
                    match isMatch with
                    | true ->
                        let problems =
                            match stack.Length >= 3 with
                            | true -> $"{Path.GetRelativePath(root, path)}:{index + 1}: match is nested inside three match arms" :: problems
                            | false -> problems
                        scan (index + 1) (indent :: stack) problems
                    | false -> scan (index + 1) stack problems
        scan 0 [] [])
    |> Seq.toList

/// <summary>The file recording, per source file, how many functions still annotate their result.</summary>
let annotationBaseline = Path.Combine(root, "tools", "annotations.baseline")

/// <summary>Counts, per source file, the functions that annotate their result; types are inferred (AGENTS.md).</summary>
/// <returns>The repository-relative path and count of every file that has any.</returns>
let annotationCounts () =
    let annotated = Regex(@"^\s*((let|and|member|static member)\b[^=]*\)|)\s*:\s*[^=]+=\s*$")
    Directory.EnumerateFiles(Path.Combine(root, "src", "Farfbuzz"), "*.fs", SearchOption.AllDirectories)
    |> Seq.filter (fun path -> not (path.Contains $"{Path.DirectorySeparatorChar}Generated{Path.DirectorySeparatorChar}"))
    |> Seq.map (fun path ->
        Path.GetRelativePath(root, path).Replace('\\', '/'),
        File.ReadAllLines path |> Array.filter (fun line -> not (line.TrimStart().StartsWith "//") && annotated.IsMatch line) |> Array.length)
    |> Seq.filter (fun (_, count) -> count > 0)
    |> Seq.sortBy fst
    |> Seq.toList

/// <summary>Reads the allowed result-annotation count of every file in the baseline.</summary>
/// <returns>The counts by repository-relative path; a file that is not listed is allowed none.</returns>
let annotationAllowance () =
    match File.Exists annotationBaseline with
    | false -> Map.empty
    | true ->
        File.ReadAllLines annotationBaseline
        |> Array.filter (fun line -> line.Length > 0 && not (line.StartsWith "#"))
        |> Array.map (fun line -> let parts = line.Split '\t' in parts[1], int parts[0])
        |> Map.ofArray

/// <summary>Reports every source file with more result annotations than the baseline allows.</summary>
/// <returns>The offending files.</returns>
let annotationProblems () =
    let allowed = annotationAllowance ()
    annotationCounts ()
    |> List.filter (fun (path, count) -> count > (allowed |> Map.tryFind path |> Option.defaultValue 0))
    |> List.map (fun (path, count) ->
        let limit = allowed |> Map.tryFind path |> Option.defaultValue 0
        $"{path}: {count} functions annotate their result, the baseline allows {limit}: types are inferred, remove `: T` before `=` (AGENTS.md)")

/// <summary>Rewrites the annotation baseline; a count can only go down, and a new file is allowed none.</summary>
/// <returns>Zero.</returns>
let lowerAnnotationBaseline () =
    let allowed = annotationAllowance ()
    let first = not (File.Exists annotationBaseline)
    let rows =
        annotationCounts ()
        |> List.map (fun (path, count) -> path, (match first with | true -> count | false -> min count (allowed |> Map.tryFind path |> Option.defaultValue 0)))
        |> List.filter (fun (_, count) -> count > 0)
    let header =
        [ "# Functions that still annotate their result, per source file: count, tab, path."
          "# Types are inferred (AGENTS.md). A count may only go down and a file that is not listed is allowed none."
          "# Only `dotnet fsi build.fsx annotations` rewrites this file, after annotations were removed." ]
    File.WriteAllLines(annotationBaseline, header @ (rows |> List.map (fun (path, count) -> $"{count}\t{path}")))
    Console.WriteLine $"    {rows |> List.sumBy snd} result annotations allowed in {rows.Length} files"
    0

/// <summary>Checks the rules a coding agent could break without a test noticing; see AGENTS.md.</summary>
/// <returns>Zero when every rule holds.</returns>
let guard () =
    let submodule =
        let head = git [ "-C"; "upstream/harfbuzz"; "rev-parse"; "HEAD" ]
        let dirty = git [ "-C"; "upstream/harfbuzz"; "status"; "--porcelain" ]
        [ if head <> [ pinnedUpstream ] then yield $"upstream/harfbuzz is not at the pinned commit {pinnedUpstream}"
          if not dirty.IsEmpty then yield "upstream/harfbuzz has local changes" ]
    let sincePhase =
        match phaseStart () with
        | None -> []
        | Some tag ->
            let touched =
                protectedPaths
                |> List.collect (fun path -> git [ "diff"; "--name-only"; tag; "--"; path ])
                |> List.filter (fun path -> not (path.EndsWith "FarfbuzzShaper.fs"))
                |> List.map (fun path -> $"{path} is protected but changed since {tag}")
            let rewritten =
                appendOnly |> List.collect (fun path -> match changes tag path with _, [] -> [] | _, _ -> [ $"{path} is append-only but lines were changed since {tag}" ])
            touched @ rewritten
    let divergencesChanged = not (git [ "diff"; "--name-only"; "HEAD"; "--"; "DIVERGENCES.md" ]).IsEmpty
    let baselineLosses =
        baselines |> List.collect (fun path ->
            match changes "HEAD" path with
            | _, removed when removed |> List.exists (fun line -> line.Length > 0 && not (line.StartsWith "#")) && not divergencesChanged ->
                [ $"{path} lost entries without a DIVERGENCES.md change" ]
            | _ -> [])
    let weakened =
        git [ "diff"; "--name-only"; "HEAD"; "--"; "tests" ]
        |> List.filter (fun path -> path.EndsWith ".fs" && not (List.contains path ratchetFiles))
        |> List.collect (fun path ->
            fst (changes "HEAD" path)
            |> List.filter (fun line -> Regex.IsMatch(line, @"\bptest\b|\bftest\b|\bskiptest\b|floatClose|Accuracy\.")))
        |> List.map (fun line -> $"a test was weakened: {line.Trim()}")
    let sources =
        Directory.EnumerateFiles(Path.Combine(root, "src", "Farfbuzz"), "*.fs", SearchOption.AllDirectories)
        |> Seq.filter (fun path -> not (path.EndsWith "Sealed.fs") && not (path.Contains $"{Path.DirectorySeparatorChar}Generated{Path.DirectorySeparatorChar}"))
        |> Seq.toList
    let banned =
        sources |> List.collect (fun path ->
            File.ReadAllLines path
            |> Array.indexed
            |> Array.filter (fun (_, line) -> not (line.TrimStart().StartsWith "//"))
            |> Array.map (fun (index, line) -> index, (match line.IndexOf "  //" with -1 -> line | at -> line.Substring(0, at)))
            |> Array.collect (fun (index, line) ->
                bannedConstructs
                |> List.filter (fun (pattern, _) -> Regex.IsMatch(line, pattern))
                |> List.map (fun (_, rule) -> $"{Path.GetRelativePath(root, path)}:{index + 1}: {rule}")
                |> List.toArray)
            |> Array.toList)
    let hugeLiterals =
        sources |> List.collect (fun path ->
            File.ReadAllLines path
            |> Array.indexed
            |> Array.filter (fun (_, line) -> Regex.Matches(line, @"\b\d+(u|uy|us|L|UL)?\b").Count > 32)
            |> Array.map (fun (index, _) -> $"{Path.GetRelativePath(root, path)}:{index + 1}: data table outside src/Farfbuzz/Generated (generate it, never hand-port)")
            |> Array.toList)
    let docs = sources |> List.collect (fun path -> undocumented path |> List.map (fun line -> line.Replace(root + string Path.DirectorySeparatorChar, String.Empty)))
    let nesting = nestingProblems ()
    let localRecursion =
        Directory.EnumerateFiles(Path.Combine(root, "src", "Farfbuzz"), "*.fs", SearchOption.AllDirectories)
        |> Seq.filter (fun path -> not (path.Contains $"{Path.DirectorySeparatorChar}Generated{Path.DirectorySeparatorChar}"))
        |> Seq.collect (fun path ->
            File.ReadAllLines path
            |> Array.indexed
            |> Array.filter (fun (_, line) -> Regex.IsMatch(line, @"^ {8,}let rec\b"))
            |> Array.map (fun (index, _) -> $"{Path.GetRelativePath(root, path)}:{index + 1}: local let rec: use a module-level [<TailCall>] function"))
        |> Seq.toList
    let publicApiPinned =
        let baselineChanged = not (git [ "diff"; "--name-only"; "HEAD"; "--"; "tests/Farfbuzz.PublicApi/public-api.baseline" ]).IsEmpty
        let planChanged = not (git [ "diff"; "--name-only"; "HEAD"; "--"; "PLAN.md" ]).IsEmpty
        match baselineChanged, planChanged with
        | true, false -> [ "tests/Farfbuzz.PublicApi/public-api.baseline changed without a PLAN.md change: the owner's decision recorded there is the only way to change the pinned API" ]
        | _ -> []
    let problems = submodule @ sincePhase @ baselineLosses @ weakened @ banned @ hugeLiterals @ docs @ nesting @ localRecursion @ annotationProblems () @ publicApiPinned
    problems |> List.iter (fun problem -> Console.Error.WriteLine("    " + problem))
    match problems with
    | [] ->
        Console.WriteLine $"""    guard holds{(match phaseStart () with Some tag -> $" (protected paths checked since {tag})" | None -> " (no phase-*-start tag yet: protected paths not checked)")}"""
        0
    | _ -> 1

/// <summary>Installs the pre-commit hook that runs the guard.</summary>
/// <returns>Zero.</returns>
let installHooks () =
    let target = Path.Combine(root, ".git", "hooks", "pre-commit")
    File.Copy(Path.Combine(root, "tools", "hooks", "pre-commit"), target, true)
    File.SetUnixFileMode(target, UnixFileMode.UserRead ||| UnixFileMode.UserWrite ||| UnixFileMode.UserExecute ||| UnixFileMode.GroupRead ||| UnixFileMode.OtherRead)
    Console.WriteLine $"    installed {target}"
    0

/// <summary>The value of an environment variable, or a default when it is unset or empty.</summary>
/// <param name="name">The variable.</param>
/// <param name="fallback">The default.</param>
/// <returns>The value.</returns>
let environmentOr name fallback =
    match Environment.GetEnvironmentVariable name with
    | null -> fallback
    | value when value.Length = 0 -> fallback
    | value -> value

/// <summary>The BenchmarkDotNet step of the performance report: shape, font and tag benchmarks with the full JSON. The job is
/// BenchmarkDotNet's default one unless <c>FARFBUZZ_PERF_JOB</c> names another (for example <c>short</c> on a loaded machine).</summary>
let benchmarkStep =
    let job =
        match environmentOr "FARFBUZZ_PERF_JOB" String.Empty with
        | name when name.Length = 0 -> []
        | name -> [ "--job"; name ]
    dotnet "shape, font and tag benchmarks, full JSON"
        ([ "run"; "-c"; "Release"; "--project"; "bench/Farfbuzz.Bench"; "--"; "--filter"; "*UpstreamShape*"; "*UpstreamFont*"; "*UpstreamOt*"; "--memory"; "--exporters"; "fulljson"; "--artifacts"; "artifacts/bench" ] @ job)

/// <summary>The step that writes <c>docs/research/perf-report.md</c> from the benchmark and runner JSON.</summary>
/// <param name="note">The text of the environment variable <c>FARFBUZZ_PERF_NOTE</c>, placed under the report's environment header.</param>
/// <returns>The step.</returns>
let reportStep (note: string) =
    let noted = if note.Length = 0 then [] else [ "--note"; note ]
    dotnet "performance report (no --enforce)" ([ "fsi"; "tools/perf/PerfReport.fsx" ] @ noted @ [ "artifacts/bench/results"; "artifacts/runner" ])

/// <summary>Publish arguments pointing the runner's embedded fonts and texts at another upstream checkout, from
/// <c>FARFBUZZ_UPSTREAM</c>; none by default, which uses this checkout's <c>upstream/harfbuzz</c>.</summary>
let benchUpstream =
    match environmentOr "FARFBUZZ_UPSTREAM" String.Empty with
    | path when path.Length = 0 -> []
    | path -> [ $"-p:BenchUpstream={path.TrimEnd('/')}/" ]

/// <summary>The fewest passes of every runner measurement: <c>FARFBUZZ_MATRIX_PASSES</c>, ten by default.</summary>
let matrixPasses = environmentOr "FARFBUZZ_MATRIX_PASSES" "10"

/// <summary>The fewest passes of the WebAssembly interpreter's measurements, which run some thirty times slower than CoreCLR:
/// <c>FARFBUZZ_MATRIX_PASSES_INTERP</c>, <c>FARFBUZZ_MATRIX_PASSES</c> by default.</summary>
let interpreterPasses = environmentOr "FARFBUZZ_MATRIX_PASSES_INTERP" matrixPasses

/// <summary>The folder holding the runner's publishes and JSON.</summary>
let runnerDirectory = Path.Combine(root, "artifacts", "runner")

/// <summary>The browser bundle of the wasm runner, where <c>main.mjs</c> runs under Node.</summary>
let wasmRunnerBundle = Path.Combine(root, "artifacts", "bin", "Farfbuzz.Bench.Wasm", "release_browser-wasm", "AppBundle")

/// <summary>Where the fonts and texts of the scripts added to the speed comparison are fetched to, outside the repository's history.</summary>
let benchDataDirectory = Path.Combine(root, "artifacts", "bench-data")

/// <summary>The files of the added scripts, each with its folder under <see cref="benchDataDirectory"/>, its name, the URL it comes from
/// (pinned to a commit) and its SHA-256. The fonts are Noto and Marck Script (SIL Open Font License 1.1), the texts the Universal
/// Declaration of Human Rights from the UDHR in Unicode collection (Unicode licence).</summary>
let benchDataFiles =
    let noto = "https://raw.githubusercontent.com/notofonts/notofonts.github.io/682229039c5a594f247890d94e3fb497be8d58dd/fonts"
    let cjk = "https://raw.githubusercontent.com/notofonts/noto-cjk/f8d157532fbfaeda587e826d4cd5b21a49186f7c/Sans/SubsetOTF"
    let udhr = "https://raw.githubusercontent.com/eric-muller/udhr/e6a1b6f8cb048fba115d6b81bab47875a4eca0d8/data/udhr"
    let google = "https://raw.githubusercontent.com/google/fonts/06bfe0d62a98375cb09c942fa18e68bd32cff057/ofl"
    [ "fonts", "NotoSansGeorgian-Regular.ttf", $"{noto}/NotoSansGeorgian/hinted/ttf/NotoSansGeorgian-Regular.ttf", "d3e33254b09e7bb2c5cf0f17e554b80462056c5a107097f258d495168c3a9346"
      "fonts", "NotoSansArmenian-Regular.ttf", $"{noto}/NotoSansArmenian/hinted/ttf/NotoSansArmenian-Regular.ttf", "720df88c332417a235b4d6209d14ec2e2bf4bfe2a954b7453d869ea593bfce1e"
      "fonts", "NotoSansThai-Regular.ttf", $"{noto}/NotoSansThai/hinted/ttf/NotoSansThai-Regular.ttf", "61cf814eec46b294d6ea4401ac295d0cecd5207bd2331dcc5a15e7301d30ee44"
      "fonts", "NotoNaskhArabic-Regular.ttf", $"{noto}/NotoNaskhArabic/hinted/ttf/NotoNaskhArabic-Regular.ttf", "6f0a92031367b2f5a2078fe9d24f3433122b61a0bad57c423aad8f3c39aa2e6e"
      "fonts", "NotoSansSC-Regular.otf", $"{cjk}/SC/NotoSansSC-Regular.otf", "faa6c9df652116dde789d351359f3d7e5d2285a2b2a1f04a2d7244df706d5ea9"
      "fonts", "NotoSansJP-Regular.otf", $"{cjk}/JP/NotoSansJP-Regular.otf", "dff723ba59d57d136764a04b9b2d03205544f7cd785a711442d6d2d085ac5073"
      "fonts", "NotoSansKR-Regular.otf", $"{cjk}/KR/NotoSansKR-Regular.otf", "69975a0ac8472717870aefeab0a4d52739308d90856b9955313b2ad5e0148d68"
      "fonts", "MarckScript-Regular.ttf", $"{google}/marckscript/MarckScript-Regular.ttf", "504de8cc6f919163bc9afb67b9bc208d258230c5e8d00cb82ec52474f9fafc3b"
      "udhr", "udhr_pol.xml", $"{udhr}/udhr_pol.xml", "12716a61c3a279c3f361ff621f0f01067ec8a340f6481bac7b234345b43928b3"
      "udhr", "udhr_nob.xml", $"{udhr}/udhr_nob.xml", "fdeb0f9081735b337cdd2d872a397a950077b2d55950fe15a11ffd54b95511a3"
      "udhr", "udhr_hun.xml", $"{udhr}/udhr_hun.xml", "f28144225aa49d73d3b1bc68c50061f6cfbaad3f79868d5b4a09158ee00836c9"
      "udhr", "udhr_rus.xml", $"{udhr}/udhr_rus.xml", "4d91ee6b21c4f2fd26bae1a62b6f2922b2a33335b1559ccfaea29bdaf2ec2972"
      "udhr", "udhr_ell_monotonic.xml", $"{udhr}/udhr_ell_monotonic.xml", "961f32b7aca8371e4709352c9c096b28c6876867aa45e233736547c46fe35748"
      "udhr", "udhr_ell_polytonic.xml", $"{udhr}/udhr_ell_polytonic.xml", "60242b91f737fbfb4b8026453c8a507d3cd33626ee043929a1725c26d25b70e3"
      "udhr", "udhr_kat.xml", $"{udhr}/udhr_kat.xml", "9008d8a70d9883d8099c9d18326c3f39ea1d6d7440c69ab05f9607d8573f15a9"
      "udhr", "udhr_hye.xml", $"{udhr}/udhr_hye.xml", "a12927bfa39c328555c92f002538eceb12be32b0440470f739bba294dfb46188"
      "udhr", "udhr_cmn_hans.xml", $"{udhr}/udhr_cmn_hans.xml", "b72660bd2a51835a6dec1a74d87c2f22a41b706be865cc102efcf2317e77f0c7"
      "udhr", "udhr_jpn.xml", $"{udhr}/udhr_jpn.xml", "6baad6c56d629abed7c824ffe0d9f6ad168838ccdd348d0053bc9d51431c7ecb"
      "udhr", "udhr_kor.xml", $"{udhr}/udhr_kor.xml", "1d2da9c033c6a662da4726e93e72b791a6a00a3d718361513aa40fb0b85795df"
      "udhr", "udhr_tha.xml", $"{udhr}/udhr_tha.xml", "4794405502643a3f60568df3d7e841465650c374d02ad3af9fde952e984200b1"
      "udhr", "udhr_khk.xml", $"{udhr}/udhr_khk.xml", "25cfa21379a01da753494304030772bb91215b619e77aed5f3638017b1e8ace6"
      "udhr", "udhr_arb.xml", $"{udhr}/udhr_arb.xml", "a04e769170f8acaf9afe84b723e0a9865950cca9ec35e47b08da831a7e86dcec" ]

/// <summary>The text file each UDHR file becomes, named after the language tag the benchmark pairs use.</summary>
let benchDataTexts =
    [ "udhr_pol.xml", "pl-udhr.txt"; "udhr_nob.xml", "nb-udhr.txt"; "udhr_hun.xml", "hu-udhr.txt"; "udhr_rus.xml", "ru-udhr.txt"
      "udhr_ell_monotonic.xml", "el-udhr.txt"; "udhr_ell_polytonic.xml", "el-polyton-udhr.txt"; "udhr_kat.xml", "ka-udhr.txt"; "udhr_hye.xml", "hy-udhr.txt"; "udhr_cmn_hans.xml", "zh-udhr.txt"
      "udhr_jpn.xml", "ja-udhr.txt"; "udhr_kor.xml", "ko-udhr.txt"; "udhr_tha.xml", "th-udhr.txt"; "udhr_khk.xml", "mn-udhr.txt"
      "udhr_arb.xml", "ar-udhr.txt" ]

/// <summary>The SHA-256 of a file, in lower-case hexadecimal.</summary>
/// <param name="path">The file.</param>
/// <returns>The hash.</returns>
let sha256Of (path: string) =
    use stream = File.OpenRead path
    Convert.ToHexString(Security.Cryptography.SHA256.HashData stream).ToLowerInvariant()

/// <summary>The lines a UDHR file gives the benchmark: the text of every title and paragraph in document order, inner white space
/// collapsed to one space, each ended by a newline.</summary>
/// <param name="path">The UDHR XML file.</param>
/// <returns>The text.</returns>
let udhrText (path: string) =
    Xml.Linq.XDocument.Load(path).Descendants()
    |> Seq.filter (fun element -> element.Name.LocalName = "title" || element.Name.LocalName = "para")
    |> Seq.map (fun element -> Regex.Replace(element.Value, @"\s+", " ").Trim())
    |> Seq.filter (fun line -> line.Length > 0)
    |> Seq.map (fun line -> line + "\n")
    |> String.concat String.Empty

/// <summary>Fetches the fonts and texts of the added scripts into <see cref="benchDataDirectory"/>, keeping a file already there whose
/// SHA-256 matches, refusing one that does not, and writes the text file of each UDHR file.</summary>
/// <returns>Zero when every file is in place with its pinned hash.</returns>
let fetchBenchData () =
    use client = new Net.Http.HttpClient()
    let fetched =
        benchDataFiles
        |> List.map (fun (folder, name, url, expected) ->
            let directory = Path.Combine(benchDataDirectory, folder)
            Directory.CreateDirectory directory |> ignore
            let path = Path.Combine(directory, name)
            match File.Exists path && sha256Of path = expected with
            | true -> true
            | false ->
                let partial = path + ".part"
                File.WriteAllBytes(partial, client.GetByteArrayAsync(url).GetAwaiter().GetResult())
                match sha256Of partial = expected with
                | true ->
                    File.Move(partial, path, true)
                    Console.WriteLine $"    fetched {folder}/{name}"
                    true
                | false ->
                    Console.Error.WriteLine $"    {folder}/{name}: SHA-256 {sha256Of partial}, expected {expected} ({url})"
                    File.Delete partial
                    false)
    match List.forall id fetched with
    | false -> 1
    | true ->
        let texts = Path.Combine(benchDataDirectory, "texts")
        Directory.CreateDirectory texts |> ignore
        benchDataTexts
        |> List.iter (fun (source, target) -> File.WriteAllText(Path.Combine(texts, target), udhrText (Path.Combine(benchDataDirectory, "udhr", source))))
        Console.WriteLine $"    {benchDataFiles.Length} files in place, {benchDataTexts.Length} texts written to {Path.GetRelativePath(root, texts)}"
        0

/// <summary>The pairs the NativeAOT runner measures: the ten of HarfBuzz's benchmark, and the added scripts when their files are fetched.</summary>
let aotPairs =
    match Directory.Exists(Path.Combine(benchDataDirectory, "texts")) with
    | true -> "with-scripts"
    | false -> "all"

/// <summary>The command line of one runner measurement.</summary>
/// <param name="pairs">The pairs, <c>all</c> or <c>with-scripts</c>.</param>
/// <param name="passes">The fewest passes.</param>
/// <param name="impl">The implementation, <c>farfbuzz</c> or <c>native</c>.</param>
/// <param name="json">The JSON file the runner writes.</param>
/// <returns>The arguments after the program.</returns>
let runnerArguments pairs passes impl json =
    [ "shape"; "--pair"; pairs; "--passes"; passes; "--impl"; impl; "--json"; json ]

/// <summary>The publish arguments that build the native implementation into a runner.</summary>
/// <param name="impl">The implementation.</param>
/// <returns>The extra arguments.</returns>
let implArguments impl =
    match impl with
    | "native" -> [ "-p:BenchImpl=native" ]
    | _ -> []

/// <summary>Publishes the NativeAOT runner for the host and runs it on every pair.</summary>
/// <param name="impl">The implementation.</param>
/// <returns>The publish step and the run step.</returns>
let runnerAotSteps impl =
    let name = if impl = "farfbuzz" then "aot" else $"aot-{impl}"
    let directory = Path.Combine(runnerDirectory, name)
    [ dotnet $"publish runner, NativeAOT, {impl}"
        ([ "publish"; "bench/Farfbuzz.Bench.Runner"; "-c"; "Release"; "-r"; hostRid; "-p:PublishAot=true"; "-o"; directory ] @ implArguments impl @ benchUpstream)
      { Title = $"runner, NativeAOT, {impl}"
        Action = Run (Path.Combine(directory, "Farfbuzz.Bench.Runner"), runnerArguments aotPairs matrixPasses impl (Path.Combine(runnerDirectory, $"aot-{impl}.json")), root) } ]

/// <summary>Publishes the browser-wasm runner, with or without ahead-of-time compilation, and runs it under Node on every pair.</summary>
/// <param name="flavour">The label and file name stem, <c>wasm-interp</c> or <c>wasm-aot</c>.</param>
/// <param name="aot">Whether to compile ahead of time.</param>
/// <param name="impl">The implementation.</param>
/// <returns>The publish step and the run step.</returns>
let runnerWasmSteps flavour aot impl =
    let compilation = if aot then [ "-p:RunAOTCompilation=true" ] else []
    [ dotnet $"publish runner, {flavour}, {impl}"
        ([ "publish"; "bench/Farfbuzz.Bench.Wasm"; "-c"; "Release" ] @ compilation @ implArguments impl @ benchUpstream)
      { Title = $"runner, {flavour}, {impl}"
        Action = Run ("node", "main.mjs" :: runnerArguments "all" (if aot then matrixPasses else interpreterPasses) impl (Path.Combine(runnerDirectory, $"{flavour}-{impl}.json")), wasmRunnerBundle) } ]

/// <summary>Every runner measurement of the platform matrix: NativeAOT, WebAssembly interpreter and WebAssembly AOT, Farfbuzz then native.</summary>
let runnerSteps =
    ([ "farfbuzz"; "native" ] |> List.collect runnerAotSteps)
    @ ([ "farfbuzz"; "native" ] |> List.collect (runnerWasmSteps "wasm-interp" false))
    @ ([ "farfbuzz"; "native" ] |> List.collect (runnerWasmSteps "wasm-aot" true))

/// <summary>The BenchmarkDotNet steps of the platform matrix: the CoreCLR measurements of the <c>perf</c> stage, left out only when
/// <c>FARFBUZZ_PERF_SKIP_BDN</c> is set, for a checkout whose <c>upstream/harfbuzz</c> or oracle is not built.</summary>
let matrixBenchmarkSteps =
    match environmentOr "FARFBUZZ_PERF_SKIP_BDN" String.Empty with
    | skip when skip.Length = 0 -> [ benchmarkStep ]
    | _ -> []

/// <summary>The directory the equivalence dumps are written to.</summary>
let equivalenceDirectory = Path.Combine(root, "artifacts", "equivalence")

/// <summary>The equivalence project in one implementation, as a <c>dotnet</c> step.</summary>
/// <param name="title">What the step does.</param>
/// <param name="verb">The dotnet verb, build or run.</param>
/// <param name="implementation">native, facade or core.</param>
/// <param name="arguments">What follows the project arguments.</param>
/// <returns>The step.</returns>
let equivalenceStep title verb implementation arguments =
    let common = [ verb; "--project"; "tests/Farfbuzz.HarfBuzzSharp.Equivalence"; "-c"; "Release"; $"-p:HarfBuzzSharpImpl={implementation}" ]
    match verb with
    | "build" -> dotnet title ([ "build"; "tests/Farfbuzz.HarfBuzzSharp.Equivalence"; "-c"; "Release"; $"-p:HarfBuzzSharpImpl={implementation}" ] @ arguments)
    | _ -> dotnet title (common @ [ "--no-build" ] @ arguments)

/// <summary>The steps of the whole-suite equivalence: the three builds, the three dumps and the comparison, which requires that the facade
/// differs from the native implementation exactly where the core does, and equals the core there.</summary>
let equivalenceSteps =
    let dump implementation = Path.Combine(equivalenceDirectory, $"{implementation}.tsv")
    [ { Title = "equivalence dumps folder"; Action = Inline (fun () -> Directory.CreateDirectory equivalenceDirectory |> ignore; 0) } ]
    @ [ for implementation in [ "native"; "facade"; "core" ] -> equivalenceStep $"equivalence, build {implementation}" "build" implementation [] ]
    @ [ for implementation in [ "native"; "facade"; "core" ] ->
            equivalenceStep $"equivalence, dump {implementation}" "run" implementation [ "--"; "dump"; dump implementation ] ]
    @ [ equivalenceStep "equivalence, compare" "run" "core" [ "--"; "compare"; dump "native"; dump "facade"; dump "core" ] ]

/// <summary>Compares the facade with the public surface of HarfBuzzSharp 14.4.0, strictly and with parameter names, against the reference pack of the
/// running runtime; there is no suppression file.</summary>
/// <returns>The exit code of ApiCompat.</returns>
let apiCompat () =
    let dotnetRoot = Path.GetDirectoryName Environment.ProcessPath
    let packs = Path.Combine(dotnetRoot, "packs", "Microsoft.NETCore.App.Ref")
    let reference =
        Directory.GetDirectories packs
        |> Array.sortDescending
        |> Array.map (fun version -> Path.Combine(version, "ref", "net10.0"))
        |> Array.find Directory.Exists
    let package = Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.UserProfile, ".nuget", "packages", "harfbuzzsharp", "14.4.0", "lib", "net10.0", "HarfBuzzSharp.dll")
    let facade = Path.Combine(root, "artifacts", "bin", "Farfbuzz.HarfBuzzSharp", "release", "HarfBuzzSharp.dll")
    fst (execute "dotnet" [ "apicompat"; "--left-assembly"; package; "--right-assembly"; facade; "--strict-mode"; "--enable-rule-cannot-change-parameter-name"
                            "--lref"; reference; "--rref"; $"{reference},{Path.GetDirectoryName facade}" ] root false)

/// <summary>The headless Avalonia 11 corpus through HarfBuzz and through the facade, and the comparison of the two dumps.</summary>
let avaloniaHeadlessSteps =
    let native = Path.Combine(root, "artifacts", "avalonia11", "native.tsv")
    let facade = Path.Combine(root, "artifacts", "avalonia11", "facade.tsv")
    [ dotnet "Avalonia 11 headless corpus, HarfBuzz" [ "run"; "-c"; "Release"; "--project"; "samples/Farfbuzz.Samples.Avalonia11.Headless"; "-p:UseFarfbuzz=false"; "--"; native ]
      dotnet "Avalonia 11 headless corpus, facade" [ "run"; "-c"; "Release"; "--project"; "samples/Farfbuzz.Samples.Avalonia11.Headless"; "-p:UseFarfbuzz=true"; "--"; facade ]
      { Title = "Avalonia 11 headless corpus, the two dumps are identical"
        Action = Inline (fun () -> match File.ReadAllBytes native = File.ReadAllBytes facade with | true -> 0 | false -> Console.Error.WriteLine "the Avalonia 11 dumps differ"; 1) } ]

/// <summary>Where the package check packs to, restores into and builds its consumer; emptied before every check.</summary>
let packageDirectory = Path.Combine(root, "artifacts", "package")

/// <summary>The projects published as NuGet packages, in dependency order.</summary>
let packedProjects = [ "src/Farfbuzz"; "src/Farfbuzz.HarfBuzzSharp.Bridge"; "src/Farfbuzz.HarfBuzzSharp" ]

/// <summary>The outside consumer of the facade package.</summary>
let packageConsumer = "tests/Farfbuzz.Package.Consumer"

/// <summary>Packs every published project and installs the facade package in an outside consumer, restored into its own package
/// folder so the machine's NuGet cache never holds a locally packed version, then shapes a line through it.</summary>
let packageSteps =
    [ { Title = "package check, empty its folder"
        Action = Inline (fun () ->
            match Directory.Exists packageDirectory with
            | true -> Directory.Delete(packageDirectory, true)
            | false -> ()
            0) } ]
    @ [ for project in packedProjects ->
            dotnet $"pack {project}" [ "pack"; project; "-c"; "Release"; "-o"; Path.Combine(packageDirectory, "nupkg") ] ]
    @ [ dotnet "restore the facade package in an outside consumer" [ "restore"; packageConsumer; "--packages"; Path.Combine(packageDirectory, "packages") ]
        dotnet "shape a line through the installed facade package"
            [ "run"; "--project"; packageConsumer; "--no-restore"; "--"; Path.Combine(root, "samples", "Farfbuzz.Samples.Avalonia11", "Assets", "Roboto-Regular.ttf") ] ]

/// <summary>The stages this script knows, each a list of steps run in order.</summary>
let stages : Map<string, Step list> =
    Map [
        "guard", [ { Title = "guard: protected files, baselines, banned constructs, documentation"; Action = Inline guard } ]
        "install-hooks", [ { Title = "install the pre-commit hook"; Action = Inline installHooks } ]
        "build", [ dotnet "build (Release)" [ "build"; "Farfbuzz.slnx"; "-c"; "Release" ] ]
        "tests", [ expecto "tests/Farfbuzz.Tests" []; expecto "tests/Farfbuzz.PublicApi" [] ]
        "oracle", [ { Title = "build HarfBuzz 15.1.0 oracle and hb-shape"; Action = Inline buildOracle } ]
        "oracle-ensure", [ { Title = "ensure the HarfBuzz oracle is built"; Action = Inline ensureOracle } ]
        "diff", [ expecto "tests/Farfbuzz.Diff" [] ]
        "bench", [ dotnet "benchmarks, both native jobs (long)" [ "run"; "-c"; "Release"; "--project"; "bench/Farfbuzz.Bench"; "--"; "--filter"; "*"; "--exporters"; "github" ] ]
        "bench-quick", [ dotnet "benchmark smoke (toolkit, dry)" [ "run"; "-c"; "Release"; "--project"; "bench/Farfbuzz.Bench"; "--"; "--filter"; "*Folds*"; "--job"; "dry" ] ]
        "perf", [ benchmarkStep; reportStep String.Empty ]
        "perf-matrix", matrixBenchmarkSteps @ runnerSteps @ [ reportStep (environmentOr "FARFBUZZ_PERF_NOTE" String.Empty) ]
        "metrics", [ { Title = "complexity of Farfbuzz modules against the upstream functions they port (lizard 1.25.1)"; Action = Inline metrics } ]
        "annotations", [ { Title = "lower the result-annotation baseline"; Action = Inline lowerAnnotationBaseline } ]
        "ratchet",
            [ { Title = "promote newly passing conformance runs"
                Action = Inline (fun () -> mergeBaseline (Path.Combine(root, "artifacts", "conformance", "newly-passing.txt")) (Path.Combine(root, "tests", "Farfbuzz.Tests", "conformance.baseline"))) }
              { Title = "promote newly agreeing random-text cases"
                Action = Inline (fun () -> mergeBaseline (Path.Combine(root, "artifacts", "conformance", "random-text-agreeing.txt")) (Path.Combine(root, "tests", "Farfbuzz.Diff", "random-text.baseline"))) } ]
        "facade",
            [ dotnet "facade probe, CoreCLR (identity, writable glyph span, consumer surfaces)"
                [ "run"; "--project"; "tests/Farfbuzz.Facade.Probe"; "-c"; "Release"; "--no-build" ] ]
            @ equivalenceSteps
            @ [ { Title = "ApiCompat of the facade against HarfBuzzSharp 14.4.0, strict, without a suppression file"; Action = Inline apiCompat } ]
            @ [ for implementation in [ "native"; "facade" ] ->
                    dotnet $"C# probe, {implementation}" [ "run"; "--project"; "tests/Farfbuzz.HarfBuzzSharp.CSharpProbe"; "-c"; "Release"; $"-p:HarfBuzzSharpImpl={implementation}" ] ]
            @ avaloniaHeadlessSteps
        "package", packageSteps
        "bench-data", [ { Title = "fetch the fonts and texts of the added scripts, pinned by SHA-256"; Action = Inline fetchBenchData } ]
        "facade-native",
            [ dotnet "publish facade probe, NativeAOT" [ "publish"; "tests/Farfbuzz.Facade.Probe"; "-f"; "net10.0"; "-r"; hostRid; "-c"; "Release" ]
              { Title = "facade probe, NativeAOT"; Action = Run (nativeFacadeProbe, [], root) } ]
        "facade-wasm",
            [ dotnet "publish facade probe, browser-wasm" [ "publish"; "tests/Farfbuzz.Facade.Probe"; "-f"; "net10.0-browser"; "-c"; "Release"; "-p:FarfbuzzWasm=true" ]
              { Title = "facade probe, browser-wasm"; Action = Run ("node", [ "main.mjs" ], wasmFacadeBundle) } ]
        "smoke-native",
            [ publishSmoke "publish smoke, NativeAOT" [ "-f"; "net10.0"; "-r"; hostRid ] ]
            @ smokeRuns "NativeAOT" nativeSmoke [] root
        "smoke-wasm",
            [ publishSmoke "publish smoke, browser-wasm interpreter" [ "-f"; "net10.0-browser"; "-p:FarfbuzzWasm=true" ] ]
            @ smokeRuns "browser-wasm interpreter" "node" [ "main.mjs" ] wasmBundle
        "smoke-aot",
            [ publishSmoke "publish smoke, browser-wasm AOT" [ "-f"; "net10.0-browser"; "-p:FarfbuzzWasm=true"; "-p:RunAOTCompilation=true" ] ]
            @ smokeRuns "browser-wasm AOT" "node" [ "main.mjs" ] wasmBundle
    ]

/// <summary>Stage aliases expanding to several stages.</summary>
let aliases : Map<string, string list> =
    Map [
        "default", [ "guard"; "build"; "tests"; "oracle-ensure"; "diff"; "facade"; "package" ]
        "smoke", [ "smoke-native"; "smoke-wasm" ]
        "native-ci", [ "guard"; "build"; "tests"; "oracle-ensure"; "diff"; "facade"; "smoke-native"; "facade-native"; "bench-quick" ]
        "ci", [ "guard"; "build"; "tests"; "oracle-ensure"; "diff"; "facade"; "smoke-native"; "smoke-wasm"; "facade-native"; "facade-wasm"; "bench-quick" ]
        "full", [ "guard"; "build"; "tests"; "oracle-ensure"; "diff"; "facade"; "smoke-native"; "smoke-wasm"; "facade-native"; "facade-wasm"; "smoke-aot" ]
    ]

/// <summary>Runs one step and returns its exit code.</summary>
/// <param name="step">The step to run.</param>
/// <returns>The exit code: zero on success.</returns>
let runStep (step: Step) : int =
    Console.WriteLine $"==> {step.Title}"
    match step.Action with
    | Run (program, arguments, directory) -> fst (execute program arguments directory false)
    | RunChecked (program, arguments, offending) ->
        let code, lines = execute program arguments root true
        let bad = lines |> List.filter offending |> List.distinct
        bad |> List.iter (fun line -> Console.Error.WriteLine line)
        match code, bad with
        | 0, [] -> 0
        | 0, _ ->
            Console.Error.WriteLine $"{bad.Length} trimming/AOT warning(s) outside FSharp.Core"
            1
        | failure, _ ->
            lines |> List.filter (fun line -> line.Contains "error") |> List.iter (fun line -> Console.Error.WriteLine line)
            failure
    | Probe (program, arguments, directory) ->
        let code, lines = execute program arguments directory true
        let verdict = if code = 0 then "survived" else "overflowed the stack or failed"
        let detail = lines |> List.tryFind (fun line -> line.Contains "PASS" || line.Contains "RangeError") |> Option.defaultValue ""
        Console.WriteLine $"    probe {verdict} (exit code {code}) {detail.Trim()}"
        0
    | Inline computation -> computation ()

/// <summary>Runs steps in order, stopping at the first failure.</summary>
/// <param name="steps">The steps.</param>
/// <returns>Zero, or the exit code of the first failing step.</returns>
[<TailCall>]
let rec runAll (steps: Step list) : int =
    match steps with
    | [] -> 0
    | step :: rest ->
        match runStep step with
        | 0 -> runAll rest
        | failure ->
            Console.Error.WriteLine $"FAILED: {step.Title} (exit code {failure})"
            failure

/// <summary>Expands stage names and aliases into steps; unknown names are reported.</summary>
/// <param name="names">The requested stage names.</param>
/// <returns>The steps, or the unknown names.</returns>
let resolve (names: string list) : Result<Step list, string list> =
    let expanded = names |> List.collect (fun name -> aliases |> Map.tryFind name |> Option.defaultValue [ name ])
    match expanded |> List.filter (fun name -> not (stages.ContainsKey name)) with
    | [] -> Ok (expanded |> List.collect (fun name -> stages[name]))
    | unknown -> Error unknown

let requested =
    match fsi.CommandLineArgs |> Array.toList |> List.tail with
    | [] -> [ "default" ]
    | names -> names

match resolve requested with
| Ok steps -> exit (runAll steps)
| Error unknown ->
    Console.Error.WriteLine $"""Unknown stage(s): {String.Join(", ", unknown)}. Known: {String.Join(", ", Seq.append stages.Keys aliases.Keys)}"""
    exit 2

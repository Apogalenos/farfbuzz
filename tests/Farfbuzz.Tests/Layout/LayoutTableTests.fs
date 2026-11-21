module Farfbuzz.Tests.Layout.LayoutTableTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>The NotoNastaliq Urdu face, as <c>test-ot-layout.c</c> uses it.</summary>
/// <returns>The OpenType face.</returns>
let private nastaliq () =
    OtFace.create (Face.create (read "test/api/fonts/NotoNastaliqUrdu-Regular.ttf") 0u)

/// <summary>The Estedad face, as <c>test-ot-layout.c</c> uses it.</summary>
/// <returns>The OpenType face.</returns>
let private estedad () =
    OtFace.create (Face.create (read "test/api/fonts/Estedad-VF.ttf") 0u)

/// <summary>Finds a script, as <c>hb_ot_layout_table_find_script</c>.</summary>
/// <param name="face">The OpenType face.</param>
/// <param name="table">The table tag.</param>
/// <param name="tag">The script tag.</param>
/// <returns>Whether found and the index.</returns>
let private findScript (face: OtFace) (table: Tag) (tag: Tag) : struct (bool * int) =
    let struct (found, index, _) = LayoutApi.selectScript face table (ImmutableArray.Create tag)
    struct (found, index)

/// <summary>Builds a one-script script list for a tag.</summary>
/// <param name="tag">The script tag.</param>
/// <returns>The bytes.</returns>
let private oneScriptList (tag: Tag) : byte array =
    Array.concat [ be16 1u; be32 (uint32 tag); be16 8u; be16 0u; be16 0u ]

/// <summary>The P5.S03 GSUB/GPOS table cases.</summary>
[<Tests>]
let tests =
    testList "P5.S03" [
        test "table_get_script_tags" {
            let otFace = nastaliq ()
            Expect.equal (LayoutApi.scriptTags otFace Tags.Gsub |> List.ofSeq) [ Tag.make 'a' 'r' 'a' 'b'; Tags.dflt; Tags.Latn ] "GSUB scripts"
            Expect.equal (LayoutApi.scriptTags otFace Tags.Gpos |> List.ofSeq) [ Tag.make 'a' 'r' 'a' 'b' ] "GPOS scripts"
        }
        test "table_find_script" {
            let otFace = nastaliq ()
            let arab = Tag.make 'a' 'r' 'a' 'b'
            Expect.equal (findScript otFace Tags.Gsub arab) (struct (true, 0)) "GSUB arab"
            Expect.equal (findScript otFace Tags.Gsub Tags.dflt) (struct (true, 1)) "GSUB dflt"
            Expect.equal (findScript otFace Tags.Gsub Tags.Latn) (struct (true, 2)) "GSUB latn"
            Expect.equal (findScript otFace Tags.Gpos arab) (struct (true, 0)) "GPOS arab"
            Expect.equal (findScript otFace Tags.Gpos Tags.dflt) (struct (false, LayoutTable.NoScriptIndex)) "GPOS dflt"
            Expect.equal (findScript otFace Tags.Gpos Tags.Latn) (struct (false, LayoutTable.NoScriptIndex)) "GPOS latn"
        }
        test "table_get_feature_tags" {
            let otFace = nastaliq ()
            let gsub = LayoutApi.tableFeatureTags otFace Tags.Gsub |> List.ofSeq
            Expect.equal (List.length gsub) 14 "GSUB has 14 features"
            Expect.equal gsub[0] (Tag.make 'c' 'c' 'm' 'p') "ccmp first"
            Expect.equal gsub[10] (Tag.make 'i' 's' 'o' 'l') "isol tenth"
            Expect.equal gsub[13] (Tag.make 'r' 'l' 'i' 'g') "rlig last"
            let gpos = LayoutApi.tableFeatureTags otFace Tags.Gpos |> List.ofSeq
            Expect.equal gpos [ Tag.make 'c' 'u' 'r' 's'; Tag.make 'm' 'a' 'r' 'k'; Tag.make 'm' 'k' 'm' 'k' ] "GPOS features"
        }
        test "script_get_language_tags" {
            let otFace = estedad ()
            let far = Tag.make 'F' 'A' 'R' ' '
            let kur = Tag.make 'K' 'U' 'R' ' '
            Expect.equal (LayoutApi.languageTags otFace Tags.Gsub 0 |> List.ofSeq) [ far; kur ] "GSUB script 0 languages"
            Expect.equal (LayoutApi.languageTags otFace Tags.Gpos 1 |> List.ofSeq) [ far; kur ] "GPOS script 1 languages"
        }
        test "language_get_feature_tags" {
            let otFace = estedad ()
            Expect.equal
                (LayoutApi.featureTags otFace Tags.Gsub 0 0 |> List.ofSeq)
                [ Tag.make 'c' 'a' 'l' 't'; Tag.make 'f' 'i' 'n' 'a'; Tag.make 'i' 'n' 'i' 't'; Tag.make 'l' 'i' 'g' 'a'; Tag.make 'm' 'e' 'd' 'i'; Tag.make 'r' 'l' 'i' 'g' ]
                "GSUB script 0 language 0 features"
            Expect.equal
                (LayoutApi.featureTags otFace Tags.Gpos 1 0 |> List.ofSeq)
                [ Tag.make 'k' 'e' 'r' 'n'; Tag.make 'm' 'a' 'r' 'k'; Tag.make 'm' 'k' 'm' 'k' ]
                "GPOS script 1 language 0 features"
        }
        test "1.2 offsets win" {
            let gsub =
                Array.concat
                    [ be32 0x00010002u
                      be16 26u
                      be16 0u
                      be16 0u
                      be32 0u
                      be32 38u
                      be32 0u
                      be32 0u
                      oneScriptList (Tag.make 'l' 'a' 't' 'n')
                      oneScriptList (Tag.make 'a' 'r' 'a' 'b') ]
            let otFace = OtFace.create (Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Gsub, gsub ])) 0u)
            Expect.equal (LayoutApi.scriptTags otFace Tags.Gsub |> List.ofSeq) [ Tag.make 'a' 'r' 'a' 'b' ] "the 32-bit script list wins"
        }
        test "unsorted ScriptList" {
            let scriptList =
                Array.concat
                    [ be16 2u
                      be32 (uint32 (Tag.make 'l' 'a' 't' 'n')); be16 14u
                      be32 (uint32 (Tag.make 'a' 'r' 'a' 'b')); be16 18u
                      be16 0u; be16 0u
                      be16 0u; be16 0u ]
            let gsub = Array.concat [ be32 0x00010000u; be16 10u; be16 0u; be16 0u; scriptList ]
            let table = (OtFace.create (Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Gsub, gsub ])) 0u)).Gsub.Value
            Expect.equal (LayoutTable.findScriptIndex table (Tag.make 'l' 'a' 't' 'n')) (ValueSome 0) "latn is found at 0"
            Expect.equal (LayoutTable.findScriptIndex table (Tag.make 'a' 'r' 'a' 'b')) ValueNone "arab is missed by the binary search"
        }
    ]

/// <summary>Builds a GSUB table with one script and the given number of single-substitution lookups.</summary>
/// <param name="count">The number of lookups, each with one coverage.</param>
/// <returns>The GSUB table bytes.</returns>
let private gsubWithLookups (count: int) : byte array =
    let coverage = Array.concat [ be16 1u; be16 1u; be16 1u ]
    let subtable = Array.concat [ be16 1u; be16 6u; be16 1u; coverage ]
    let lookup = Array.concat [ be16 1u; be16 0u; be16 1u; be16 8u; subtable ]
    let scriptList = Array.concat [ be16 1u; be32 (uint32 (Tag.make 'l' 'a' 't' 'n')); be16 8u; be16 0u; be16 0u ]
    let offsets =
        [ 0 .. count - 1 ]
        |> List.map (fun index -> be16 (uint32 (2 + count * 2 + index * lookup.Length)))
        |> Array.concat
    let lookupList = Array.concat [ be16 (uint32 count); offsets; Array.concat (List.replicate count lookup) ]
    Array.concat [ be32 0x00010000u; be16 10u; be16 0u; be16 22u; scriptList; lookupList ]

/// <summary>The budget a load of a GSUB table with 2 000 lookups stays under: the fixed cost of the Lazy array and its
/// closures, without any coverage walk. Recorded at 467 704 bytes after the accelerator became lazy.</summary>
[<Literal>]
let GsubLoadBudget = 500_000L

/// <summary>The PERF4 test: loading a layout table builds no accelerator, and allocates under a budget.</summary>
[<Tests>]
let perf4Tests =
    testList "PERF4" [
        test "loading a layout table builds no accelerator" {
            let gsub = gsubWithLookups 2000
            let fontBytes = sfnt 0x00010000u [ Tags.Gsub, gsub ]
            let table = (OtFace.create (Face.create (ReadOnlyMemory fontBytes) 0u)).Gsub.Value
            Expect.isGreaterThan table.Lookups.Length 0 "the table has lookups"
            let mutable created = false
            for lazyAccel in table.Lookups do
                if lazyAccel.IsValueCreated then created <- true
            Expect.isFalse created "no accelerator value is built during load"
            let allocated =
                Farfbuzz.Tests.Allocation.bytesAllocatedBy (fun () ->
                    let face = Face.create (ReadOnlyMemory fontBytes) 0u
                    (OtFace.create face).Gsub.Value |> ignore)
            Expect.isLessThanOrEqual allocated GsubLoadBudget "bytes allocated by the load"
        }
    ]

/// <summary>A single-substitution format 1 subtable of one coverage and a zero delta.</summary>
let private single1 =
    Array.concat [ be16 1u; be16 6u; be16 0u; Array.concat [ be16 1u; be16 1u; be16 1u ] ]

/// <summary>The minimal subtable bytes of every GSUB (lookup type, format) pair, 16- and 24-bit, that a real font can carry.</summary>
let private gsubPairs : (int * int * byte array) list =
    [ 1, 1, Array.concat [ be16 1u; be16 0u; be16 0u ]
      1, 2, Array.concat [ be16 2u; be16 0u; be16 0u ]
      1, 3, Array.concat [ be16 3u; be32 0u; be24 0u ]
      1, 4, Array.concat [ be16 4u; be32 0u; be24 0u ]
      2, 1, Array.concat [ be16 1u; be16 0u; be16 0u ]
      2, 2, Array.concat [ be16 2u; be32 0u; be24 0u ]
      3, 1, Array.concat [ be16 1u; be16 0u; be16 0u ]
      3, 2, Array.concat [ be16 2u; be32 0u; be24 0u ]
      4, 1, Array.concat [ be16 1u; be16 0u; be16 0u ]
      4, 2, Array.concat [ be16 2u; be32 0u; be24 0u ]
      5, 1, Array.concat [ be16 1u; be16 0u; be16 0u ]
      5, 2, Array.concat [ be16 2u; be16 0u; be16 0u; be16 0u ]
      5, 3, Array.concat [ be16 3u; be16 0u; be16 0u ]
      5, 4, Array.concat [ be16 4u; be32 0u; be24 0u ]
      5, 5, Array.concat [ be16 5u; be32 0u; be32 0u; be24 0u ]
      5, 6, Array.concat [ be16 6u; be16 0u; be16 0u ]
      6, 1, Array.concat [ be16 1u; be16 0u; be16 0u ]
      6, 2, Array.concat [ be16 2u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u ]
      6, 3, Array.concat [ be16 3u; be16 0u; be16 0u; be16 0u; be16 0u ]
      6, 4, Array.concat [ be16 4u; be32 0u; be24 0u ]
      6, 5, Array.concat [ be16 5u; be32 0u; be32 0u; be32 0u; be32 0u; be24 0u ]
      7, 1, Array.concat [ be16 1u; be16 1u; be32 8u; single1 ]
      8, 1, Array.concat [ be16 1u; be16 0u; be16 0u; be16 0u; be16 0u ]
      8, 2, Array.concat [ be16 2u; be32 0u; be16 0u; be16 0u; be24 0u ] ]

/// <summary>The minimal subtable bytes of every GPOS (lookup type, format) pair, 16- and 24-bit, that a real font can carry.</summary>
let private gposPairs : (int * int * byte array) list =
    [ 1, 1, Array.concat [ be16 1u; be16 0u; be16 0u ]
      1, 2, Array.concat [ be16 2u; be16 0u; be16 0u; be16 0u ]
      1, 3, Array.concat [ be16 3u; be32 0u; be16 0u ]
      1, 4, Array.concat [ be16 4u; be32 0u; be16 0u; be24 0u ]
      2, 1, Array.concat [ be16 1u; be16 0u; be16 0u; be16 0u; be16 0u ]
      2, 2, Array.concat [ be16 2u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u ]
      2, 3, Array.concat [ be16 3u; be32 0u; be16 0u; be16 0u; be24 0u ]
      2, 4, Array.concat [ be16 4u; be32 0u; be16 0u; be16 0u; be32 0u; be32 0u; be16 0u; be16 0u ]
      3, 1, Array.concat [ be16 1u; be16 0u; be16 0u ]
      3, 2, Array.concat [ be16 2u; be32 0u; be24 0u ]
      4, 1, Array.concat [ be16 1u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u ]
      4, 2, Array.concat [ be16 2u; be32 0u; be32 0u; be16 0u; be32 0u; be32 0u ]
      5, 1, Array.concat [ be16 1u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u ]
      5, 2, Array.concat [ be16 2u; be32 0u; be32 0u; be16 0u; be32 0u; be32 0u ]
      6, 1, Array.concat [ be16 1u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u ]
      6, 2, Array.concat [ be16 2u; be32 0u; be32 0u; be16 0u; be32 0u; be32 0u ]
      7, 1, Array.concat [ be16 1u; be16 0u; be16 0u ]
      7, 2, Array.concat [ be16 2u; be16 0u; be16 0u; be16 0u ]
      7, 3, Array.concat [ be16 3u; be16 0u; be16 0u ]
      7, 4, Array.concat [ be16 4u; be32 0u; be24 0u ]
      7, 5, Array.concat [ be16 5u; be32 0u; be32 0u; be24 0u ]
      7, 6, Array.concat [ be16 6u; be16 0u; be16 0u ]
      8, 1, Array.concat [ be16 1u; be16 0u; be16 0u ]
      8, 2, Array.concat [ be16 2u; be16 0u; be16 0u; be16 0u; be16 0u; be16 0u ]
      8, 3, Array.concat [ be16 3u; be16 0u; be16 0u; be16 0u; be16 0u ]
      8, 4, Array.concat [ be16 4u; be32 0u; be24 0u ]
      8, 5, Array.concat [ be16 5u; be32 0u; be32 0u; be32 0u; be32 0u; be24 0u ]
      9, 1, Array.concat [ be16 1u; be16 1u; be32 8u; Array.concat [ be16 1u; be16 0u; be16 0u ] ] ]

/// <summary>The P5.S15 exit-criteria test: every known (lookup type, format) pair parses to a real case, none to Pending.</summary>
[<Tests>]
let s15Tests =
    testList "P5.S15" [
        test "every known lookup type and format parses to a real case" {
            for (lookupType, format, bytes) in gsubPairs do
                match GsubTables.parse (ReadOnlySpan bytes) 0 lookupType with
                | SubstPending (t, f, _, _) -> failtest $"GSUB type {lookupType} format {format} parses as pending (type {t} format {f})"
                | _ -> ()
            for (lookupType, format, bytes) in gposPairs do
                match GposTables.parse (ReadOnlySpan bytes) 0 lookupType with
                | PosPending (t, f, _, _) -> failtest $"GPOS type {lookupType} format {format} parses as pending (type {t} format {f})"
                | _ -> ()
        }
    ]

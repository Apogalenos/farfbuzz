module Farfbuzz.Diff.LayoutTableDiff

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Oracle
open Farfbuzz.Diff.Differential
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts
open Farfbuzz.Diff.LayoutGens

/// <summary>The GSUB table tag.</summary>
let private gsubTag = Tag.make 'G' 'S' 'U' 'B'

/// <summary>The GPOS table tag.</summary>
let private gposTag = Tag.make 'G' 'P' 'O' 'S'

/// <summary>Builds a Farfbuzz face from a test font.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) =
    Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Builds a Farfbuzz OtFace for a test font.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The OpenType face.</returns>
let private otFaceOf (font: TestFont) (index: uint32) =
    OtFace.create (faceOf font index)

/// <summary>Spells the tags of a table as uint32.</summary>
/// <param name="tags">The tags.</param>
/// <returns>The uint32 values.</returns>
let private tagValues (tags: ImmutableArray<Tag>) : uint32 list =
    tags |> Seq.map uint32 |> Seq.toList

/// <summary>Reads a script-tag list through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag.</param>
/// <returns>The tags.</returns>
let private oracleScriptTags (face: nativeint) (tableTag: uint32) : uint32 list =
    let mutable count = 0u
    let total = Native.hb_ot_layout_table_get_script_tags(face, tableTag, 0u, &count, Array.empty)
    let tags = Array.zeroCreate (int total)
    let mutable written = uint32 tags.Length
    Native.hb_ot_layout_table_get_script_tags(face, tableTag, 0u, &written, tags) |> ignore
    tags |> Array.toList

/// <summary>Reads a table feature-tag list through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag.</param>
/// <returns>The tags.</returns>
let private oracleTableFeatureTags (face: nativeint) (tableTag: uint32) : uint32 list =
    let mutable count = 0u
    let total = Native.hb_ot_layout_table_get_feature_tags(face, tableTag, 0u, &count, Array.empty)
    let tags = Array.zeroCreate (int total)
    let mutable written = uint32 tags.Length
    Native.hb_ot_layout_table_get_feature_tags(face, tableTag, 0u, &written, tags) |> ignore
    tags |> Array.toList

/// <summary>Reads a script's language-tag list through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="scriptIndex">The script index.</param>
/// <returns>The tags.</returns>
let private oracleLanguageTags (face: nativeint) (tableTag: uint32) (scriptIndex: uint32) : uint32 list =
    let mutable count = 0u
    let total = Native.hb_ot_layout_script_get_language_tags(face, tableTag, scriptIndex, 0u, &count, Array.empty)
    let tags = Array.zeroCreate (int total)
    let mutable written = uint32 tags.Length
    Native.hb_ot_layout_script_get_language_tags(face, tableTag, scriptIndex, 0u, &written, tags) |> ignore
    tags |> Array.toList

/// <summary>Reads a language system's feature-index list through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="scriptIndex">The script index.</param>
/// <param name="languageIndex">The language index.</param>
/// <returns>The indices.</returns>
let private oracleFeatureIndexes (face: nativeint) (tableTag: uint32) (scriptIndex: uint32) (languageIndex: uint32) : uint32 list =
    let mutable count = 0u
    let total = Native.hb_ot_layout_language_get_feature_indexes(face, tableTag, scriptIndex, languageIndex, 0u, &count, Array.empty)
    let indexes = Array.zeroCreate (int total)
    let mutable written = uint32 indexes.Length
    Native.hb_ot_layout_language_get_feature_indexes(face, tableTag, scriptIndex, languageIndex, 0u, &written, indexes) |> ignore
    indexes |> Array.toList

/// <summary>Reads a language system's feature-tag list through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="scriptIndex">The script index.</param>
/// <param name="languageIndex">The language index.</param>
/// <returns>The tags.</returns>
let private oracleLanguageFeatureTags (face: nativeint) (tableTag: uint32) (scriptIndex: uint32) (languageIndex: uint32) : uint32 list =
    let mutable count = 0u
    let total = Native.hb_ot_layout_language_get_feature_tags(face, tableTag, scriptIndex, languageIndex, 0u, &count, Array.empty)
    let tags = Array.zeroCreate (int total)
    let mutable written = uint32 tags.Length
    Native.hb_ot_layout_language_get_feature_tags(face, tableTag, scriptIndex, languageIndex, 0u, &written, tags) |> ignore
    tags |> Array.toList

/// <summary>Reads a feature's lookup-index list through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="featureIndex">The feature index.</param>
/// <returns>The indices.</returns>
let private oracleFeatureLookups (face: nativeint) (tableTag: uint32) (featureIndex: uint32) : uint32 list =
    let mutable count = 0u
    let total = Native.hb_ot_layout_feature_get_lookups(face, tableTag, featureIndex, 0u, &count, Array.empty)
    let indexes = Array.zeroCreate (int total)
    let mutable written = uint32 indexes.Length
    Native.hb_ot_layout_feature_get_lookups(face, tableTag, featureIndex, 0u, &written, indexes) |> ignore
    indexes |> Array.toList

/// <summary>Selects a script through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="scriptTags">The candidate tags.</param>
/// <returns>Whether a candidate matched, the index and the chosen tag.</returns>
let private oracleSelectScript (face: nativeint) (tableTag: uint32) (scriptTags: uint32 array) : struct (bool * int * uint32) =
    let mutable scriptIndex = 0u
    let mutable chosenScript = 0u
    let found = Native.hb_ot_layout_table_select_script(face, tableTag, uint32 scriptTags.Length, scriptTags, &scriptIndex, &chosenScript)
    struct (found <> 0, int scriptIndex, chosenScript)

/// <summary>Selects a language through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="scriptIndex">The script index.</param>
/// <param name="languageTags">The candidate tags.</param>
/// <returns>Whether a candidate matched, the index and the chosen tag.</returns>
let private oracleSelectLanguage (face: nativeint) (tableTag: uint32) (scriptIndex: uint32) (languageTags: uint32 array) : struct (bool * int * uint32) =
    let mutable languageIndex = 0u
    let mutable chosenLanguage = 0u
    let found = Native.hb_ot_layout_script_select_language2(face, tableTag, scriptIndex, uint32 languageTags.Length, languageTags, &languageIndex, &chosenLanguage)
    struct (found <> 0, int languageIndex, chosenLanguage)

/// <summary>Reads a language system's required feature through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="scriptIndex">The script index.</param>
/// <param name="languageIndex">The language index.</param>
/// <returns>Whether present, the index and the tag.</returns>
let private oracleRequiredFeature (face: nativeint) (tableTag: uint32) (scriptIndex: uint32) (languageIndex: uint32) : struct (bool * uint32 * uint32) =
    let mutable featureIndex = 0u
    let mutable featureTag = 0u
    let found = Native.hb_ot_layout_language_get_required_feature(face, tableTag, scriptIndex, languageIndex, &featureIndex, &featureTag)
    struct (found <> 0, featureIndex, featureTag)

/// <summary>Finds a feature at table level through the oracle.</summary>
/// <param name="face">The face handle.</param>
/// <param name="tableTag">The table tag.</param>
/// <param name="featureTag">The feature tag.</param>
/// <returns>Whether found and the index.</returns>
let private oracleFindFeature (face: nativeint) (tableTag: uint32) (featureTag: uint32) : struct (bool * int) =
    let mutable featureIndex = 0u
    let found = Shims.farfbuzz_oracle_table_find_feature(face, tableTag, featureTag, &featureIndex)
    struct (found <> 0, int featureIndex)

/// <summary>Converts a Farfbuzz selection to the oracle's uint32 shape.</summary>
/// <param name="selection">The selection.</param>
/// <returns>The shape.</returns>
let private selectShape (selection: struct (bool * int * Tag)) : struct (bool * int * uint32) =
    let struct (found, index, tag) = selection
    struct (found, index, uint32 tag)

/// <summary>Whether the oracle accepts a table as present, <c>has_substitution</c> or <c>has_positioning</c>.</summary>
/// <param name="oracleFace">The oracle face.</param>
/// <param name="table">The table tag.</param>
/// <returns>True when the table has data.</returns>
let private oracleValidTable (oracleFace: nativeint) (table: Tag) : bool =
    match table = gsubTag with
    | true -> Native.hb_ot_layout_has_substitution oracleFace <> 0
    | false -> Native.hb_ot_layout_has_positioning oracleFace <> 0

/// <summary>Every face with a GSUB or GPOS table, paired with the table tag.</summary>
/// <returns>The faces.</returns>
let private tableFaces () =
    OracleFaces.all ()
    |> Seq.collect (fun struct (font, index) ->
        let face = faceOf font index
        let oracleFace = OracleFaces.face font index
        seq {
            match (Face.referenceTable face gsubTag).IsEmpty with
            | false when oracleValidTable oracleFace gsubTag -> yield struct (font, index, gsubTag)
            | _ -> ()
            match (Face.referenceTable face gposTag).IsEmpty with
            | false when oracleValidTable oracleFace gposTag -> yield struct (font, index, gposTag)
            | _ -> ()
        })

/// <summary>Every script index of every table face, including one past the end.</summary>
/// <returns>The face, table and script-index tuples.</returns>
let private scriptInputs () =
    tableFaces ()
    |> Seq.collect (fun struct (font, index, table) ->
        let otFace = otFaceOf font index
        let scriptCount = LayoutApi.scriptTags otFace table |> Seq.length
        seq { for scriptIndex in 0 .. scriptCount -> struct (font, index, table, scriptIndex) })

/// <summary>Every script and language index of every table face, plus the default-language index.</summary>
/// <returns>The face, table, script and language tuples.</returns>
let private scriptLangInputs () =
    scriptInputs ()
    |> Seq.collect (fun struct (font, index, table, scriptIndex) ->
        let otFace = otFaceOf font index
        let languageCount = LayoutApi.languageTags otFace table scriptIndex |> Seq.length
        seq {
            for languageIndex in 0 .. languageCount - 1 -> struct (font, index, table, scriptIndex, languageIndex)
            yield struct (font, index, table, scriptIndex, LayoutTable.DefaultLanguageIndex)
        })

/// <summary>Every feature index of every table face.</summary>
/// <returns>The face, table and feature-index tuples.</returns>
let private featureInputs () =
    tableFaces ()
    |> Seq.collect (fun struct (font, index, table) ->
        let otFace = otFaceOf font index
        let featureCount = LayoutApi.tableFeatureTags otFace table |> Seq.length
        seq { for featureIndex in 0 .. featureCount - 1 -> struct (font, index, table, featureIndex) })

/// <summary>Five tags unlikely to be present, for the find-feature miss path.</summary>
let private absentTags () =
    [ Tag.make 'z' 'z' 'z' 'z'; Tag.make 'y' 'y' 'y' 'y'; Tag.make 'x' 'x' 'x' 'x'; Tag.make 'w' 'w' 'w' 'w'; Tag.make 'v' 'v' 'v' 'v' ]

/// <summary>Whether a face's table is absent or accepted by HarfBuzz; invalid subtables are deferred to S06.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="table">The table tag.</param>
/// <returns>True when the table is absent or valid.</returns>
let private testableTable (font: TestFont) (index: uint32) (table: Tag) : bool =
    let face = faceOf font index
    match (Face.referenceTable face table).IsEmpty with
    | true -> true
    | false -> oracleValidTable (OracleFaces.face font index) table

/// <summary>Compares every S03 query for mutated file bytes, returning the first mismatch.</summary>
/// <param name="bytes">The mutated file bytes.</param>
/// <param name="index">The face index.</param>
/// <param name="table">The table tag.</param>
/// <returns>The mismatch description, or none.</returns>
let private compareMutated (bytes: byte array) (index: uint32) (table: Tag) : string voption =
    let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
    let oracleFace = Native.hb_face_create(blob, index)
    let face = Face.create (ReadOnlyMemory bytes) index
    let otFace = OtFace.create face
    let tableTag = uint32 table
    let result =
        match LayoutApi.scriptTags otFace table |> tagValues with
        | ours when ours <> oracleScriptTags oracleFace tableTag -> ValueSome "script tags"
        | _ ->
            match LayoutApi.tableFeatureTags otFace table |> tagValues with
            | ours when ours <> oracleTableFeatureTags oracleFace tableTag -> ValueSome "table feature tags"
            | _ ->
                let scriptCount = LayoutApi.scriptTags otFace table |> Seq.length
                let scriptMismatch =
                    seq { for scriptIndex in 0 .. scriptCount - 1 -> scriptIndex }
                    |> Seq.tryPick (fun scriptIndex ->
                        match LayoutApi.languageTags otFace table scriptIndex |> tagValues with
                        | ours when ours <> oracleLanguageTags oracleFace tableTag (uint32 scriptIndex) -> Some $"language tags of script {scriptIndex}"
                        | _ ->
                            let languageCount = LayoutApi.languageTags otFace table scriptIndex |> Seq.length
                            seq { yield! [ 0 .. languageCount - 1 ]; yield LayoutTable.DefaultLanguageIndex }
                            |> Seq.tryPick (fun languageIndex ->
                                let struct (requiredFound, requiredIndex, requiredTag) = LayoutApi.requiredFeature otFace table scriptIndex languageIndex
                                let requiredOurs = struct (requiredFound, requiredIndex, uint32 requiredTag)
                                match requiredOurs = oracleRequiredFeature oracleFace tableTag (uint32 scriptIndex) (uint32 languageIndex) with
                                | false -> Some $"required feature of script {scriptIndex} language {languageIndex}"
                                | true ->
                                    match LayoutApi.featureIndexes otFace table scriptIndex languageIndex |> Seq.map uint32 |> Seq.toList with
                                    | ours when ours <> oracleFeatureIndexes oracleFace tableTag (uint32 scriptIndex) (uint32 languageIndex) -> Some $"feature indexes of script {scriptIndex} language {languageIndex}"
                                    | _ ->
                                        match LayoutApi.featureTags otFace table scriptIndex languageIndex |> tagValues with
                                        | ours when ours <> oracleLanguageFeatureTags oracleFace tableTag (uint32 scriptIndex) (uint32 languageIndex) -> Some $"feature tags of script {scriptIndex} language {languageIndex}"
                                        | _ -> None))
                match scriptMismatch with
                | Some description -> ValueSome description
                | None ->
                    let featureCount = LayoutApi.tableFeatureTags otFace table |> Seq.length
                    let featureMismatch =
                        seq { for featureIndex in 0 .. featureCount - 1 -> featureIndex }
                        |> Seq.tryPick (fun featureIndex ->
                            match LayoutApi.featureLookups otFace table featureIndex |> Seq.map uint32 |> Seq.toList with
                            | ours when ours <> oracleFeatureLookups oracleFace tableTag (uint32 featureIndex) -> Some $"feature lookups of feature {featureIndex}"
                            | _ -> None)
                    match featureMismatch with
                    | Some description -> ValueSome description
                    | None ->
                        match LayoutApi.lookupCount otFace table = int (Native.hb_ot_layout_table_get_lookup_count(oracleFace, tableTag)) with
                        | false -> ValueSome "lookup count"
                        | true ->
                            let presenceOk =
                                match table = gsubTag with
                                | true -> LayoutApi.hasSubstitution otFace = (Native.hb_ot_layout_has_substitution oracleFace <> 0)
                                | false -> LayoutApi.hasPositioning otFace = (Native.hb_ot_layout_has_positioning oracleFace <> 0)
                            match presenceOk with
                            | true -> ValueNone
                            | false -> ValueSome "table presence"
    Native.hb_face_destroy oracleFace
    Native.hb_blob_destroy blob
    result

/// <summary>The P5.S03 GSUB/GPOS table differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    testList "P5.S03" [
        exhaustive "script tags" (tableFaces ())
            (fun struct (font, index, table) -> LayoutApi.scriptTags (otFaceOf font index) table |> tagValues)
            (fun struct (font, index, table) -> oracleScriptTags (OracleFaces.face font index) (uint32 table))
        exhaustive "select script" (tableFaces () |> Seq.collect (fun struct (font, index, table) ->
            LayoutGens.scriptsAndLanguages () |> Seq.map (fun sl -> struct (font, index, table, sl))))
            (fun struct (font, index, table, sl) -> selectShape (LayoutApi.selectScript (otFaceOf font index) table sl.ScriptTags))
            (fun struct (font, index, table, sl) -> oracleSelectScript (OracleFaces.face font index) (uint32 table) (sl.ScriptTags |> Seq.map uint32 |> Seq.toArray))
        exhaustive "language tags" (scriptInputs ())
            (fun struct (font, index, table, scriptIndex) -> LayoutApi.languageTags (otFaceOf font index) table scriptIndex |> tagValues)
            (fun struct (font, index, table, scriptIndex) -> oracleLanguageTags (OracleFaces.face font index) (uint32 table) (uint32 scriptIndex))
        exhaustive "select language" (scriptInputs () |> Seq.collect (fun struct (font, index, table, scriptIndex) ->
            LayoutGens.scriptsAndLanguages () |> Seq.map (fun sl -> struct (font, index, table, scriptIndex, sl.LanguageTags))))
            (fun struct (font, index, table, scriptIndex, languageTags) -> selectShape (LayoutApi.selectLanguage (otFaceOf font index) table scriptIndex languageTags))
            (fun struct (font, index, table, scriptIndex, languageTags) -> oracleSelectLanguage (OracleFaces.face font index) (uint32 table) (uint32 scriptIndex) (languageTags |> Seq.map uint32 |> Seq.toArray))
        exhaustive "required feature" (scriptLangInputs ())
            (fun struct (font, index, table, scriptIndex, languageIndex) ->
                let struct (found, featureIndex, featureTag) = LayoutApi.requiredFeature (otFaceOf font index) table scriptIndex languageIndex
                struct (found, featureIndex, uint32 featureTag))
            (fun struct (font, index, table, scriptIndex, languageIndex) -> oracleRequiredFeature (OracleFaces.face font index) (uint32 table) (uint32 scriptIndex) (uint32 languageIndex))
        exhaustive "language system feature indexes" (scriptLangInputs ())
            (fun struct (font, index, table, scriptIndex, languageIndex) -> LayoutApi.featureIndexes (otFaceOf font index) table scriptIndex languageIndex |> Seq.map uint32 |> Seq.toList)
            (fun struct (font, index, table, scriptIndex, languageIndex) -> oracleFeatureIndexes (OracleFaces.face font index) (uint32 table) (uint32 scriptIndex) (uint32 languageIndex))
        exhaustive "language system feature tags" (scriptLangInputs ())
            (fun struct (font, index, table, scriptIndex, languageIndex) -> LayoutApi.featureTags (otFaceOf font index) table scriptIndex languageIndex |> tagValues)
            (fun struct (font, index, table, scriptIndex, languageIndex) -> oracleLanguageFeatureTags (OracleFaces.face font index) (uint32 table) (uint32 scriptIndex) (uint32 languageIndex))
        exhaustive "table feature tags" (tableFaces ())
            (fun struct (font, index, table) -> LayoutApi.tableFeatureTags (otFaceOf font index) table |> tagValues)
            (fun struct (font, index, table) -> oracleTableFeatureTags (OracleFaces.face font index) (uint32 table))
        exhaustive "find feature" (tableFaces () |> Seq.collect (fun struct (font, index, table) ->
            let otFace = otFaceOf font index
            seq {
                yield! LayoutApi.tableFeatureTags otFace table |> Seq.map (fun tag -> struct (font, index, table, tag))
                yield! absentTags () |> Seq.map (fun tag -> struct (font, index, table, tag))
            }))
            (fun struct (font, index, table, featureTag) ->
                let struct (found, featureIndex) = LayoutApi.findFeature (otFaceOf font index) table featureTag
                struct (found, featureIndex))
            (fun struct (font, index, table, featureTag) -> oracleFindFeature (OracleFaces.face font index) (uint32 table) (uint32 featureTag))
        exhaustive "feature lookups" (featureInputs ())
            (fun struct (font, index, table, featureIndex) -> LayoutApi.featureLookups (otFaceOf font index) table featureIndex |> Seq.map uint32 |> Seq.toList)
            (fun struct (font, index, table, featureIndex) -> oracleFeatureLookups (OracleFaces.face font index) (uint32 table) (uint32 featureIndex))
        exhaustive "lookup count and table presence" (OracleFaces.all () |> Seq.filter (fun struct (font, index) ->
            testableTable font index gsubTag && testableTable font index gposTag))
            (fun struct (font, index) ->
                let face = otFaceOf font index
                struct (LayoutApi.lookupCount face gsubTag, LayoutApi.lookupCount face gposTag, LayoutApi.hasSubstitution face, LayoutApi.hasPositioning face))
            (fun struct (font, index) ->
                let face = OracleFaces.face font index
                struct (int (Native.hb_ot_layout_table_get_lookup_count(face, uint32 gsubTag)), int (Native.hb_ot_layout_table_get_lookup_count(face, uint32 gposTag)), Native.hb_ot_layout_has_substitution face <> 0, Native.hb_ot_layout_has_positioning face <> 0))
        testCase "P5.S03 mutated GSUB and GPOS tables" (fun () ->
            let failures = ResizeArray<string>()
            let mutable mutations = 0
            for struct (font, index) in OracleFaces.all () do
                for table in [ gsubTag; gposTag ] do
                    let face = faceOf font index
                    match Face.tryFindTable face table with
                    | ValueNone -> ()
                    | ValueSome _ when not (oracleValidTable (OracleFaces.face font index) table) -> ()
                    | ValueSome record ->
                        let bytes = OracleFaces.data font.Path
                        let baseOffset = int (face.BaseOffset + record.Offset)
                        let version = (uint32 bytes[baseOffset] <<< 24) ||| (uint32 bytes[baseOffset + 1] <<< 16) ||| (uint32 bytes[baseOffset + 2] <<< 8) ||| uint32 bytes[baseOffset + 3]
                        let v12 = version >= 0x00010002u
                        for fieldOffset, width, present in [ 4, 2, true; 6, 2, true; 14, 4, v12; 18, 4, v12 ] do
                            match present with
                            | false -> ()
                            | true ->
                                let at = baseOffset + fieldOffset
                                let readValue () =
                                    match width with
                                    | 2 -> (uint32 bytes[at] <<< 8) ||| uint32 bytes[at + 1]
                                    | _ -> (uint32 bytes[at] <<< 24) ||| (uint32 bytes[at + 1] <<< 16) ||| (uint32 bytes[at + 2] <<< 8) ||| uint32 bytes[at + 3]
                                let writeValue (target: byte array) (value: uint32) =
                                    match width with
                                    | 2 ->
                                        target[at] <- byte (value >>> 8)
                                        target[at + 1] <- byte value
                                    | _ ->
                                        target[at] <- byte (value >>> 24)
                                        target[at + 1] <- byte (value >>> 16)
                                        target[at + 2] <- byte (value >>> 8)
                                        target[at + 3] <- byte value
                                let value = readValue ()
                                let maxValue = if width = 2 then 0xFFFFu else 0xFFFFFFFFu
                                for mutated in [ 0u; 1u; value - 1u; value + 1u; maxValue ] do
                                    let candidate = Array.copy bytes
                                    writeValue candidate mutated
                                    match compareMutated candidate index table with
                                    | ValueNone -> ()
                                    | ValueSome description -> failures.Add $"{font.Suite}/{System.IO.Path.GetFileName font.Path} face {index} {Tag.toString table} field {fieldOffset} (value {value} -> {mutated}): {description}"
                                    mutations <- mutations + 1
            printfn "P5.S03 compared %d mutated GSUB/GPOS tables" mutations
            Expect.isEmpty failures ("mutated GSUB/GPOS differences:\n" + String.concat "\n" failures))
    ]

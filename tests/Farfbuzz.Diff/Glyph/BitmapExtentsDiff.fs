module Farfbuzz.Diff.BitmapExtentsDiff

open System
open System.Collections.Immutable
open System.IO
open Expecto
open Farfbuzz
open Farfbuzz.OpenType
open Farfbuzz.Oracle
open Farfbuzz.Harness
open Farfbuzz.Diff.OracleFaces
open Farfbuzz.Diff.TestFonts

/// <summary>Creates a Farfbuzz face from a test font's bytes.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <returns>The face.</returns>
let private faceOf (font: TestFont) (index: uint32) = Face.create (ReadOnlyMemory (OracleFaces.data font.Path)) index

/// <summary>Farfbuzz's extents as a comparable tuple; the sentinel marks a not-ported source.</summary>
/// <param name="font">The font.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The extents.</returns>
let private oursExtents (font: Font) (glyph: uint32) : struct (bool * int * int * int * int) =
    match Extents.glyph font glyph with
    | Ok ValueNone -> struct (false, 0, 0, 0, 0)
    | Ok (ValueSome extents) -> struct (true, extents.XBearing, extents.YBearing, extents.Width, extents.Height)
    | Error _ -> struct (false, -1, -1, -1, -1)

/// <summary>Builds the Farfbuzz font for a face at a ppem, scale and synthetic state.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="ppem">The horizontal and vertical ppem.</param>
/// <param name="scale">The horizontal and vertical scale.</param>
/// <param name="embolden">The synthetic bold strength.</param>
/// <param name="slant">The synthetic slant.</param>
/// <returns>The font.</returns>
let private oursFontAt (font: TestFont) (index: uint32) (ppem: uint32) (scale: int) (embolden: float32) (slant: float32) : Font =
    let face = faceOf font index
    let scaled = Font.withScale scale scale (Font.withPpem ppem ppem (Font.create face))
    let bolded = match embolden <> 0f with | true -> Font.withSyntheticBold embolden embolden false scaled | false -> scaled
    match slant <> 0f with
    | true -> Font.withSyntheticSlant slant bolded
    | false -> bolded

/// <summary>Creates an oracle font at a ppem, scale and synthetic state.</summary>
/// <param name="font">The test font.</param>
/// <param name="index">The face index.</param>
/// <param name="ppem">The horizontal and vertical ppem.</param>
/// <param name="scale">The horizontal and vertical scale.</param>
/// <param name="embolden">The synthetic bold strength.</param>
/// <param name="slant">The synthetic slant.</param>
/// <returns>The font handle.</returns>
let private oracleFontAt (font: TestFont) (index: uint32) (ppem: uint32) (scale: int) (embolden: float32) (slant: float32) : nativeint =
    let face = OracleFaces.face font index
    let handle = Native.hb_font_create face
    Native.hb_ot_font_set_funcs handle
    Native.hb_font_set_scale(handle, scale, scale)
    Native.hb_font_set_ppem(handle, ppem, ppem)
    match embolden <> 0f with | true -> Native.hb_font_set_synthetic_bold(handle, embolden, embolden, 0) | false -> ()
    match slant <> 0f with | true -> Native.hb_font_set_synthetic_slant(handle, slant) | false -> ()
    handle

/// <summary>Reads one glyph's extents through the oracle.</summary>
/// <param name="handle">The oracle font.</param>
/// <param name="glyph">The glyph id.</param>
/// <returns>The extents.</returns>
let private oracleExtent (handle: nativeint) (glyph: uint32) : struct (bool * int * int * int * int) =
    let mutable extents = Unchecked.defaultof<Native.OracleGlyphExtents>
    let present = Native.hb_font_get_glyph_extents(handle, glyph, &extents)
    struct (present <> 0, extents.XBearing, extents.YBearing, extents.Width, extents.Height)

/// <summary>Whether a Farfbuzz answer agrees with an oracle answer: a not-ported answer agrees with a present one.</summary>
/// <param name="ours">Farfbuzz's answer.</param>
/// <param name="oracle">The oracle's answer.</param>
/// <returns>True when they agree.</returns>
let private agrees (ours: struct (bool * int * int * int * int)) (oracle: struct (bool * int * int * int * int)) =
    match ours, oracle with
    | struct (false, -1, _, _, _), struct (true, _, _, _, _) -> true
    | _ -> ours = oracle

/// <summary>The ppem probes of a strike's ppem: the value, one below and one above.</summary>
/// <param name="ppem">The strike ppem.</param>
/// <returns>The probes.</returns>
let private strikeProbes (ppem: uint32) : uint32 array =
    match ppem with
    | 0u -> [| 0u; 1u |]
    | _ -> [| ppem - 1u; ppem; ppem + 1u |]

/// <summary>The ppem probes of a face: 0, 1, 300, 65535 and each strike's ppem with its neighbours.</summary>
/// <param name="strikes">The strikes, as ppem and offset pairs.</param>
/// <returns>The probes, distinct.</returns>
let private ppemProbes (strikes: ImmutableArray<struct (int * int)>) : uint32 array =
    let strikeValues = strikes |> Seq.collect (fun struct (ppem, _) -> strikeProbes (uint32 ppem)) |> Seq.toArray
    Array.append [| 0u; 1u; 300u; 65535u |] strikeValues |> Array.distinct

/// <summary>The extra bitmap fonts under <c>test/api/fonts/</c>.</summary>
/// <returns>The faces.</returns>
let private extraFaces () : ImmutableArray<TestFont> =
    let apiFonts = Path.Combine(RepositoryPaths.upstream (), "test", "api", "fonts")
    let noto = Directory.EnumerateFiles(apiFonts, "NotoColorEmoji*.ttf") |> Seq.toList
    let relative = [ "api/fonts/sbix.ttf"; "api/fonts/sbix_X.ttf"; "api/fonts/chromacheck-sbix.ttf"; "api/fonts/chromacheck-cbdt.ttf" ]
    let notoRelative = noto |> List.map (fun path -> "api/fonts/" + Path.GetFileName path)
    TestFonts.extra (relative @ notoRelative)

/// <summary>Runs the extents comparison for one kind of bitmap table over its faces.</summary>
/// <param name="name">The test name.</param>
/// <param name="faces">The bitmap faces.</param>
/// <param name="strikesOf">Reads a face's strikes.</param>
/// <returns>The test.</returns>
let private extentsTest (name: string) (faces: struct (TestFont * uint32) array) (strikesOf: Face -> ImmutableArray<struct (int * int)>) =
    testCase name (fun () ->
        let failures = ResizeArray<string>()
        let mutable comparisons = 0
        for struct (font, index) in faces do
            let face = faceOf font index
            let upem = face.Upem
            let glyphCount = face.GlyphCount
            let ppems = ppemProbes (strikesOf face)
            let scales = [| upem; 3 * upem + 7; -upem |]
            let synthetics = [| 0f, 0f; 0.05f, 0.2f |]
            for ppem in ppems do
                for scale in scales do
                    for embolden, slant in synthetics do
                        let oursFont = oursFontAt font index ppem scale embolden slant
                        let oracleFont = oracleFontAt font index ppem scale embolden slant
                        for glyph in 0u .. uint32 glyphCount do
                            let ours = oursExtents oursFont glyph
                            let expected = oracleExtent oracleFont glyph
                            comparisons <- comparisons + 1
                            match agrees ours expected with
                            | true -> ()
                            | false -> failures.Add($"%s{font.Path}#%d{index} glyph {glyph} ppem {ppem} scale {scale} embolden %A{embolden} slant %A{slant}: oracle %A{expected}, farfbuzz %A{ours}")
                        Native.hb_font_destroy oracleFont
        Console.WriteLine $"{name}: {comparisons} comparisons"
        Expect.isEmpty failures ("first mismatches:\n" + String.concat "\n" (Seq.truncate 20 failures)))

/// <summary>Encodes a uint16 big-endian.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint16) : byte array = [| byte (uint32 value >>> 8); byte value |]

/// <summary>Encodes a uint32 big-endian.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be32 (value: uint32) : byte array =
    [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>The replacement uint16 values for one mutated count or index field.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements.</returns>
let private u16Replacements (value: uint16) : uint16 array =
    [| 0us; 1us; uint16 (int value - 1); uint16 (int value + 1); 0xFFFFus |] |> Array.distinct

/// <summary>The replacement uint32 values for one mutated offset field.</summary>
/// <param name="value">The original value.</param>
/// <returns>The replacements.</returns>
let private u32Replacements (value: uint32) : uint32 array =
    [| 0u; 1u; value - 1u; value + 1u; 0xFFFFFFFFu |] |> Array.distinct

/// <summary>One field mutation: the absolute byte offset, a label and the replacement bytes.</summary>
type private Mutation = { At: int; Name: string; Replacement: byte array }

/// <summary>The absolute file offset of a table.</summary>
/// <param name="face">The face.</param>
/// <param name="tag">The table tag.</param>
/// <returns>The offset, or none.</returns>
let private tableAbsAt (face: Face) (tag: Tag) : int voption =
    match Face.tryFindTable face tag with
    | ValueNone -> ValueNone
    | ValueSome record -> ValueSome (int (face.BaseOffset + record.Offset))

/// <summary>The absolute file offset of a table's length field in the directory.</summary>
/// <param name="face">The face.</param>
/// <param name="tag">The table tag.</param>
/// <returns>The offset, or none.</returns>
let private lengthFieldAt (face: Face) (tag: Tag) : int voption =
    match Face.tryFindTable face tag with
    | ValueNone -> ValueNone
    | ValueSome _ ->
        let index = face.Directory |> Seq.findIndex (fun r -> r.Tag = tag)
        ValueSome (int face.BaseOffset + 12 + index * 16 + 12)

/// <summary>The sbix table mutations of one face.</summary>
/// <param name="face">The face.</param>
/// <returns>The mutations.</returns>
let private sbixMutations (face: Face) : Mutation list =
    match tableAbsAt face Tags.Sbix with
    | ValueNone -> []
    | ValueSome sbixAt ->
        let table = Face.referenceTable face Tags.Sbix
        let span = table.Span
        let mutations = ResizeArray<Mutation>()
        for replacement in u16Replacements (BinaryCalls.u16 span 0) do
            mutations.Add { At = sbixAt; Name = "version"; Replacement = be16 replacement }
        let strikeCount = int (BinaryCalls.u32 span 4)
        for replacement in u32Replacements (BinaryCalls.u32 span 4) do
            mutations.Add { At = sbixAt + 4; Name = "strike count"; Replacement = be32 replacement }
        for strike in 0 .. strikeCount - 1 do
            let strikeOffset = int (BinaryCalls.u32 span (8 + strike * 4))
            for replacement in u32Replacements (BinaryCalls.u32 span (8 + strike * 4)) do
                mutations.Add { At = sbixAt + 8 + strike * 4; Name = $"strike {strike} offset"; Replacement = be32 replacement }
            match strikeOffset with
            | 0 -> ()
            | _ ->
                for replacement in u16Replacements (BinaryCalls.u16 span strikeOffset) do
                    mutations.Add { At = sbixAt + strikeOffset; Name = $"strike {strike} ppem"; Replacement = be16 replacement }
                for glyph in 0 .. face.GlyphCount - 1 do
                    let a = int (BinaryCalls.u32 span (strikeOffset + 4 + glyph * 4))
                    let b = int (BinaryCalls.u32 span (strikeOffset + 4 + (glyph + 1) * 4))
                    match b > a + 8 && b <= table.Length - strikeOffset with
                    | false -> ()
                    | true ->
                        let glyphAt = sbixAt + strikeOffset + a
                        for replacement in [| be32 (uint32 b); be32 0xFFFFFFFFu |] do
                            mutations.Add { At = sbixAt + strikeOffset + 4 + glyph * 4; Name = $"strike {strike} glyph {glyph} image offset"; Replacement = replacement }
                        for replacement in [| be32 0x706E6720u; be32 0x64757065u; be32 0x6A706720u; be32 0u |] do
                            mutations.Add { At = glyphAt + 4; Name = $"strike {strike} glyph {glyph} graphic type"; Replacement = replacement }
                        for replacement in u16Replacements (BinaryCalls.u16 span (strikeOffset + a + 8)) do
                            mutations.Add { At = glyphAt + 8; Name = $"strike {strike} glyph {glyph} dupe target"; Replacement = be16 replacement }
                        let pngAt = strikeOffset + a + 8
                        for replacement in [| 0u; 65536u; 0xFFFFFFFFu |] do
                            mutations.Add { At = sbixAt + pngAt + 16; Name = $"strike {strike} glyph {glyph} png width"; Replacement = be32 replacement }
                            mutations.Add { At = sbixAt + pngAt + 20; Name = $"strike {strike} glyph {glyph} png height"; Replacement = be32 replacement }
        match lengthFieldAt face Tags.Sbix with
        | ValueNone -> ()
        | ValueSome at ->
            let record = Face.tryFindTable face Tags.Sbix |> ValueOption.get
            for length in [| 0u; 20u; 21u; record.Length / 2u |] do
                if length < record.Length then mutations.Add { At = at; Name = "truncated tail"; Replacement = be32 length }
        mutations |> Seq.toList

/// <summary>The CBDT/CBLC table mutations of one face.</summary>
/// <param name="face">The face.</param>
/// <returns>The mutations.</returns>
let private cbdtMutations (face: Face) : Mutation list =
    match tableAbsAt face Tags.Cblc with
    | ValueNone -> []
    | ValueSome cblcAt ->
        let cblc = Face.referenceTable face Tags.Cblc
        let span = cblc.Span
        let mutations = ResizeArray<Mutation>()
        for replacement in u32Replacements (BinaryCalls.u32 span 4) do
            mutations.Add { At = cblcAt + 4; Name = "size table count"; Replacement = be32 replacement }
        let count = int (BinaryCalls.u32 span 4)
        for strike in 0 .. count - 1 do
            let sizeAt = 8 + strike * 48
            for replacement in u32Replacements (BinaryCalls.u32 span (sizeAt + 0)) do
                mutations.Add { At = cblcAt + sizeAt + 0; Name = $"strike {strike} indexSubtableArrayOffset"; Replacement = be32 replacement }
            for replacement in u32Replacements (BinaryCalls.u32 span (sizeAt + 8)) do
                mutations.Add { At = cblcAt + sizeAt + 8; Name = $"strike {strike} numberOfIndexSubtables"; Replacement = be32 replacement }
            let arrayOffset = int (BinaryCalls.u32 span (sizeAt + 0))
            let numTables = int (BinaryCalls.u32 span (sizeAt + 8))
            for record in 0 .. numTables - 1 do
                let recordAt = arrayOffset + record * 8
                match recordAt + 8 <= span.Length with
                | false -> ()
                | true ->
                    for replacement in u16Replacements (BinaryCalls.u16 span recordAt) do
                        mutations.Add { At = cblcAt + recordAt; Name = $"strike {strike} record {record} first glyph"; Replacement = be16 replacement }
                    for replacement in u16Replacements (BinaryCalls.u16 span (recordAt + 2)) do
                        mutations.Add { At = cblcAt + recordAt + 2; Name = $"strike {strike} record {record} last glyph"; Replacement = be16 replacement }
                    let subtableOffset = arrayOffset + int (BinaryCalls.u32 span (recordAt + 4))
                    match subtableOffset + 8 <= span.Length with
                    | false -> ()
                    | true ->
                        for replacement in u16Replacements (BinaryCalls.u16 span subtableOffset) do
                            mutations.Add { At = cblcAt + subtableOffset; Name = $"strike {strike} record {record} index format"; Replacement = be16 replacement }
                        for replacement in u16Replacements (BinaryCalls.u16 span (subtableOffset + 2)) do
                            mutations.Add { At = cblcAt + subtableOffset + 2; Name = $"strike {strike} record {record} image format"; Replacement = be16 replacement }
                        for replacement in u32Replacements (BinaryCalls.u32 span (subtableOffset + 8)) do
                            mutations.Add { At = cblcAt + subtableOffset + 8; Name = $"strike {strike} record {record} offset array entry"; Replacement = be32 replacement }
        match lengthFieldAt face Tags.Cblc with
        | ValueNone -> ()
        | ValueSome at ->
            let record = Face.tryFindTable face Tags.Cblc |> ValueOption.get
            for length in [| 0u; 20u; 21u; record.Length / 2u |] do
                if length < record.Length then mutations.Add { At = at; Name = "truncated tail"; Replacement = be32 length }
        mutations |> Seq.toList

/// <summary>Creates an oracle font for mutated bytes at scale = upem.</summary>
/// <param name="mutated">The mutated file bytes.</param>
/// <param name="index">The face index.</param>
/// <returns>The font handle.</returns>
let private mutatedOracleFont (mutated: byte array) (index: uint32) : nativeint =
    let blob = Native.hb_blob_create(mutated, uint32 mutated.Length, 0, 0n, 0n)
    let face = Native.hb_face_create(blob, index)
    let handle = Native.hb_font_create face
    Native.hb_ot_font_set_funcs handle
    let upem = int (Native.hb_face_get_upem face)
    Native.hb_font_set_scale(handle, upem, upem)
    Native.hb_face_destroy face
    Native.hb_blob_destroy blob
    handle

/// <summary>The sampled glyphs of a face.</summary>
/// <param name="glyphCount">The glyph count.</param>
/// <returns>The glyph ids.</returns>
let private sampleGlyphs (glyphCount: int) : uint32 array =
    [| 0u; 1u; 2u; 3u; uint32 glyphCount |]
    |> Array.filter (fun glyph -> int glyph <= glyphCount)
    |> Array.distinct

/// <summary>The mutated bitmap table differential, one field rewritten at a time.</summary>
/// <param name="faces">The bitmap faces.</param>
/// <param name="mutationsOf">Reads a face's mutations.</param>
/// <param name="name">The table name.</param>
/// <returns>The test.</returns>
let private mutatedTest (name: string) (faces: struct (TestFont * uint32) array) (mutationsOf: Face -> Mutation list) =
    testCase name (fun () ->
        let failures = ResizeArray<string>()
        let mutable mutatedTables = 0
        for struct (font, index) in faces do
            let bytes = OracleFaces.data font.Path
            let face = Face.create (ReadOnlyMemory bytes) index
            let glyphs = sampleGlyphs face.GlyphCount
            for mutation in mutationsOf face do
                mutatedTables <- mutatedTables + 1
                let mutated = Array.copy bytes
                for b in 0 .. mutation.Replacement.Length - 1 do
                    mutated[mutation.At + b] <- mutation.Replacement[b]
                let oursFace = Face.create (ReadOnlyMemory mutated) index
                let oursFont = Font.create oursFace
                let oracleFont = mutatedOracleFont mutated index
                for glyph in glyphs do
                    let ours = oursExtents oursFont glyph
                    let expected = oracleExtent oracleFont glyph
                    match agrees ours expected with
                    | true -> ()
                    | false -> failures.Add($"{font.Path}#{index} {name} {mutation.Name} glyph {glyph}: oracle %A{expected}, farfbuzz %A{ours}")
                Native.hb_font_destroy oracleFont
        Console.WriteLine $"{name}: {mutatedTables} mutated tables"
        Expect.isEmpty failures ("first mismatches:\n" + String.concat "\n" (Seq.truncate 20 failures)))

/// <summary>Runs the vertical-origin comparison over bitmap faces: the origin falls back to the glyph's extents on a
/// face without VORG or vmtx, and those extents are the bitmap's when sbix or CBDT answers
/// (<c>hb-ot-font.cc:770-793</c>).</summary>
/// <param name="name">The test name.</param>
/// <param name="faces">The bitmap faces.</param>
/// <param name="strikesOf">Reads a face's strikes.</param>
/// <returns>The test.</returns>
let private vOriginTest (name: string) (faces: struct (TestFont * uint32) array) (strikesOf: Face -> ImmutableArray<struct (int * int)>) =
    testCase name (fun () ->
        let failures = ResizeArray<string>()
        let mutable comparisons = 0
        let mutable notPorted = 0
        for struct (font, index) in faces do
            let face = faceOf font index
            let upem = face.Upem
            let glyphCount = face.GlyphCount
            for ppem in ppemProbes (strikesOf face) do
                for scale in [| upem; -upem |] do
                    for embolden, slant in [| 0f, 0f; 0.05f, 0.2f |] do
                        let oursFont = oursFontAt font index ppem scale embolden slant
                        let oracleFont = oracleFontAt font index ppem scale embolden slant
                        for glyph in 0u .. uint32 glyphCount do
                            let mutable x = 0
                            let mutable y = 0
                            let present = Native.hb_font_get_glyph_v_origin(oracleFont, glyph, &x, &y)
                            comparisons <- comparisons + 1
                            match Origins.vOrigin oursFont glyph with
                            | Error (OutlinesNotPorted _) -> notPorted <- notPorted + 1
                            | Error other -> failures.Add($"%s{font.Path}#%d{index} glyph {glyph} ppem {ppem} scale {scale}: farfbuzz %A{other}")
                            | Ok (struct (oursX, oursY)) ->
                                match present <> 0 && oursX = x && oursY = y with
                                | true -> ()
                                | false -> failures.Add($"%s{font.Path}#%d{index} glyph {glyph} ppem {ppem} scale {scale} embolden %A{embolden} slant %A{slant}: oracle ({x}, {y}) present {present}, farfbuzz ({oursX}, {oursY})")
                        Native.hb_font_destroy oracleFont
        Console.WriteLine $"{name}: {comparisons} comparisons, {notPorted} answered as outlines not ported"
        Expect.isLessThan notPorted comparisons "some vertical origins must be compared, not all reported as not ported"
        Expect.isEmpty failures ("first mismatches:\n" + String.concat "\n" (Seq.truncate 20 failures)))

/// <summary>The P7.S18 sbix and CBDT extents differentials.</summary>
/// <returns>The test list.</returns>
let tests () =
    let sbixFaces =
        OracleFaces.all ()
        |> Seq.append (extraFaces () |> Seq.map (fun font -> struct (font, uint32 font.Index)))
        |> Seq.filter (fun struct (font, index) -> Sbix.hasData (Sbix.load (faceOf font index)))
        |> Seq.toArray
    let cbdtFaces =
        OracleFaces.all ()
        |> Seq.append (extraFaces () |> Seq.map (fun font -> struct (font, uint32 font.Index)))
        |> Seq.filter (fun struct (font, index) -> (Face.referenceTable (faceOf font index) Tags.Cblc).Length > 0)
        |> Seq.toArray
    testList "P7.S18" [
        extentsTest "P7.S18 sbix extents" sbixFaces (fun face -> (Sbix.load face).Strikes)
        extentsTest "P7.S18 cbdt extents" cbdtFaces (fun face -> (Cbdt.load face).Strikes)
        mutatedTest "P7.S18 mutated sbix" sbixFaces sbixMutations
        mutatedTest "P7.S18 mutated cbdt" cbdtFaces cbdtMutations
        vOriginTest "P7.S18 sbix vertical origins" sbixFaces (fun face -> (Sbix.load face).Strikes)
        vOriginTest "P7.S18 cbdt vertical origins" cbdtFaces (fun face -> (Cbdt.load face).Strikes)
    ]

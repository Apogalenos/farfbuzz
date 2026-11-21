namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The Arabic fallback shaping of a font that has no joining or ligature lookups of its own,
/// <c>arabic_fallback_shape</c> (src/hb-ot-shaper-arabic-fallback.hh:28-383): lookups built at plan time from the font's cmap
/// over the Unicode presentation forms, or, for a font that looks Windows-1256 encoded, the hand-coded lookups of
/// src/hb-ot-shaper-arabic-win1256.hh. Both are ordinary single and ligature substitution lookups in the bytes of a GSUB table
/// that <see cref="LayoutTable.ofTable"/> parses and the lookup kernel applies, one pass each, with the mask of its feature.</summary>
[<RequireQualifiedAccess>]
module internal ArabicFallback =
    /// <summary>The lookup flag that ignores marks, <c>OT::LookupFlag::IgnoreMarks</c>.</summary>
    [<Literal>]
    let private LookupFlagIgnoreMarks = 8

    /// <summary>The type of a single substitution lookup.</summary>
    [<Literal>]
    let private LookupTypeSingle = 1

    /// <summary>The type of a ligature substitution lookup.</summary>
    [<Literal>]
    let private LookupTypeLigature = 4

    /// <summary>The size of a lookup with one subtable, whose subtable follows.</summary>
    [<Literal>]
    let private LookupHeaderSize = 8

    /// <summary>The size of the header of a GSUB table of version 1.0 and of its empty script and feature lists, where the lookup list starts.</summary>
    [<Literal>]
    let private GsubHeaderSize = 14

    /// <summary>The largest glyph a single substitution of the fallback can hold.</summary>
    [<Literal>]
    let private MaxGlyph16 = 0xFFFFu

    /// <summary>The glyph of alef in a Windows-1256 font.</summary>
    [<Literal>]
    let private Win1256Alef = 199u

    /// <summary>The glyph of lam in a Windows-1256 font.</summary>
    [<Literal>]
    let private Win1256Lam = 225u

    /// <summary>The glyph of alef maksura in a Windows-1256 font.</summary>
    [<Literal>]
    let private Win1256AlefMaksura = 236u

    /// <summary>The glyph of yeh in a Windows-1256 font.</summary>
    [<Literal>]
    let private Win1256Yeh = 237u

    /// <summary>The glyph of sukun in a Windows-1256 font.</summary>
    [<Literal>]
    let private Win1256Sukun = 250u

    /// <summary>The feature <c>init</c>.</summary>
    [<Literal>]
    let private Init = 0x696E6974u<tag>

    /// <summary>The feature <c>medi</c>.</summary>
    [<Literal>]
    let private Medi = 0x6D656469u<tag>

    /// <summary>The feature <c>fina</c>.</summary>
    [<Literal>]
    let private Fina = 0x66696E61u<tag>

    /// <summary>The feature <c>isol</c>.</summary>
    [<Literal>]
    let private Isol = 0x69736F6Cu<tag>

    /// <summary>The features of the fallback lookups, in the order of the columns of the shaping table, followed by <c>rlig</c> for
    /// each of the three ligature tables, <c>arabic_fallback_features</c>.</summary>
    let private fallbackFeatures : ImmutableArray<Tag> =
        Sealed.ofSpan (ReadOnlySpan [| Init; Medi; Fina; Isol; Tags.Rlig; Tags.Rlig; Tags.Rlig |])

    /// <summary>The bytes of a big-endian 16-bit number.</summary>
    /// <param name="value">The number.</param>
    /// <returns>Two bytes.</returns>
    let private be16 (value: int) =
        Sealed.ofSpan (ReadOnlySpan [| byte (value >>> 8); byte value |])

    /// <summary>The bytes of an array of big-endian 16-bit numbers.</summary>
    /// <param name="values">The numbers.</param>
    /// <returns>Two bytes for each.</returns>
    let private be16s (values: ImmutableArray<int>) =
        Sealed.init (values.Length * 2) (fun index ->
            let value = values[index / 2]
            match index % 2 with
            | 0 -> byte (value >>> 8)
            | _ -> byte value)

    /// <summary>Joins parts end to end.</summary>
    /// <param name="parts">The parts.</param>
    /// <param name="joined">What has been joined so far.</param>
    /// <returns>The parts one after the other.</returns>
    [<TailCall>]
    let rec private joinFrom (parts: ImmutableArray<byte> list) (joined: ImmutableArray<byte>) =
        match parts with
        | [] -> joined
        | part :: rest -> joinFrom rest (Sealed.concat joined part)

    /// <summary>Joins parts end to end.</summary>
    /// <param name="parts">The parts.</param>
    /// <returns>The parts one after the other.</returns>
    let private join (parts: ImmutableArray<byte> list) = joinFrom parts ImmutableArray<byte>.Empty

    /// <summary>The number of bytes of the parts before one, counted up from a start.</summary>
    /// <param name="parts">The parts.</param>
    /// <param name="index">The part whose offset is wanted.</param>
    /// <param name="at">The next part to add.</param>
    /// <param name="total">The bytes so far.</param>
    /// <returns>The offset of the part.</returns>
    [<TailCall>]
    let rec private sizeBefore (parts: ImmutableArray<ImmutableArray<byte>>) (index: int) (at: int) (total: int) =
        match at >= index with
        | true -> total
        | false -> sizeBefore parts index (at + 1) (total + parts[at].Length)

    /// <summary>The number of bytes before each part, for the offsets of a table that holds the parts one after the other.</summary>
    /// <param name="start">Where the first part starts.</param>
    /// <param name="parts">The parts.</param>
    /// <returns>The offset of each part.</returns>
    let private offsetsOf (start: int) (parts: ImmutableArray<ImmutableArray<byte>>) =
        Sealed.init parts.Length (fun index -> sizeBefore parts index 0 start)

    /// <summary>The glyph of a code point, <c>hb_font_get_glyph</c> with no variation selector.</summary>
    /// <param name="font">The font.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>The glyph, none when the font has none.</returns>
    let private glyphOf (font: Font) (codepoint: uint32) = Font.nominalGlyph font codepoint

    /// <summary>A single substitution lookup of the presentation forms of one column of the shaping table,
    /// <c>arabic_fallback_synthesize_lookup_single</c>: each letter the font maps, whose form the font maps to another glyph.</summary>
    /// <param name="font">The font.</param>
    /// <param name="form">The column: 0 initial, 1 medial, 2 final, 3 isolated.</param>
    /// <returns>The bytes of the lookup, none when no letter has a form.</returns>
    let private singleLookup (font: Font) (form: int) =
        let letters = Sealed.init (int ArabicTable.ShapingLast - int ArabicTable.ShapingFirst + 1) (fun index -> ArabicTable.ShapingFirst + uint32 index)
        let pairs =
            Sealed.choose (fun u ->
                match ArabicTable.shaping u form with
                | 0us -> ValueNone
                | shaped ->
                    match glyphOf font u, glyphOf font (uint32 shaped) with
                    | ValueSome letter, ValueSome substitute when letter <> substitute && letter <= MaxGlyph16 && substitute <= MaxGlyph16 -> ValueSome (struct (letter, substitute))
                    | _ -> ValueNone) (letters.AsSpan())
        match pairs.IsEmpty with
        | true -> ValueNone
        | false ->
            let sorted = Sealed.stableSortBy (fun (struct (letter, _)) -> int letter) pairs
            let count = sorted.Length
            let glyphs = Sealed.map (fun (struct (letter, _)) -> int letter) (sorted.AsSpan())
            let substitutes = Sealed.map (fun (struct (_, substitute)) -> int substitute) (sorted.AsSpan())
            let subtable = join [ be16 2; be16 (6 + 2 * count); be16 count; be16s substitutes; be16 1; be16 count; be16s glyphs ]
            ValueSome (join [ be16 LookupTypeSingle; be16 LookupFlagIgnoreMarks; be16 1; be16 LookupHeaderSize; subtable ])

    /// <summary>The bytes of one ligature: its glyph, the number of its components with the first, and the rest.</summary>
    /// <param name="ligature">The ligature glyph.</param>
    /// <param name="components">The components after the first.</param>
    /// <returns>The bytes.</returns>
    let private ligatureBytes (ligature: int) (components: ImmutableArray<int>) =
        join [ be16 ligature; be16 (1 + components.Length); be16s components ]

    /// <summary>The bytes of the ligatures that start with one glyph.</summary>
    /// <param name="ligatures">The ligatures, each with its components.</param>
    /// <returns>The ligature set.</returns>
    let private ligatureSetBytes (ligatures: ImmutableArray<struct (int * ImmutableArray<int>)>) =
        let parts = Sealed.map (fun (struct (ligature, components)) -> ligatureBytes ligature components) (ligatures.AsSpan())
        let header = 2 + 2 * parts.Length
        join [ be16 parts.Length; be16s (offsetsOf header parts); join (List.ofSeq parts) ]

    /// <summary>The ligatures of one set the font can show: the ligature and all its components must be mapped.</summary>
    /// <param name="font">The font.</param>
    /// <param name="set">The set.</param>
    /// <returns>The ligature glyphs with their component glyphs.</returns>
    let private fontLigatures (font: Font) (set: ArabicLigatureSet) =
        Sealed.choose (fun (struct (components: ImmutableArray<uint16>, ligature: uint16)) ->
            match glyphOf font (uint32 ligature), Sealed.tryMapAll (fun (letter: uint16) -> glyphOf font (uint32 letter)) (components.AsSpan()) with
            | ValueSome glyph, ValueSome glyphs -> ValueSome (struct (int (uint16 glyph), Sealed.map (fun (g: uint32) -> int (uint16 g)) (glyphs.AsSpan())))
            | _ -> ValueNone) (set.Entries.AsSpan())

    /// <summary>A ligature substitution lookup over one of the ligature tables, <c>arabic_fallback_synthesize_lookup_ligature</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="table">The ligature table.</param>
    /// <param name="flags">The lookup flags.</param>
    /// <returns>The bytes of the lookup, none when no ligature can be shown.</returns>
    let private ligatureLookup (font: Font) (table: ImmutableArray<ArabicLigatureSet>) (flags: int) =
        let sets =
            Sealed.choose (fun (set: ArabicLigatureSet) ->
                match glyphOf font (uint32 set.First) with
                | ValueNone -> ValueNone
                | ValueSome first ->
                    let ligatures = fontLigatures font set
                    match ligatures.IsEmpty with
                    | true -> ValueNone
                    | false -> ValueSome (struct (int (uint16 first), ligatures))) (table.AsSpan())
        match sets.IsEmpty with
        | true -> ValueNone
        | false ->
            let sorted = Sealed.stableSortBy (fun (struct (first, _)) -> first) sets
            let setBytes = Sealed.map (fun (struct (_, ligatures)) -> ligatureSetBytes ligatures) (sorted.AsSpan())
            let firsts = Sealed.map (fun (struct (first, _)) -> first) (sorted.AsSpan())
            let header = 6 + 2 * sorted.Length
            let coverage = join [ be16 1; be16 firsts.Length; be16s firsts ]
            let setsTotal = join (List.ofSeq setBytes)
            let subtable = join [ be16 1; be16 (header + setsTotal.Length); be16 sorted.Length; be16s (offsetsOf header setBytes); setsTotal; coverage ]
            ValueSome (join [ be16 LookupTypeLigature; be16 flags; be16 1; be16 LookupHeaderSize; subtable ])

    /// <summary>The lookup of one of the fallback features, <c>arabic_fallback_synthesize_lookup</c>: a single substitution lookup
    /// for each of the four forms and a ligature lookup for each of the three ligature tables.</summary>
    /// <param name="font">The font.</param>
    /// <param name="index">The position in <see cref="fallbackFeatures"/>.</param>
    /// <returns>The bytes of the lookup, none when there is nothing to substitute.</returns>
    let private synthesize (font: Font) (index: int) =
        match index with
        | 4 -> ligatureLookup font ArabicTable.ligatures3 LookupFlagIgnoreMarks
        | 5 -> ligatureLookup font ArabicTable.ligatures LookupFlagIgnoreMarks
        | 6 -> ligatureLookup font ArabicTable.ligaturesMark 0
        | _ -> singleLookup font index

    /// <summary>A lookup of the fallback with the mask of its feature.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private FallbackLookup =
        { Mask: uint32
          Bytes: ImmutableArray<byte> }

    /// <summary>The lookups of the fallback over the Unicode presentation forms, <c>arabic_fallback_plan_init_unicode</c>.</summary>
    /// <param name="map">The compiled map.</param>
    /// <param name="font">The font.</param>
    /// <returns>The lookups the font can do, with the masks of their features.</returns>
    let private unicodeLookups (map: OtMap) (font: Font) =
        let indices = Sealed.init fallbackFeatures.Length id
        Sealed.choose (fun (index: int) ->
            let mask = MapBuilder.featureMask1 map fallbackFeatures[index]
            match mask <> 0u with
            | false -> ValueNone
            | true ->
                match synthesize font index with
                | ValueSome bytes -> ValueSome { Mask = mask; Bytes = bytes }
                | ValueNone -> ValueNone) (indices.AsSpan())

    /// <summary>Whether a font looks Windows-1256 encoded: its letters sit where that code page has them.</summary>
    /// <param name="font">The font.</param>
    /// <returns>True when alef, lam, alef maksura, yeh and sukun are at their Windows-1256 positions.</returns>
    let private looksWin1256 (font: Font) =
        glyphOf font 0x0627u = ValueSome Win1256Alef
        && glyphOf font 0x0644u = ValueSome Win1256Lam
        && glyphOf font 0x0649u = ValueSome Win1256AlefMaksura
        && glyphOf font 0x064Au = ValueSome Win1256Yeh
        && glyphOf font 0x0652u = ValueSome Win1256Sukun

    /// <summary>The lookups of a Windows-1256 font, <c>arabic_fallback_plan_init_win1256</c>: each record of the manifest whose
    /// feature the map has, the lookup running from its offset to the end of the data, where its subtables are.</summary>
    /// <param name="map">The compiled map.</param>
    /// <param name="font">The font.</param>
    /// <returns>The lookups, none when the font does not look Windows-1256 encoded or the map has none of the features.</returns>
    let private win1256Lookups (map: OtMap) (font: Font) =
        match looksWin1256 font with
        | false -> ImmutableArray<FallbackLookup>.Empty
        | true ->
            Sealed.choose (fun (struct (tag: Tag, offset: int)) ->
                let mask = MapBuilder.featureMask1 map tag
                match mask <> 0u with
                | false -> ValueNone
                | true -> ValueSome { Mask = mask; Bytes = Sealed.ofSpan (ArabicWin1256.bytes.AsSpan().Slice offset) }) (ArabicWin1256.manifest.AsSpan())

    /// <summary>The lookups of the fallback plan, <c>arabic_fallback_plan_create</c>: those over the presentation forms, else those
    /// of a Windows-1256 font.</summary>
    /// <param name="map">The compiled map.</param>
    /// <param name="font">The font.</param>
    /// <returns>The lookups, none when the font has neither.</returns>
    let private lookupsOf (map: OtMap) (font: Font) =
        let unicode = unicodeLookups map font
        match unicode.IsEmpty with
        | false -> unicode
        | true -> win1256Lookups map font

    /// <summary>The bytes of a GSUB table that holds the lookups and no script or feature.</summary>
    /// <param name="lookups">The lookups.</param>
    /// <returns>The table.</returns>
    let private tableBytes (lookups: ImmutableArray<FallbackLookup>) =
        let bodies = Sealed.map (fun (lookup: FallbackLookup) -> lookup.Bytes) (lookups.AsSpan())
        let header = 2 + 2 * bodies.Length
        let lookupList = join [ be16 bodies.Length; be16s (offsetsOf header bodies); join (List.ofSeq bodies) ]
        join [ be16 1; be16 0; be16 (GsubHeaderSize - 4); be16 (GsubHeaderSize - 2); be16 GsubHeaderSize; be16 0; be16 0; lookupList ]

    /// <summary>The lookup maps of the fallback lookups: each with its mask, joiners skipped as for a feature with no flags.</summary>
    /// <param name="lookups">The lookups.</param>
    /// <returns>The lookup maps.</returns>
    let private lookupMaps (lookups: ImmutableArray<FallbackLookup>) =
        Sealed.init lookups.Length (fun index ->
            { LookupIndex = index
              AutoZwnj = true
              AutoZwj = true
              Random = false
              PerSyllable = false
              Mask = lookups[index].Mask
              FeatureTag = Tags.Empty })

    /// <summary>Shapes a run with the fallback lookups, <c>arabic_fallback_shape</c> and <c>arabic_fallback_plan_shape</c>: the
    /// plan is built from the font and the map each time, which gives the value upstream builds once and keeps, since it reads only
    /// the font's cmap.</summary>
    /// <param name="data">The plan data.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run, with its joining masks set.</param>
    /// <returns>The run after the fallback lookups.</returns>
    let shape (data: ArabicPlan) (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        match data.DoFallback with
        | false -> run
        | true ->
            let lookups = lookupsOf plan.Map font
            match lookups.IsEmpty with
            | true -> run
            | false ->
                let bytes = tableBytes lookups
                let table = LayoutTable.ofTable (bytes.AsMemory()) Tags.Gsub (SubtableParser(GsubTables.parse)) GsubTables.subtableDigest GsubTables.secondDigest
                let stages = ImmutableArray.Create { LastLookup = lookups.Length; Pause = NoPause }
                Engine.applyGsub false table (lookupMaps lookups) stages font run

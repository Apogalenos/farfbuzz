namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The glyphs of one line that are not in the arrays of the document: a line whose shaped glyph count is larger than its
/// code-point count, or whose result carries no positions of its own length.</summary>
type DetachedLine =
    { LineInfos: ImmutableArray<GlyphInfo>
      LinePositions: ImmutableArray<GlyphPosition> }

/// <summary>Where the shaped glyphs of one line of a document are.</summary>
[<Struct>]
type LineOutcome =
    | Arena of start: int * length: int // a window of the document's two arrays
    | Alone of line: DetachedLine // arrays of its own, for the rare line that does not fit its window
    | Refused of error: ShapeError // the line could not be shaped

/// <summary>The shaped lines of a document: one array of glyphs and one of positions for the whole document, and for each line
/// where its glyphs are. The properties of the glyphs (<c>GlyphProps</c>) are not kept: the shapers that need them have used them by
/// the time a line is shaped.</summary>
type ShapedDocument =
    { ArenaInfos: ImmutableArray<GlyphInfo>
      ArenaPositions: ImmutableArray<GlyphPosition>
      Lines: ImmutableArray<LineOutcome> }

/// <summary>Reading the lines of a shaped document.</summary>
[<RequireQualifiedAccess>]
module ShapedDocument =
    /// <summary>The glyphs of a line.</summary>
    /// <param name="document">The document.</param>
    /// <param name="index">The line.</param>
    /// <returns>The glyphs, none for a line that could not be shaped.</returns>
    let infosOf (document: ShapedDocument) (index: int) =
        match document.Lines[index] with
        | Arena (start, length) -> document.ArenaInfos.AsMemory().Slice(start, length)
        | Alone line -> line.LineInfos.AsMemory()
        | Refused _ -> ReadOnlyMemory<GlyphInfo>.Empty

    /// <summary>The positions of a line.</summary>
    /// <param name="document">The document.</param>
    /// <param name="index">The line.</param>
    /// <returns>The positions, none for a line that could not be shaped.</returns>
    let positionsOf (document: ShapedDocument) (index: int) =
        match document.Lines[index] with
        | Arena (start, length) -> document.ArenaPositions.AsMemory().Slice(start, length)
        | Alone line -> line.LinePositions.AsMemory()
        | Refused _ -> ReadOnlyMemory<GlyphPosition>.Empty

    /// <summary>Why a line could not be shaped.</summary>
    /// <param name="document">The document.</param>
    /// <param name="index">The line.</param>
    /// <returns>The error, none for a line that was shaped.</returns>
    let errorOf (document: ShapedDocument) (index: int) =
        match document.Lines[index] with
        | Arena _ -> ValueNone
        | Alone _ -> ValueNone
        | Refused error -> ValueSome error

/// <summary>The decoder of one text encoding, for the document entry points.</summary>
type internal ILineCodec<'T> =
    /// <summary>Reads forward.</summary>
    /// <param name="text">The text.</param>
    /// <param name="index">The unit index.</param>
    /// <param name="replacement">The replacement.</param>
    /// <returns>The code point and the next index.</returns>
    abstract Next: text: ReadOnlySpan<'T> * index: int * replacement: uint32 -> struct (uint32 * int)
    /// <summary>Whether every unit of a text is one code point, so that no decoding is needed to count them.</summary>
    /// <param name="text">The text.</param>
    /// <returns>True when the text decodes unit by unit.</returns>
    abstract Direct: text: ReadOnlySpan<'T> -> bool
    /// <summary>Writes the glyph infos of a text that decodes unit by unit into the first elements of an array, never called for a
    /// codec whose <c>Direct</c> is false.</summary>
    /// <param name="text">The text.</param>
    /// <param name="target">The array.</param>
    /// <param name="clusterBase">The cluster of the first unit.</param>
    abstract DirectInto: text: ReadOnlySpan<'T> * target: GlyphInfo array * clusterBase: int -> unit
    /// <summary>Appends text to a run, the general route's way in.</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="offset">The first unit.</param>
    /// <param name="length">The units.</param>
    /// <param name="settings">The run to append to.</param>
    /// <returns>The run with the text.</returns>
    abstract Add: text: ReadOnlySpan<'T> * offset: int * length: int * settings: GlyphRun -> GlyphRun
    /// <summary>Appends text to a run that a caller shapes at once and shares with no one, the general route's way in for a line that is
    /// shaped without verification: a codec may leave out the properties array that the first stage of shaping replaces.</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="offset">The first unit.</param>
    /// <param name="length">The units.</param>
    /// <param name="settings">The run to append to.</param>
    /// <returns>The run with the text.</returns>
    abstract AddFresh: text: ReadOnlySpan<'T> * offset: int * length: int * settings: GlyphRun -> GlyphRun

/// <summary>The UTF-8 decoder.</summary>
[<Struct>]
type internal Utf8Lines =
    interface ILineCodec<byte> with
        member _.Next (text, index, replacement) = Utf.next8 text index replacement
        member _.Direct text = System.Text.Ascii.IsValid text
        member _.DirectInto (text, target, clusterBase) = Sealed.byteInfosInto text target clusterBase
        member _.Add (text, offset, length, settings) = GlyphRun.addUtf8 text offset length settings
        member _.AddFresh (text, offset, length, settings) = GlyphRun.addUtf8Fresh text offset length settings

/// <summary>The UTF-16 decoder.</summary>
[<Struct>]
type internal Utf16Lines =
    interface ILineCodec<uint16> with
        member _.Next (text, index, replacement) = Utf.next16 text index replacement
        member _.Direct _ = false
        member _.DirectInto (_, _, _) = ()
        member _.Add (text, offset, length, settings) = GlyphRun.addUtf16 text offset length settings
        member _.AddFresh (text, offset, length, settings) = GlyphRun.addUtf16 text offset length settings

/// <summary>The validating UTF-32 decoder.</summary>
[<Struct>]
type internal Utf32Lines =
    interface ILineCodec<uint32> with
        member _.Next (text, index, replacement) = Utf.next32 text index replacement true
        member _.Direct _ = false
        member _.DirectInto (_, _, _) = ()
        member _.Add (text, offset, length, settings) = GlyphRun.addUtf32 text offset length settings
        member _.AddFresh (text, offset, length, settings) = GlyphRun.addUtf32 text offset length settings

/// <summary>The decoder of code points that are not validated.</summary>
[<Struct>]
type internal CodepointLines =
    interface ILineCodec<uint32> with
        member _.Next (text, index, _) = Utf.next32 text index 0u false
        member _.Direct _ = false
        member _.DirectInto (_, _, _) = ()
        member _.Add (text, offset, length, settings) = GlyphRun.addCodepoints text offset length settings
        member _.AddFresh (text, offset, length, settings) = GlyphRun.addCodepoints text offset length settings

/// <summary>What one pass over a line says before it is shaped.</summary>
[<Struct>]
type internal LineFacts =
    { Count: int
      Direct: bool
      FirstScript: Script }

/// <summary>What the kernel needs to shape the lines of one set of segment properties in the arrays of a worker.</summary>
type internal LeanKernel =
    { KernelPlan: ShapePlan
      OtPlan: OtShapePlan
      Glyphs: SimpleRun.SimpleGlyphs
      Probe: GlyphRun
      LookupFree: bool
      Mask: uint32
      FinalMask: uint32
      AndMask: uint32 }

/// <summary>The plan of one set of segment properties, or why there is none, with the kernel when its lines may take it.</summary>
type internal ResolvedLine =
    { Key: Script // the first script of the lines the properties were guessed for
      Props: SegmentProperties
      Seeded: GlyphRun // the settings of every line with these properties, so that a line built from text needs no guessing
      Plan: Result<ShapePlan, ShapeError>
      Kernel: LeanKernel voption }

/// <summary>What the kernel in a worker's arrays made of a line.</summary>
[<Struct>]
type internal LeanResult =
    | LeanDone of length: int // the worker's arrays hold the finished line, of this many glyphs
    | LeanGeneral // the line is not one the kernel finishes: the general route shapes it from the text
    | LeanShaped of shaped: Result<GlyphRun, ShapeError> // the kernel's tails finished the line as a run of its own

/// <summary>What shaping a line from glyphs the caller holds into arrays the caller owns made of it.</summary>
[<Struct>]
type internal IntoOutcome =
    | IntoShaped of count: int * positionCount: int * randomState: uint32 * successful: bool // the arrays hold the glyphs and positions, this many of each
    | IntoTooShort of needed: int // an array is shorter than the shaped line: nothing was written, the arrays are as they were
    | IntoRefused of error: ShapeError // the line could not be shaped: nothing was written

/// <summary>Document shaping into two arrays, the lines of a text shaped without a run per line: the text is decoded into the arrays
/// a worker owns and shaped there by the kernel, and only the finished glyphs and positions are copied into the document. A line the
/// kernel does not take is shaped by the general route from a run built as <c>GlyphRun.addUtf8</c> builds it, so that every line is
/// equal in its glyphs and positions to the run <c>Shape.shapeCached</c> gives.</summary>
[<RequireQualifiedAccess>]
module LineShape =
    /// <summary>The most glyphs an array of a document holds.</summary>
    [<Literal>]
    let private ArenaLimit = GlyphRun.MaxLenDefault

    /// <summary>The offset and length a line has in a text, clamped as <c>hb_buffer_add_utf</c> clamps them.</summary>
    /// <param name="textLength">The units of the text.</param>
    /// <param name="start">The first unit asked for.</param>
    /// <param name="length">The units asked for, negative for the rest.</param>
    /// <returns>The offset and the length.</returns>
    let internal clamp (textLength: int) (start: int) (length: int) =
        let offset = match start < 0 with | true -> textLength | false -> min start textLength
        let rest = match length < 0 with | true -> textLength - offset | false -> length
        struct (offset, max 0 (min rest (textLength - offset)))

    /// <summary>Counts the code points of a line.</summary>
    /// <param name="codec">The decoder.</param>
    /// <param name="text">The line.</param>
    /// <param name="replacement">The replacement.</param>
    /// <param name="at">The next unit.</param>
    /// <param name="count">The count so far.</param>
    /// <returns>The code points.</returns>
    [<TailCall>]
    let rec private countFrom<'T, 'C when 'C : struct and 'C :> ILineCodec<'T>> (codec: 'C) (text: ReadOnlySpan<'T>) (replacement: uint32) (at: int) (count: int) =
        match at >= text.Length with
        | true -> count
        | false ->
            let struct (_, next) = codec.Next(text, at, replacement)
            countFrom codec text replacement next (count + 1)

    /// <summary>Finds the script of the first code point of a line that is neither Common, Inherited nor Unknown.</summary>
    /// <param name="codec">The decoder.</param>
    /// <param name="text">The line.</param>
    /// <param name="replacement">The replacement.</param>
    /// <param name="at">The next unit.</param>
    /// <returns>The script, or <c>Scripts.Invalid</c> when every code point is neutral.</returns>
    [<TailCall>]
    let rec private scriptFrom<'T, 'C when 'C : struct and 'C :> ILineCodec<'T>> (codec: 'C) (text: ReadOnlySpan<'T>) (replacement: uint32) (at: int) =
        match at >= text.Length with
        | true -> Scripts.Invalid
        | false ->
            let struct (codepoint, next) = codec.Next(text, at, replacement)
            let candidate = Unicode.script codepoint
            match candidate = Scripts.Common || candidate = Scripts.Inherited || candidate = Scripts.Unknown with
            | true -> scriptFrom codec text replacement next
            | false -> candidate

    /// <summary>Looks at a line: how many code points it has and, when the segment properties need it, its first script.</summary>
    /// <param name="codec">The decoder.</param>
    /// <param name="text">The text.</param>
    /// <param name="replacement">The replacement.</param>
    /// <param name="needsScript">Whether the segment properties have no script.</param>
    /// <param name="line">The offset and length asked for.</param>
    /// <returns>The facts.</returns>
    let private factsOf<'T, 'C when 'C : struct and 'C :> ILineCodec<'T>> (codec: 'C) (text: ReadOnlyMemory<'T>) (replacement: uint32) (needsScript: bool) (line: struct (int * int)) =
        let span = text.Span
        let struct (start, requested) = line
        let struct (offset, length) = clamp span.Length start requested
        match length > Int32.MaxValue / 8 with
        | true -> { Count = 0; Direct = false; FirstScript = Scripts.Invalid }
        | false ->
            let item = span.Slice(offset, length)
            let direct = codec.Direct item
            let count = match direct with | true -> length | false -> countFrom codec item replacement 0 0
            let script = match needsScript with | false -> Scripts.Invalid | true -> scriptFrom codec item replacement 0
            { Count = count; Direct = direct; FirstScript = script }

    /// <summary>Builds what the kernel needs for the lines of one set of segment properties, when they may take it.</summary>
    /// <param name="font">The font.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="props">The segment properties.</param>
    /// <param name="plan">The plan.</param>
    /// <returns>The kernel, or none when the lines go to the general route.</returns>
    let private kernelOf (font: Font) (settings: GlyphRun) (props: SegmentProperties) (plan: Result<ShapePlan, ShapeError>) =
        match plan with
        | Error _ -> ValueNone
        | Ok plan ->
            match plan.Ot with
            | ValueNone -> ValueNone
            | ValueSome otPlan ->
                let probe = { settings with SegmentProperties = props }
                match settings.Flags &&& BufferFlags.Verify = BufferFlags.Default && SimpleRun.isSimplePlan otPlan probe with
                | false -> ValueNone
                | true ->
                    let lookupFree = plan.GsubCovered.IsEmpty && plan.GposCovered.IsEmpty
                    let final = SimpleRun.finalMask otPlan probe
                    ValueSome
                        { KernelPlan = plan
                          OtPlan = otPlan
                          Glyphs = SimpleRun.SimpleGlyphs(font, otPlan.FallbackGlyphClasses, true, settings.NotFound)
                          Probe = probe
                          LookupFree = lookupFree
                          Mask = (match lookupFree with | true -> final | false -> otPlan.Map.GlobalMask)
                          FinalMask = final
                          AndMask = Flags.andMaskOf settings.Flags }

    /// <summary>Finds the plan resolved for the lines whose first script is a given one.</summary>
    /// <param name="resolved">The resolved plans.</param>
    /// <param name="script">The first script of a line.</param>
    /// <param name="index">The next plan.</param>
    /// <returns>The index of the plan, or minus one.</returns>
    [<TailCall>]
    let rec private indexOfScript (resolved: ImmutableArray<ResolvedLine>) (script: Script) (index: int) =
        match index >= resolved.Length with
        | true -> -1
        | false ->
            match resolved[index].Key = script with
            | true -> index
            | false -> indexOfScript resolved script (index + 1)

    /// <summary>Resolves, in the order the lines first need them, one plan for every distinct segment-properties value of the
    /// non-empty lines, through the cache, as <c>Shape.shapeMany</c> does for runs.</summary>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers.</param>
    /// <param name="defaultLanguage">The language of a line that has none.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="facts">The facts of each line.</param>
    /// <param name="index">The next line.</param>
    /// <param name="resolved">The plans resolved so far.</param>
    /// <returns>The resolved plans and the cache with the new plans added.</returns>
    [<TailCall>]
    let rec private resolveFrom (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (defaultLanguage: Language) (settings: GlyphRun) (facts: ImmutableArray<LineFacts>) (index: int) (resolved: ImmutableArray<ResolvedLine>) =
        match index >= facts.Length with
        | true -> struct (resolved, cache)
        | false ->
            let fact = facts[index]
            match fact.Count = 0 || indexOfScript resolved fact.FirstScript 0 >= 0 with
            | true -> resolveFrom cache font features shapers defaultLanguage settings facts (index + 1) resolved
            | false ->
                let props = GlyphRun.guessWith defaultLanguage settings.SegmentProperties fact.FirstScript
                let struct (plan, cache) = Shape.planFromCache cache font features shapers { settings with SegmentProperties = props }
                let added = Sealed.appendOne (resolved.AsSpan()) { Key = fact.FirstScript; Props = props; Seeded = { settings with SegmentProperties = props }; Plan = plan; Kernel = kernelOf font settings props plan }
                resolveFrom cache font features shapers defaultLanguage settings facts (index + 1) added

    /// <summary>Decodes a line into the first glyphs of an array.</summary>
    [<Struct>]
    type private LineUnfolder<'T, 'C when 'C : struct and 'C :> ILineCodec<'T>> =
        val private codec: 'C
        val private replacement: uint32
        val private clusterBase: int

        /// <summary>Creates the unfolder.</summary>
        /// <param name="codec">The decoder.</param>
        /// <param name="replacement">The replacement.</param>
        /// <param name="clusterBase">The cluster of the first unit.</param>
        new (codec, replacement, clusterBase) = { codec = codec; replacement = replacement; clusterBase = clusterBase }

        interface ISpanUnfolder<'T, int, GlyphInfo> with
            member this.Next (source, state) =
                let struct (codepoint, after) = this.codec.Next(source, state, this.replacement)
                struct ({ Codepoint = codepoint; Mask = 0u; Cluster = uint32 (this.clusterBase + state); Var1 = 0u; Var2 = 0u }, after)

    /// <summary>Finishes the flags of a line the lean loop positioned in the arrays a worker owns, as <c>SimpleRun</c> finishes it: a
    /// buffer that wants the tatweel rule, and a line in which two glyphs share a cluster, go to the general route.</summary>
    /// <param name="kernel">The kernel.</param>
    /// <param name="flags">The buffer flags of every line.</param>
    /// <param name="scratch">The worker's arrays, holding the glyphs.</param>
    /// <param name="count">The glyphs of the line.</param>
    /// <returns>Whether the arrays hold the finished line.</returns>
    let private finishedLean (kernel: LeanKernel) (flags: BufferFlags) (scratch: LineScratch) (count: int) =
        match flags &&& BufferFlags.ProduceSafeToInsertTatweel = BufferFlags.Default with
        | false -> LeanGeneral
        | true ->
            match Sealed.finishFlagsOf scratch.ScratchInfos count kernel.AndMask with
            | true -> LeanDone count
            | false -> LeanGeneral

    /// <summary>Applies the pair lookups to a line in the arrays a worker owns, when the lean loop is the one that applies them, and
    /// finishes its flags, as <c>SimpleRun</c> finishes a line the lean loop positioned.</summary>
    /// <param name="kernel">The kernel.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="font">The font.</param>
    /// <param name="scratch">The worker's arrays, holding the glyphs.</param>
    /// <param name="count">The glyphs of the line.</param>
    /// <returns>Whether the arrays hold the finished line.</returns>
    let private pairLane (kernel: LeanKernel) (settings: GlyphRun) (font: Font) (scratch: LineScratch) (count: int) =
        match font.SlantXY <> 0f with
        | true -> LeanGeneral
        | false ->
            let budget = GlyphRun.enteredMaxOpsOf count settings.MaxOps
            match Engine.positionOwnedFirst kernel.KernelPlan.GposPairs font scratch.ScratchInfos scratch.ScratchPositions count settings.Flags settings.ClusterLevel budget with
            | ValueNone -> LeanGeneral
            | ValueSome _ -> finishedLean kernel settings.Flags scratch count

    /// <summary>The run fields the substitution engine reads for a line.</summary>
    /// <param name="kernel">The kernel.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="count">The glyphs of the line.</param>
    /// <returns>The fields, without glyphs, properties or positions.</returns>
    let private enteredBy (kernel: LeanKernel) (settings: GlyphRun) (count: int) =
        { RunInput.Infos = ImmutableArray<GlyphInfo>.Empty
          Props = ImmutableArray<GlyphProps>.Empty
          Positions = ImmutableArray<GlyphPosition>.Empty
          HavePositions = false
          MaxLen = GlyphRun.enteredMaxLenOf count settings.MaxLen
          MaxOps = GlyphRun.enteredMaxOpsOf count settings.MaxOps
          Successful = kernel.Probe.Successful
          ClusterLevel = kernel.Probe.ClusterLevel
          Flags = kernel.Probe.Flags
          Serial = 0uy
          RandomState = kernel.Probe.RandomState
          ScratchFlags = ScratchFlags.Default }

    /// <summary>Finishes a line the substitution engine changed through the kernel's tails, from copies of the engine's glyphs: the
    /// route a line takes when the lean tail does not apply to it.</summary>
    /// <param name="kernel">The kernel.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="font">The font.</param>
    /// <param name="count">The glyphs of the line before the substitution.</param>
    /// <param name="owned">What the engine made of the line.</param>
    /// <returns>The line the kernel's tails finished.</returns>
    let private tailedBy (kernel: LeanKernel) (settings: GlyphRun) (font: Font) (count: int) (owned: OwnedGsub) =
        let engine =
            { kernel.Probe with
                MaxLen = GlyphRun.enteredMaxLenOf count settings.MaxLen
                MaxOps = owned.OwnedMaxOps
                Serial = uint32 owned.OwnedSerial
                RandomState = owned.OwnedRandom
                ScratchFlags = owned.OwnedScratch
                Successful = owned.OwnedSuccessful
                Infos = Sealed.copiedRange owned.OwnedInfos owned.OwnedLength
                Props = Sealed.copiedRange owned.OwnedProps owned.OwnedLength
                Positions = ImmutableArray<GlyphPosition>.Empty
                HavePositions = false
                ContentType = GlyphContent }
        let struct (_, shaped) =
            match Sealed.anyGlyphIn kernel.KernelPlan.GposCovered (engine.Infos.AsSpan()) with
            | false -> SimpleRun.afterSubstitution kernel.OtPlan font kernel.Probe engine
            | true -> SimpleRun.substitutedKernelled kernel.KernelPlan kernel.OtPlan font kernel.Probe engine
        LeanShaped shaped

    /// <summary>Finishes a changed line no pair lookup applies to, in the arrays a worker owns, as <c>SimpleRun.afterSubstitution</c>
    /// finishes it: the default positions and the flags propagated over the clusters.</summary>
    /// <param name="kernel">The kernel.</param>
    /// <param name="font">The font.</param>
    /// <param name="scratch">The worker's arrays, holding the glyphs the engine left.</param>
    /// <param name="length">The glyphs of the line.</param>
    /// <returns>The finished line, or none when a glyph is refused and nothing was written.</returns>
    let private plainChanged (kernel: LeanKernel) (font: Font) (scratch: LineScratch) (length: int) =
        let metrics = font.Ot.Hmtx.Value.Table.Span
        let finished = Sealed.finishSubstitutedOf kernel.Glyphs scratch.ScratchInfos scratch.ScratchProps scratch.ScratchPositions metrics length GlyphPropsBits.Mark ~~~UnicodeProps.GenCatMask kernel.AndMask
        match finished with
        | true -> ValueSome (LeanDone length)
        | false -> ValueNone

    /// <summary>Finishes a changed line some pair lookup applies to, in the arrays a worker owns, as <c>SimpleRun.substitutedKernelled</c>
    /// finishes it: the default positions, the pair lookups the lean loop applies, and the flags. The lean loop reads that every glyph
    /// carries the global mask, that none is a mark or default ignorable and that no two share a cluster, which are checked as the
    /// positions are written; nothing but the positions is written until they hold.</summary>
    /// <param name="kernel">The kernel.</param>
    /// <param name="flags">The buffer flags of every line.</param>
    /// <param name="level">The cluster level of every line.</param>
    /// <param name="font">The font.</param>
    /// <param name="scratch">The worker's arrays, holding the glyphs the engine left.</param>
    /// <param name="owned">What the engine made of the line, whose budget left is the only budget the lean loop may be given.</param>
    /// <returns>The finished line, or none when the lean loop does not apply and no glyph was written.</returns>
    let private pairedChanged (kernel: LeanKernel) (flags: BufferFlags) (level: ClusterLevel) (font: Font) (scratch: LineScratch) (owned: OwnedGsub) =
        let length = owned.OwnedLength
        let metrics = font.Ot.Hmtx.Value.Table.Span
        let written = Sealed.leanAdvancesOf kernel.Glyphs scratch.ScratchInfos scratch.ScratchProps scratch.ScratchPositions metrics length GlyphPropsBits.Mark ~~~UnicodeProps.GenCatMask kernel.OtPlan.Map.GlobalMask
        match written with
        | false -> ValueNone
        | true ->
            match Engine.positionOwnedFirst kernel.KernelPlan.GposPairs font scratch.ScratchInfos scratch.ScratchPositions length flags level owned.OwnedMaxOps with
            | ValueNone -> ValueNone
            | ValueSome _ -> ValueSome (finishedLean kernel flags scratch length)

    /// <summary>Finishes a line the substitution engine changed. The engine wrote its glyphs into the worker's arrays; when they are
    /// still there, the run succeeded, set no scratch flag, the buffer does not want the tatweel rule and the font is not slanted, they
    /// are finished in place by the lean tails, and otherwise the kernel's tails finish them from copies.</summary>
    /// <param name="kernel">The kernel.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="font">The font.</param>
    /// <param name="scratch">The worker's arrays.</param>
    /// <param name="count">The glyphs of the line before the substitution.</param>
    /// <param name="owned">What the engine made of the line.</param>
    /// <returns>Whether the arrays hold the finished line, or the line the kernel's tails finished.</returns>
    let private changedLine (kernel: LeanKernel) (settings: GlyphRun) (font: Font) (scratch: LineScratch) (count: int) (owned: OwnedGsub) =
        let inPlace = obj.ReferenceEquals(owned.OwnedInfos, scratch.ScratchInfos) && owned.OwnedLength <= count
        let lean = inPlace && owned.OwnedSuccessful && owned.OwnedScratch = ScratchFlags.Default && font.SlantXY = 0f && settings.Flags &&& BufferFlags.ProduceSafeToInsertTatweel = BufferFlags.Default
        let finished =
            match lean with
            | false -> ValueNone
            | true ->
                match Sealed.anyGlyphIn kernel.KernelPlan.GposCovered (ReadOnlySpan<GlyphInfo>(scratch.ScratchInfos, 0, owned.OwnedLength)) with
                | true -> pairedChanged kernel settings.Flags settings.ClusterLevel font scratch owned
                | false -> plainChanged kernel font scratch owned.OwnedLength
        match finished with
        | ValueSome result -> result
        | ValueNone -> tailedBy kernel settings font count owned

    /// <summary>Finishes a line whose glyphs the substitution engine may change, in the arrays a worker owns: the engine edits the
    /// line where it is, and a line it leaves as it was goes on to the positioning of <see cref="pairLane"/> as <c>SimpleRun</c> takes it.</summary>
    /// <param name="kernel">The kernel.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="font">The font.</param>
    /// <param name="scratch">The worker's arrays, holding the glyphs.</param>
    /// <param name="count">The glyphs of the line.</param>
    /// <param name="admission">The look at the glyphs the admission made.</param>
    /// <param name="positions">Whether a pair lookup can apply to the glyphs.</param>
    /// <returns>Whether the arrays hold the finished line, or the line the kernel's tails finished when the engine changed it.</returns>
    let private substituted (kernel: LeanKernel) (settings: GlyphRun) (font: Font) (scratch: LineScratch) (count: int) (admission: RunAdmission) (positions: bool) =
        let input = enteredBy kernel settings count
        let owned = Engine.substituteOwned kernel.OtPlan font kernel.KernelPlan.GsubLookupCovered kernel.KernelPlan.GsubGlyphLookups admission input scratch.ScratchInfos scratch.ScratchProps count
        match owned.OwnedUnchanged, positions with
        | false, _ -> changedLine kernel settings font scratch count owned
        | true, false ->
            Sealed.setMasksOf scratch.ScratchInfos count kernel.FinalMask
            LeanDone count
        | true, true -> pairLane kernel settings font scratch count

    /// <summary>Finishes a line the lookups touch, in the arrays a worker owns: the stages of the kernel's tail for a line no
    /// substitution applies to and the pair lookups the lean loop applies position, as <c>SimpleRun</c> finishes such a line.</summary>
    /// <param name="kernel">The kernel.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="font">The font.</param>
    /// <param name="scratch">The worker's arrays, holding the glyphs.</param>
    /// <param name="count">The glyphs of the line.</param>
    /// <param name="hit">Whether some glyph is in the substitution glyph set.</param>
    /// <returns>Whether the arrays hold the finished line, or the line the kernel's tails finished.</returns>
    let private withLookups (kernel: LeanKernel) (settings: GlyphRun) (font: Font) (scratch: LineScratch) (count: int) (hit: bool) =
        let infos = scratch.ScratchInfos
        let struct (substitutes, admission) =
            match hit with
            | false -> struct (false, { Known = false; Candidates = UInt64.MaxValue; GlyphDigest = Digest.empty })
            | true -> Engine.admit kernel.OtPlan font kernel.KernelPlan.GsubGlyphLookups kernel.OtPlan.Map.GlobalMask settings.Flags (ReadOnlySpan<GlyphInfo>(infos, 0, count))
        let positions = Sealed.anyGlyphIn kernel.KernelPlan.GposCovered (ReadOnlySpan<GlyphInfo>(infos, 0, count))
        match substitutes, positions with
        | true, _ -> substituted kernel settings font scratch count admission positions
        | false, false ->
            Sealed.setMasksOf infos count kernel.FinalMask
            LeanDone count
        | false, true -> pairLane kernel settings font scratch count

    /// <summary>Shapes a line with the kernel in the arrays a worker owns.</summary>
    /// <param name="codec">The decoder.</param>
    /// <param name="kernel">The kernel.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="font">The font.</param>
    /// <param name="text">The text.</param>
    /// <param name="line">The offset and length of the line in the text.</param>
    /// <param name="count">The code points of the line.</param>
    /// <param name="direct">Whether the line decodes unit by unit.</param>
    /// <param name="scratch">The worker's arrays.</param>
    /// <returns>Whether the arrays hold the finished line, or the line the kernel's tails finished.</returns>
    let private leanLine<'T, 'C when 'C : struct and 'C :> ILineCodec<'T>> (codec: 'C) (kernel: LeanKernel) (settings: GlyphRun) (font: Font) (text: ReadOnlyMemory<'T>) (line: struct (int * int)) (count: int) (direct: bool) (scratch: LineScratch) =
        let struct (offset, length) = line
        match direct with
        | true -> codec.DirectInto(text.Span.Slice(offset, length), scratch.ScratchInfos, offset)
        | false -> Sealed.unfoldInto (text.Span.Slice(offset, length)) scratch.ScratchInfos count 0 (LineUnfolder<'T, 'C>(codec, settings.Replacement, offset))
        let metrics = font.Ot.Hmtx.Value.Table.Span
        let made = Sealed.simpleInPlace kernel.Glyphs kernel.Mask metrics kernel.KernelPlan.GsubCovered scratch.ScratchInfos scratch.ScratchProps scratch.ScratchPositions count
        match made, kernel.LookupFree with
        | ValueNone, _ -> LeanGeneral
        | ValueSome _, true -> LeanDone count
        | ValueSome hit, false -> withLookups kernel settings font scratch count hit

    /// <summary>The worker that shapes the line at an index.</summary>
    type private LineWork<'T, 'C when 'C : struct and 'C :> ILineCodec<'T>>(codec: 'C, font: Font, features: ImmutableArray<Feature>, shapers: ImmutableArray<string> voption, settings: GlyphRun, text: ReadOnlyMemory<'T>, lines: ImmutableArray<struct (int * int)>, facts: ImmutableArray<LineFacts>, starts: ImmutableArray<int>, resolved: ImmutableArray<ResolvedLine>, arenaInfos: GlyphInfo array, arenaPositions: GlyphPosition array) =
        /// <summary>Puts a shaped line in its window when it fits.</summary>
        /// <param name="shaped">The shaped line.</param>
        /// <param name="offset">The window of the line.</param>
        /// <param name="count">The code points of the line.</param>
        /// <returns>Where the line's glyphs are.</returns>
        member private _.Placed(shaped: Result<GlyphRun, ShapeError>, offset: int, count: int) =
            match shaped with
            | Error error -> Refused error
            | Ok run ->
                match offset >= 0 && run.Infos.Length <= count && run.Positions.Length = run.Infos.Length with
                | false -> Alone { LineInfos = run.Infos; LinePositions = run.Positions }
                | true ->
                    Sealed.copyShaped arenaInfos offset run.Infos
                    Sealed.copyShaped arenaPositions offset run.Positions
                    Arena (offset, run.Infos.Length)

        /// <summary>Puts a shaped line that is still in its working set in its window when it fits, copying the glyphs and the positions out
        /// of the set: no run is made of it.</summary>
        /// <param name="shaped">The shaped line.</param>
        /// <param name="offset">The window of the line.</param>
        /// <param name="count">The code points of the line.</param>
        /// <returns>Where the line's glyphs are.</returns>
        member private _.PlacedSet(shaped: Result<WorkingSet, ShapeError>, offset: int, count: int) =
            match shaped with
            | Error error -> Refused error
            | Ok set ->
                match offset >= 0 && set.Infos.Length <= count && set.Positions.Length = set.Infos.Length with
                | false -> Alone { LineInfos = set.Infos; LinePositions = set.Positions }
                | true ->
                    Sealed.copyShaped arenaInfos offset set.Infos
                    Sealed.copyShaped arenaPositions offset set.Positions
                    Arena (offset, set.Infos.Length)

        /// <summary>Shapes a line through the general route and puts the result in its window when it fits.</summary>
        /// <param name="resolvedLine">The plan of the line, whose seeded settings carry the guessed segment properties.</param>
        /// <param name="plan">The plan of the line.</param>
        /// <param name="offset">The window of the line.</param>
        /// <param name="line">The offset and length of the line in the text.</param>
        /// <param name="count">The code points of the line.</param>
        /// <returns>Where the line's glyphs are.</returns>
        member private this.General(resolvedLine: ResolvedLine, plan: ShapePlan, offset: int, line: struct (int * int), count: int) =
            let struct (start, length) = line
            match resolvedLine.Seeded.Flags &&& BufferFlags.Verify = BufferFlags.Default with
            | false ->
                let run = codec.Add(text.Span, start, length, resolvedLine.Seeded)
                this.Placed(Shape.executeAndVerify font features shapers (Shape.textCopy run) plan run, offset, count)
            | true ->
                match Execute.tryRunFresh plan font (codec.AddFresh(text.Span, start, length, resolvedLine.Seeded)) with
                | ValueSome shaped -> this.Placed(shaped, offset, count)
                | ValueNone -> this.Placed(Execute.runDeclined plan font features (codec.Add(text.Span, start, length, resolvedLine.Seeded)), offset, count)

        /// <summary>Shapes a line with its plan, by the kernel when it takes the line and by the general route otherwise.</summary>
        /// <param name="resolvedLine">The plan of the line.</param>
        /// <param name="plan">The plan.</param>
        /// <param name="offset">The window of the line.</param>
        /// <param name="line">The offset and length of the line in the text.</param>
        /// <param name="count">The code points of the line.</param>
        /// <param name="direct">Whether the line decodes unit by unit.</param>
        /// <param name="scratch">The worker's arrays.</param>
        /// <returns>Where the line's glyphs are.</returns>
        member private this.Planned(resolvedLine: ResolvedLine, plan: ShapePlan, offset: int, line: struct (int * int), count: int, direct: bool, scratch: LineScratch) =
            match resolvedLine.Kernel, offset >= 0 with
            | ValueNone, _ -> this.General(resolvedLine, plan, offset, line, count)
            | ValueSome _, false -> this.General(resolvedLine, plan, offset, line, count)
            | ValueSome kernel, true ->
                match leanLine codec kernel settings font text line count direct scratch with
                | LeanGeneral -> this.General(resolvedLine, plan, offset, line, count)
                | LeanShaped shaped -> this.Placed(shaped, offset, count)
                | LeanDone length ->
                    Sealed.copyWindow arenaInfos offset scratch.ScratchInfos length
                    Sealed.copyWindow arenaPositions offset scratch.ScratchPositions length
                    Arena (offset, length)

        /// <summary>Clears what a line left of its window, so that the arrays hold only glyphs and zeros.</summary>
        /// <param name="outcome">Where the line's glyphs are.</param>
        /// <param name="offset">The window of the line.</param>
        /// <param name="count">The size of the window.</param>
        /// <returns>The outcome.</returns>
        member private _.Cleared(outcome: LineOutcome, offset: int, count: int) =
            match offset < 0 with
            | true -> outcome
            | false ->
                let used = match outcome with | Arena (_, length) -> length | Alone _ -> 0 | Refused _ -> 0
                Sealed.clearWindow arenaInfos (offset + used) (count - used)
                Sealed.clearWindow arenaPositions (offset + used) (count - used)
                outcome

        interface IScratchWorker<LineOutcome> with
            member this.At (index, scratch) =
                let fact = facts[index]
                let count = fact.Count
                let offset = starts[index]
                match count with
                | 0 -> Arena (max offset 0, 0)
                | _ ->
                    let resolvedLine = resolved[indexOfScript resolved fact.FirstScript 0]
                    let struct (start, requested) = lines[index]
                    let struct (clampedStart, clampedLength) = clamp text.Length start requested
                    let outcome =
                        match resolvedLine.Plan with
                        | Error error -> Refused error
                        | Ok plan -> this.Planned(resolvedLine, plan, offset, struct (clampedStart, clampedLength), count, fact.Direct, scratch)
                    this.Cleared(outcome, offset, count)

    /// <summary>Looks at the line at an index.</summary>
    [<Struct>]
    type private FactsWorker<'T, 'C when 'C : struct and 'C :> ILineCodec<'T>> =
        { Codec: 'C
          Text: ReadOnlyMemory<'T>
          Replacement: uint32
          NeedsScript: bool
          Lines: ImmutableArray<struct (int * int)> }

        interface IIndexedWorker<LineFacts> with
            member this.At index = factsOf this.Codec this.Text this.Replacement this.NeedsScript this.Lines[index]

    /// <summary>The cost of looking at a line, its length in code units.</summary>
    /// <param name="textLength">The units of the text.</param>
    /// <param name="line">The offset and length asked for.</param>
    /// <returns>The units.</returns>
    let private unitsOf (textLength: int) (line: struct (int * int)) =
        let struct (start, requested) = line
        let struct (_, length) = clamp textLength start requested
        length

    /// <summary>Looks at every line, on up to a number of threads when the text is long enough.</summary>
    /// <param name="codec">The decoder.</param>
    /// <param name="sequentialBelow">The units below which the lines are looked at on the calling thread.</param>
    /// <param name="threads">The most threads at once.</param>
    /// <param name="replacement">The replacement.</param>
    /// <param name="needsScript">Whether the segment properties have no script.</param>
    /// <param name="text">The text.</param>
    /// <param name="lines">The offset and length of each line in the text.</param>
    /// <returns>The facts of each line.</returns>
    let private factsOfLines<'T, 'C when 'C : struct and 'C :> ILineCodec<'T>> (codec: 'C) (sequentialBelow: int) (threads: int) (replacement: uint32) (needsScript: bool) (text: ReadOnlyMemory<'T>) (lines: ImmutableArray<struct (int * int)>) =
        let worker = { Codec = codec; Text = text; Replacement = replacement; NeedsScript = needsScript; Lines = lines }
        let units = Sealed.init lines.Length (fun index -> unitsOf text.Length lines[index])
        let total = ParallelFill.totalCost (units.AsSpan())
        match threads = 1 || OperatingSystem.IsBrowser() || total < int64 sequentialBelow with
        | true -> ParallelFill.sequential lines.Length worker
        | false ->
            let chunkCost = int (max (int64 ParallelFill.MinChunkCost) (total / int64 (threads * ParallelFill.ChunksPerThread)))
            ParallelFill.chunked threads lines.Length (ParallelFill.chunkStarts (units.AsSpan()) chunkCost) worker

    /// <summary>The settings of every line: the run given without text.</summary>
    /// <param name="settings">The run.</param>
    /// <returns>The run with no glyphs, positions, properties or context.</returns>
    let private withoutText (settings: GlyphRun) =
        { settings with
            Infos = ImmutableArray<GlyphInfo>.Empty
            Props = ImmutableArray<GlyphProps>.Empty
            Positions = ImmutableArray<GlyphPosition>.Empty
            ContextBefore = ImmutableArray<uint32>.Empty
            ContextAfter = ImmutableArray<uint32>.Empty
            ContentType = NoContent
            HavePositions = false }

    /// <summary>Shapes the lines of a text into two arrays.</summary>
    /// <param name="codec">The decoder.</param>
    /// <param name="sequentialBelow">The glyph count below which the document is shaped on the calling thread.</param>
    /// <param name="degree">The most threads at once; any value below one means every core.</param>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="defaultLanguage">The language of a line whose settings have none.</param>
    /// <param name="settings">The settings of every line: the flags, cluster level, replacement, segment properties and limits of the
    /// run; its glyphs and context are ignored.</param>
    /// <param name="text">The text.</param>
    /// <param name="lines">The offset and length of each line in the text.</param>
    /// <returns>The shaped document and the cache with the plans the document needed.</returns>
    let private shapeLines<'T, 'C when 'C : struct and 'C :> ILineCodec<'T>> (codec: 'C) (sequentialBelow: int) (degree: int) (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (defaultLanguage: Language) (settings: GlyphRun) (text: ReadOnlyMemory<'T>) (lines: ImmutableArray<struct (int * int)>) =
        let settings = withoutText settings
        let needsScript = settings.SegmentProperties.Script = Scripts.Invalid
        let threads = match degree <= 0 with | true -> Environment.ProcessorCount | false -> degree
        let facts = factsOfLines codec sequentialBelow threads settings.Replacement needsScript text lines
        let struct (resolved, cache) = resolveFrom cache font features shapers defaultLanguage settings facts 0 ImmutableArray.Empty
        let counts = Sealed.init lines.Length (fun index -> facts[index].Count)
        let struct (starts, total) = Sealed.windowStarts (counts.AsSpan()) ArenaLimit
        let infosArray = Sealed.uninitialised<GlyphInfo> total
        let positionsArray = Sealed.uninitialised<GlyphPosition> total
        let worker = LineWork<'T, 'C>(codec, font, features, shapers, settings, text, lines, facts, starts, resolved, infosArray, positionsArray)
        let glyphs = ParallelFill.totalCost (counts.AsSpan())
        let outcomes =
            match threads = 1 || OperatingSystem.IsBrowser() || glyphs < int64 sequentialBelow with
            | true -> ScratchFill.sequential counts worker
            | false ->
                let chunkCost = int (max (int64 ParallelFill.MinChunkCost) (glyphs / int64 (threads * ParallelFill.ChunksPerThread)))
                ScratchFill.chunked threads counts (ParallelFill.chunkStarts (counts.AsSpan()) chunkCost) worker
        struct ({ ArenaInfos = Sealed.publishArray infosArray; ArenaPositions = Sealed.publishArray positionsArray; Lines = outcomes }, cache)

    /// <summary>Shapes the lines of a UTF-8 text, each line equal in glyphs and positions to <c>GlyphRun.addUtf8</c> of the line with
    /// <c>GlyphRun.guessSegmentProperties</c> through <c>Shape.shapeCached</c>, with the threshold of the parallel path given.</summary>
    /// <param name="sequentialBelow">The glyph count below which the document is shaped on the calling thread.</param>
    /// <param name="degree">The most threads at once; any value below one means every core.</param>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="defaultLanguage">The language of a line whose settings have none.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="text">The text.</param>
    /// <param name="lines">The offset and length of each line in the text.</param>
    /// <returns>The shaped document and the cache with the plans the document needed.</returns>
    let internal utf8From (sequentialBelow: int) (degree: int) (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (defaultLanguage: Language) (settings: GlyphRun) (text: ReadOnlyMemory<byte>) (lines: ImmutableArray<struct (int * int)>) =
        shapeLines (Utf8Lines()) sequentialBelow degree cache font features shapers defaultLanguage settings text lines

    /// <summary>Shapes the lines of a UTF-8 text with the measured threshold <c>Shape.SequentialBelowGlyphs</c>.</summary>
    /// <param name="degree">The most threads at once; any value below one means every core.</param>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="defaultLanguage">The language of a line whose settings have none.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="text">The text.</param>
    /// <param name="lines">The offset and length of each line in the text.</param>
    /// <returns>The shaped document and the cache with the plans the document needed.</returns>
    let utf8 (degree: int) (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (defaultLanguage: Language) (settings: GlyphRun) (text: ReadOnlyMemory<byte>) (lines: ImmutableArray<struct (int * int)>) =
        utf8From Shape.SequentialBelowGlyphs degree cache font features shapers defaultLanguage settings text lines

    /// <summary>Shapes the lines of a UTF-16 text, see <see cref="utf8From"/>.</summary>
    /// <param name="sequentialBelow">The glyph count below which the document is shaped on the calling thread.</param>
    /// <param name="degree">The most threads at once; any value below one means every core.</param>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="defaultLanguage">The language of a line whose settings have none.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="text">The text.</param>
    /// <param name="lines">The offset and length of each line in the text.</param>
    /// <returns>The shaped document and the cache with the plans the document needed.</returns>
    let internal utf16From (sequentialBelow: int) (degree: int) (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (defaultLanguage: Language) (settings: GlyphRun) (text: ReadOnlyMemory<uint16>) (lines: ImmutableArray<struct (int * int)>) =
        shapeLines (Utf16Lines()) sequentialBelow degree cache font features shapers defaultLanguage settings text lines

    /// <summary>Shapes the lines of a UTF-16 text with the measured threshold.</summary>
    /// <param name="degree">The most threads at once; any value below one means every core.</param>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="defaultLanguage">The language of a line whose settings have none.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="text">The text.</param>
    /// <param name="lines">The offset and length of each line in the text.</param>
    /// <returns>The shaped document and the cache with the plans the document needed.</returns>
    let utf16 (degree: int) (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (defaultLanguage: Language) (settings: GlyphRun) (text: ReadOnlyMemory<uint16>) (lines: ImmutableArray<struct (int * int)>) =
        utf16From Shape.SequentialBelowGlyphs degree cache font features shapers defaultLanguage settings text lines

    /// <summary>Shapes the lines of a UTF-32 text, see <see cref="utf8From"/>.</summary>
    /// <param name="sequentialBelow">The glyph count below which the document is shaped on the calling thread.</param>
    /// <param name="degree">The most threads at once; any value below one means every core.</param>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="defaultLanguage">The language of a line whose settings have none.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="text">The text.</param>
    /// <param name="lines">The offset and length of each line in the text.</param>
    /// <returns>The shaped document and the cache with the plans the document needed.</returns>
    let internal utf32From (sequentialBelow: int) (degree: int) (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (defaultLanguage: Language) (settings: GlyphRun) (text: ReadOnlyMemory<uint32>) (lines: ImmutableArray<struct (int * int)>) =
        shapeLines (Utf32Lines()) sequentialBelow degree cache font features shapers defaultLanguage settings text lines

    /// <summary>Shapes the lines of a UTF-32 text with the measured threshold.</summary>
    /// <param name="degree">The most threads at once; any value below one means every core.</param>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="defaultLanguage">The language of a line whose settings have none.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="text">The text.</param>
    /// <param name="lines">The offset and length of each line in the text.</param>
    /// <returns>The shaped document and the cache with the plans the document needed.</returns>
    let utf32 (degree: int) (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (defaultLanguage: Language) (settings: GlyphRun) (text: ReadOnlyMemory<uint32>) (lines: ImmutableArray<struct (int * int)>) =
        utf32From Shape.SequentialBelowGlyphs degree cache font features shapers defaultLanguage settings text lines

    /// <summary>Shapes the lines of a text of code points that are not validated, see <see cref="utf8From"/>.</summary>
    /// <param name="sequentialBelow">The glyph count below which the document is shaped on the calling thread.</param>
    /// <param name="degree">The most threads at once; any value below one means every core.</param>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="defaultLanguage">The language of a line whose settings have none.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="text">The code points.</param>
    /// <param name="lines">The offset and length of each line in the text.</param>
    /// <returns>The shaped document and the cache with the plans the document needed.</returns>
    let internal codepointsFrom (sequentialBelow: int) (degree: int) (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (defaultLanguage: Language) (settings: GlyphRun) (text: ReadOnlyMemory<uint32>) (lines: ImmutableArray<struct (int * int)>) =
        shapeLines (CodepointLines()) sequentialBelow degree cache font features shapers defaultLanguage settings text lines

    /// <summary>Shapes the lines of a text of code points with the measured threshold.</summary>
    /// <param name="degree">The most threads at once; any value below one means every core.</param>
    /// <param name="cache">The plan cache.</param>
    /// <param name="font">The font.</param>
    /// <param name="features">The user features.</param>
    /// <param name="shapers">The requested shapers, or none for the default list.</param>
    /// <param name="defaultLanguage">The language of a line whose settings have none.</param>
    /// <param name="settings">The settings of every line.</param>
    /// <param name="text">The code points.</param>
    /// <param name="lines">The offset and length of each line in the text.</param>
    /// <returns>The shaped document and the cache with the plans the document needed.</returns>
    let codepoints (degree: int) (cache: PlanCache) (font: Font) (features: ImmutableArray<Feature>) (shapers: ImmutableArray<string> voption) (defaultLanguage: Language) (settings: GlyphRun) (text: ReadOnlyMemory<uint32>) (lines: ImmutableArray<struct (int * int)>) =
        codepointsFrom Shape.SequentialBelowGlyphs degree cache font features shapers defaultLanguage settings text lines

    /// <summary>Whether no glyph of a line carries a mask or private storage, so that the line is as a decoder makes it.</summary>
    /// <param name="input">The glyphs.</param>
    /// <param name="index">The next glyph.</param>
    /// <returns>True when every mask and both private fields are zero.</returns>
    [<TailCall>]
    let rec private asDecoded (input: ReadOnlySpan<GlyphInfo>) (index: int) =
        match index >= input.Length with
        | true -> true
        | false ->
            match input[index].Mask = 0u && input[index].Var1 = 0u && input[index].Var2 = 0u with
            | false -> false
            | true -> asDecoded input (index + 1)

    /// <summary>The run a caller's glyphs make with their properties zeroed: the run <c>Shape.shapeCached</c> is given for them.</summary>
    /// <param name="settings">The run that holds the segment properties, contexts, flags and limits, without glyphs.</param>
    /// <param name="input">The glyphs, taken as they are.</param>
    /// <returns>The run.</returns>
    let private runOfInput (settings: GlyphRun) (input: ReadOnlySpan<GlyphInfo>) =
        { settings with
            Infos = Sealed.ofSpan input
            Props = Sealed.zeroed<GlyphProps> input.Length
            ContentType = UnicodeContent }

    /// <summary>Puts a shaped run in the arrays of the caller when they are long enough, and writes nothing otherwise.</summary>
    /// <param name="shaped">The shaped run, or why there is none.</param>
    /// <param name="infos">The array for the glyphs.</param>
    /// <param name="positions">The array for the positions.</param>
    /// <returns>Where the shaped line is.</returns>
    let private placedInto (shaped: Result<GlyphRun, ShapeError>) (infos: Span<GlyphInfo>) (positions: Span<GlyphPosition>) =
        match shaped with
        | Error error -> IntoRefused error
        | Ok run ->
            let count = run.Infos.Length
            let positionCount = run.Positions.Length
            match count <= infos.Length && positionCount <= positions.Length with
            | false -> IntoTooShort (max count positionCount)
            | true ->
                CallerArrays.copyInto (run.Infos.AsSpan()) infos
                CallerArrays.copyInto (run.Positions.AsSpan()) positions
                IntoShaped (count, positionCount, run.RandomState, run.Successful)

    /// <summary>Shapes glyphs the caller holds, code points with their clusters, with a plan into arrays the caller owns, the result of
    /// <c>Shape.shapeCached</c> on the run the glyphs make under the settings (the verify flag aside, which this entry does not act on): glyphs
    /// that no decoder would have made, with a mask or private storage set, are shaped as the run that holds them; glyphs as a decoder makes
    /// them are shaped by <c>Execute.tryRunFresh</c>, which writes into a copy the call made, and a line it declines is shaped from a copy
    /// again by <c>Execute.runDeclined</c>. The arrays may be the array the glyphs are in: the glyphs are copied first and the result is
    /// written last, and only when both arrays are long enough, so that a caller who is told how long they must be loses nothing.</summary>
    /// <param name="plan">The plan, resolved for the segment properties of the settings.</param>
    /// <param name="font">The font.</param>
    /// <param name="settings">The run that holds the segment properties, contexts, flags, cluster level, limits and random state, with no glyphs and no properties.</param>
    /// <param name="input">The glyphs to shape: code points and clusters.</param>
    /// <param name="infos">The array for the shaped glyphs, written from its first element.</param>
    /// <param name="positions">The array for the positions, written from its first element.</param>
    /// <returns>The counts and the run's state, the length the arrays need, or why the line could not be shaped.</returns>
    let internal shapeInto (plan: ShapePlan) (font: Font) (settings: GlyphRun) (input: ReadOnlySpan<GlyphInfo>) (infos: Span<GlyphInfo>) (positions: Span<GlyphPosition>) =
        match input.Length with
        | 0 -> IntoShaped (0, 0, settings.RandomState, settings.Successful)
        | _ ->
            let shaped =
                match asDecoded input 0 with
                | false -> Execute.run plan font ImmutableArray<Feature>.Empty (runOfInput settings input)
                | true ->
                    match Execute.tryRunFresh plan font { settings with Infos = Sealed.ofSpan input; ContentType = UnicodeContent } with
                    | ValueSome fresh -> fresh
                    | ValueNone -> Execute.runDeclined plan font ImmutableArray<Feature>.Empty (runOfInput settings input)
            placedInto shaped infos positions |> Settled.value

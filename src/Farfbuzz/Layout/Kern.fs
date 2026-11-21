namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The kern machine and its application over a run, <c>hb_kern_machine_t</c> and <c>KerxTable::apply</c>.</summary>
[<RequireQualifiedAccess>]
module internal Kern =
    /// <summary>The cursive attachment type, <c>ATTACH_TYPE_CURSIVE</c>.</summary>
    [<Literal>]
    let private AttachTypeCursive = 0x02u

    /// <summary>Adds two positions with int saturation, <c>hb_saturate_add</c>.</summary>
    /// <param name="a">The augend.</param>
    /// <param name="b">The addend.</param>
    /// <returns>The saturating sum.</returns>
    let private saturateAdd (a: int) (b: int) : int =
        int (Math.Clamp(int64 a + int64 b, int64 Int32.MinValue, int64 Int32.MaxValue))

    /// <summary>A pair-kerning provider for the kern machine, the <c>Driver</c> of <c>hb_kern_machine_t</c>.</summary>
    type IKernProvider =
        /// <summary>Returns the unscaled kerning between two glyphs, zero when there is none.</summary>
        /// <param name="left">The left glyph.</param>
        /// <param name="right">The right glyph.</param>
        /// <returns>The kerning.</returns>
        abstract GetKerning : left: uint32 * right: uint32 -> int
        /// <summary>Whether a glyph can be the left glyph of a kerned pair: false only when the kerning of every pair it starts
        /// is zero, so that a scan may pass it without asking for the pair.</summary>
        /// <param name="left">The glyph.</param>
        /// <returns>False when no pair starting with the glyph kerns.</returns>
        abstract MayKern : left: uint32 -> bool

    /// <summary>The pair provider of the legacy kern table, <c>KernSubTable::get_kerning</c>.</summary>
    [<Struct>]
    type private LegacyKernProvider =
        val Subtable: KernSubtable
        new (subtable) = { Subtable = subtable }
        interface IKernProvider with
            member this.GetKerning (left, right) = KernTable.getKerning this.Subtable left right
            member _.MayKern _ = true

    /// <summary>The pair provider of a format 0 subtable whose pairs compiled into a hashed index: a small struct, so that the
    /// scan copies none of the subtable.</summary>
    [<Struct>]
    type internal IndexedPairProvider =
        val Index: PairIndex
        new (index) = { Index = index }
        interface IKernProvider with
            member this.GetKerning (left, right) = Farfbuzz.Aat.PairLookup.find this.Index left right
            member this.MayKern left = Farfbuzz.Aat.PairLookup.hasLeft this.Index left

    /// <summary>The matcher of the kern machine, <c>hb_ot_apply_context_t</c> with <c>IgnoreMarks</c>.</summary>
    /// <param name="kernMask">The kern feature mask.</param>
    /// <returns>The matcher.</returns>
    let private machineMatcher (kernMask: uint32) : Matcher =
        { LookupProps = LookupFlag.IgnoreMarks
          Mask = kernMask
          IgnoreZwnj = true
          IgnoreZwj = true
          IgnoreHidden = true
          PerSyllable = false
          Syllable = 0uy }

    /// <summary>The state of the kern pass: the scan cursor, or the edits of one kerned pair.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private KernState =
        | KernScan of index: int
        | KernApply of index: int * pair: int * position: GlyphPosition * phase: int
        | KernDone

    /// <summary>The positions of a kerned pair after the kerning: across the stream the second glyph moves only.</summary>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="kern">The unscaled kern value.</param>
    /// <param name="posI">The left glyph's position.</param>
    /// <param name="posJ">The right glyph's position.</param>
    /// <returns>The two positions.</returns>
    let private kernedPositions (font: Font) (horizontal: bool) (crossStream: bool) (kern: int) (posI: GlyphPosition) (posJ: GlyphPosition) =
        let scaled = if horizontal then Font.emScaleX font kern else Font.emScaleY font kern
        match crossStream with
        | false ->
            let kern1 = scaled >>> 1
            let kern2 = scaled - kern1
            match horizontal with
            | true ->
                struct ({ posI with XAdvance = saturateAdd posI.XAdvance kern1 },
                        { posJ with XAdvance = saturateAdd posJ.XAdvance kern2; XOffset = saturateAdd posJ.XOffset kern2 })
            | false ->
                struct ({ posI with YAdvance = saturateAdd posI.YAdvance kern1 },
                        { posJ with YAdvance = saturateAdd posJ.YAdvance kern2; YOffset = saturateAdd posJ.YOffset kern2 })
        | true ->
            match horizontal with
            | true -> struct (posI, { posJ with YOffset = scaled })
            | false -> struct (posI, { posJ with XOffset = scaled })

    /// <summary>Applies the position and flag edits of one kerned pair.</summary>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The left-glyph index.</param>
    /// <param name="pair">The right-glyph index.</param>
    /// <param name="kern">The unscaled kern value.</param>
    /// <returns>The first edit and the next state.</returns>
    let private applyPairStep (font: Font) (horizontal: bool) (crossStream: bool) (positions: ReadOnlySpan<GlyphPosition>) (index: int) (pair: int) (kern: int) : struct (RunEdit * KernState) =
        let struct (newI, newJ) = kernedPositions font horizontal crossStream kern positions[index] positions[pair]
        match crossStream with
        | false -> struct (RunEdit.SetPosition (index, newI), KernApply (index, pair, newJ, 1))
        | true -> struct (RunEdit.SetPosition (pair, newJ), KernApply (index, pair, newJ, 2))

    /// <summary>Computes a pair's kern value and applies it when non-zero.</summary>
    /// <typeparam name="'P">The pair provider.</typeparam>
    /// <param name="provider">The pair provider.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="input">The glyph infos.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The left-glyph index.</param>
    /// <param name="pair">The right-glyph index.</param>
    /// <returns>The next edit and state.</returns>
    let private kernPairStep<'P when 'P : struct and 'P :> IKernProvider> (provider: 'P) (font: Font) (horizontal: bool) (crossStream: bool) (input: ReadOnlySpan<GlyphInfo>) (positions: ReadOnlySpan<GlyphPosition>) (index: int) (pair: int) =
        let kern = provider.GetKerning (input[index].Codepoint, input[pair].Codepoint)
        match kern = 0 with
        | true -> struct (RunEdit.NextGlyphs (pair - index), KernScan pair)
        | false -> applyPairStep font horizontal crossStream positions index pair kern

    /// <summary>Finds the next pair to the right of a glyph under the kern mask.</summary>
    /// <typeparam name="'P">The pair provider.</typeparam>
    /// <param name="matcher">The skip matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="provider">The pair provider.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="input">The glyph infos.</param>
    /// <param name="inputProps">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The left-glyph index.</param>
    /// <returns>The next edit and state.</returns>
    let private scanFromPair<'P when 'P : struct and 'P :> IKernProvider> (matcher: Matcher) (gdef: Gdef) (provider: 'P) (font: Font) (horizontal: bool) (crossStream: bool) (input: ReadOnlySpan<GlyphInfo>) (inputProps: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) (index: int) =
        match Matching.next matcher gdef (NoMatchFunc()) 0 input inputProps index input.Length with
        | Miss finish -> struct (RunEdit.NextGlyphs (finish - index), KernScan finish)
        | Hit pair -> kernPairStep provider font horizontal crossStream input positions index pair

    /// <summary>Scans from one glyph, skipping glyphs outside the kern mask.</summary>
    /// <typeparam name="'P">The pair provider.</typeparam>
    /// <param name="matcher">The skip matcher.</param>
    /// <param name="gdef">The GDEF.</param>
    /// <param name="provider">The pair provider.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="input">The glyph infos.</param>
    /// <param name="inputProps">The glyph properties.</param>
    /// <param name="positions">The glyph positions.</param>
    /// <param name="index">The next left-glyph index.</param>
    /// <returns>The next edit and state.</returns>
    let private scanStep<'P when 'P : struct and 'P :> IKernProvider> (matcher: Matcher) (gdef: Gdef) (provider: 'P) (font: Font) (horizontal: bool) (crossStream: bool) (input: ReadOnlySpan<GlyphInfo>) (inputProps: ReadOnlySpan<GlyphProps>) (positions: ReadOnlySpan<GlyphPosition>) (index: int) =
        match index >= input.Length with
        | true -> struct (RunEdit.Finish, KernDone)
        | false ->
            match input[index].Mask &&& matcher.Mask = 0u with
            | true -> struct (RunEdit.NextGlyph, KernScan (index + 1))
            | false -> scanFromPair matcher gdef provider font horizontal crossStream input inputProps positions index

    /// <summary>Replays the position, flag and advance edits of one kerned pair.</summary>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="index">The left-glyph index.</param>
    /// <param name="pair">The right-glyph index.</param>
    /// <param name="position">The right glyph's updated position.</param>
    /// <param name="phase">The next edit phase.</param>
    /// <returns>The next edit and state.</returns>
    let private applyStep (crossStream: bool) (index: int) (pair: int) (position: GlyphPosition) (phase: int) : struct (RunEdit * KernState) =
        match crossStream, phase with
        | false, 1 -> struct (RunEdit.SetPosition (pair, position), KernApply (index, pair, position, 2))
        | false, 2 -> struct (RunEdit.UnsafeToBreak (index, pair + 1), KernApply (index, pair, position, 3))
        | false, _ -> struct (RunEdit.NextGlyphs (pair - index), KernScan pair)
        | true, 2 -> struct (RunEdit.AddScratch ScratchFlags.HasGposAttachment, KernApply (index, pair, position, 3))
        | true, 3 -> struct (RunEdit.UnsafeToBreak (index, pair + 1), KernApply (index, pair, position, 4))
        | true, _ -> struct (RunEdit.NextGlyphs (pair - index), KernScan pair)

    /// <summary>The pass that applies one subtable's kern machine, <c>hb_kern_machine_t::kern</c>.</summary>
    [<Struct>]
    type private KernPass<'P when 'P : struct and 'P :> IKernProvider> =
        val Matcher: Matcher
        val Gdef: Gdef
        val Provider: 'P
        val Font: Font
        val Horizontal: bool
        val CrossStream: bool

        /// <summary>Creates a kern pass.</summary>
        /// <param name="matcher">The skip matcher.</param>
        /// <param name="gdef">The GDEF.</param>
        /// <param name="provider">The pair provider.</param>
        /// <param name="font">The font.</param>
        /// <param name="horizontal">Whether the run is horizontal.</param>
        /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
        new (matcher, gdef, provider, font, horizontal, crossStream) =
            { Matcher = matcher
              Gdef = gdef
              Provider = provider
              Font = font
              Horizontal = horizontal
              CrossStream = crossStream }

        interface IRunPass<KernState> with
            member this.Step (cursor, state) =
                match state with
                | KernDone -> struct (RunEdit.Finish, KernDone)
                | KernScan index -> scanStep this.Matcher this.Gdef this.Provider this.Font this.Horizontal this.CrossStream cursor.Input cursor.InputProps cursor.Positions index
                | KernApply (index, pair, position, phase) -> applyStep this.CrossStream index pair position phase

    /// <summary>Whether the kern matcher takes the glyph at once: it is no mark and no default ignorable, so that
    /// <c>skipping_iterator_t::next</c> neither skips it nor reads more than its mask.</summary>
    /// <param name="props">The glyph's properties.</param>
    /// <returns>True for a glyph the matcher neither skips nor ignores.</returns>
    let private isPlain (props: GlyphProps) =
        props.GlyphClass &&& GlyphPropsBits.Mark = 0us && not (UnicodeProps.isDefaultIgnorable props)

    /// <summary>The decisions of the kern kernel: the scan of <see cref="scanStep"/> and the kerning of
    /// <see cref="kernedPositions"/>.</summary>
    [<Struct; NoComparison; NoEquality; System.Runtime.CompilerServices.IsReadOnly>]
    type private KernDecisions<'P when 'P : struct and 'P :> IKernProvider> =
        { Matcher: Matcher
          Gdef: Gdef
          Provider: 'P
          Font: Font
          Horizontal: bool
          Cross: bool }

        interface IKernMachine with
            member this.CrossStream = this.Cross
            member this.Scan (infos, props, index) =
                match infos[index].Mask &&& this.Matcher.Mask = 0u with
                | true -> struct (index + 1, 0, 0)
                | false ->
                    let next = index + 1
                    match next < infos.Length && isPlain props[next] with
                    | true ->
                        match infos[next].Mask &&& this.Matcher.Mask = 0u, this.Provider.MayKern infos[index].Codepoint with
                        | true, _ -> struct (next + 1, 0, 0)
                        | false, false -> struct (next, 0, 0)
                        | false, true -> struct (next, next, this.Provider.GetKerning (infos[index].Codepoint, infos[next].Codepoint))
                    | false ->
                        match Matching.next this.Matcher this.Gdef (NoMatchFunc()) 0 infos props index infos.Length with
                        | Miss finish -> struct (finish, 0, 0)
                        | Hit pair ->
                            let kern = this.Provider.GetKerning (infos[index].Codepoint, infos[pair].Codepoint)
                            struct (pair, pair, kern)
            member this.Apply (kern, first, second) = kernedPositions this.Font this.Horizontal this.Cross kern first second

    /// <summary>Marks the whole run unsafe to concat, <c>KerxTable::apply</c>'s first line.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run with the flag set, unchanged when the flag is not produced.</returns>
    let private markUnsafeToConcat (run: GlyphRun) : GlyphRun =
        match run.Flags &&& BufferFlags.ProduceUnsafeToConcat = BufferFlags.Default with
        | true -> run
        | false ->
            let infos = Sealed.mapi (fun _ (info: GlyphInfo) -> { info with Mask = info.Mask ||| uint32 GlyphFlags.UnsafeToConcat }) (run.Infos.AsSpan())
            { run with Infos = infos }

    /// <summary>Attaches every glyph into a cursive chain, <c>KerxTable::apply</c>'s cross-stream init.</summary>
    /// <param name="run">The run.</param>
    /// <param name="direction">The direction.</param>
    /// <returns>The run with the chain set.</returns>
    let private initChain (run: GlyphRun) (direction: Direction) : GlyphRun =
        let chain = if Direction.isBackward direction then 1s else -1s
        let var = (AttachTypeCursive <<< 16) ||| uint32 (uint16 chain)
        let positions = Sealed.mapi (fun _ (position: GlyphPosition) -> { position with Var = position.Var ||| var }) (run.Positions.AsSpan())
        { run with Positions = positions }

    /// <summary>Runs one subtable's kern machine over a pair provider, <c>hb_kern_machine_t::kern</c>, on the kernel or on the
    /// interpreter that stays as the reference.</summary>
    /// <typeparam name="'P">The pair provider.</typeparam>
    /// <param name="runner">Which implementation runs the machine.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="provider">The pair provider.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the subtable.</returns>
    let internal applyProviderWith<'P when 'P : struct and 'P :> IKernProvider> (runner: Farfbuzz.Aat.MachineRunner) (plan: OtShapePlan) (font: Font) (horizontal: bool) (crossStream: bool) (provider: 'P) (run: GlyphRun) =
        match plan.RequestedKerning with
        | false -> run
        | true ->
            let matcher = machineMatcher plan.KernMask
            match runner with
            | Farfbuzz.Aat.Reference ->
                GlyphRun.runPass (KernPass<'P>(matcher, font.Ot.Gdef.Value, provider, font, horizontal, crossStream)) (KernScan 0) run
            | Farfbuzz.Aat.Kernel ->
                let decisions: KernDecisions<'P> = { Matcher = matcher; Gdef = font.Ot.Gdef.Value; Provider = provider; Font = font; Horizontal = horizontal; Cross = crossStream }
                let result = Sealed.runKernPass (Farfbuzz.Aat.AatKernel.inputOf run) decisions
                Farfbuzz.Aat.AatKernel.resultOf run result

    /// <summary>Runs one subtable's kern machine over a pair provider, <c>hb_kern_machine_t::kern</c>.</summary>
    /// <typeparam name="'P">The pair provider.</typeparam>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="provider">The pair provider.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the subtable.</returns>
    let internal applyProvider<'P when 'P : struct and 'P :> IKernProvider> (plan: OtShapePlan) (font: Font) (horizontal: bool) (crossStream: bool) (provider: 'P) (run: GlyphRun) =
        applyProviderWith Farfbuzz.Aat.Kernel plan font horizontal crossStream provider run

    /// <summary>Runs one subtable's kern machine over a pair provider on arrays the caller made in this call and shares with no one:
    /// the kerned positions and the flags are written in place, <c>hb_kern_machine_t::kern</c>.</summary>
    /// <typeparam name="'P">The pair provider.</typeparam>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="provider">The pair provider.</param>
    /// <param name="run">The run, whose infos, properties and positions are the caller's own.</param>
    /// <returns>The run after the subtable.</returns>
    let internal applyProviderOwned<'P when 'P : struct and 'P :> IKernProvider> (plan: OtShapePlan) (font: Font) (horizontal: bool) (crossStream: bool) (provider: 'P) (run: GlyphRun) =
        match plan.RequestedKerning with
        | false -> run
        | true ->
            let decisions: KernDecisions<'P> = { Matcher = machineMatcher plan.KernMask; Gdef = font.Ot.Gdef.Value; Provider = provider; Font = font; Horizontal = horizontal; Cross = crossStream }
            Farfbuzz.Aat.AatKernel.resultOf run (Sealed.runKernPassOwned (Farfbuzz.Aat.AatKernel.inputOf run) decisions)

    /// <summary>Runs one legacy kern subtable's kern machine, <c>KernSubTable::dispatch</c>.</summary>
    /// <param name="owned">Whether the run's arrays are the caller's own, so that a pair kern writes them in place.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="crossStream">Whether the subtable kerns across the stream.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after the subtable.</returns>
    let private applyMachine (owned: bool) (plan: OtShapePlan) (font: Font) (horizontal: bool) (crossStream: bool) (subtable: KernSubtable) (run: GlyphRun) =
        match subtable with
        | KernMachine (data, _) ->
            let spec : Farfbuzz.Aat.Format1Spec =
                { Extended = false
                  TableBytes = data.Table
                  RangeEnd = data.RangeEnd
                  MachineAt = data.MachineAt
                  KernActionAt = data.KernActionAt
                  TupleCount = 0u }
            Farfbuzz.Aat.KerxStateMachineApply.apply spec data.Machine plan font horizontal crossStream run
        | KernUnsupported _ -> run
        | KernPairs (_, _, compiled) ->
            match compiled.Value, owned with
            | ValueSome index, true -> applyProviderOwned plan font horizontal crossStream (IndexedPairProvider(index)) run
            | ValueSome index, false -> applyProvider plan font horizontal crossStream (IndexedPairProvider(index)) run
            | ValueNone, true -> applyProviderOwned plan font horizontal crossStream (LegacyKernProvider(subtable)) run
            | ValueNone, false -> applyProvider plan font horizontal crossStream (LegacyKernProvider(subtable)) run
        | KernClasses _ | KernIndex _ ->
            match owned with
            | true -> applyProviderOwned plan font horizontal crossStream (LegacyKernProvider(subtable)) run
            | false -> applyProvider plan font horizontal crossStream (LegacyKernProvider(subtable)) run

    /// <summary>Whether the run holds a glyph the subtable can act on, <c>buffer_intersects_machine</c>; a state
    /// machine is gated by its initial glyph set, the other kinds are not gated.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="run">The run.</param>
    /// <returns>True when the subtable is to be applied.</returns>
    let private intersectsMachine (subtable: KernSubtable) (run: GlyphRun) =
        match subtable with
        | KernMachine (data, _) -> Farfbuzz.Aat.MachineGate.scanIntersects (run.Infos.AsSpan()) (data.FirstSet.AsSpan()) 0
        | KernPairs _ | KernClasses _ | KernIndex _ | KernUnsupported _ -> true

    /// <summary>Charges the buffer pass of a state machine subtable, <c>charge_buffer_pass</c>; the other kinds are
    /// not charged.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run with the pass charged.</returns>
    let private chargePass (subtable: KernSubtable) (run: GlyphRun) =
        match subtable with
        | KernMachine _ -> Farfbuzz.Aat.AatApply.chargeBufferPass run
        | KernPairs _ | KernClasses _ | KernIndex _ | KernUnsupported _ -> run

    /// <summary>Applies one charged subtable: the cross-stream chain first, then the subtable.</summary>
    /// <param name="owned">Whether the run's arrays are the caller's own.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="direction">The direction.</param>
    /// <param name="info">The subtable's flags.</param>
    /// <param name="subtable">The subtable.</param>
    /// <param name="seenCrossStream">Whether a cross-stream subtable already ran.</param>
    /// <param name="run">The charged run.</param>
    /// <returns>The run after the subtable.</returns>
    let private applyCharged (owned: bool) (plan: OtShapePlan) (font: Font) (horizontal: bool) (direction: Direction) (info: KernSubtableInfo) (subtable: KernSubtable) (seenCrossStream: bool) (run: GlyphRun) =
        let chained =
            match info.CrossStream && not seenCrossStream with
            | true -> initChain run direction
            | false -> run
        applyMachine owned plan font horizontal info.CrossStream subtable chained

    /// <summary>Applies the subtables in order, <c>KerxTable::apply</c>'s loop.</summary>
    /// <param name="owned">Whether the run's arrays are the caller's own.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="horizontal">Whether the run is horizontal.</param>
    /// <param name="direction">The direction.</param>
    /// <param name="subtables">The subtables.</param>
    /// <param name="index">The next subtable.</param>
    /// <param name="seenCrossStream">Whether a cross-stream subtable already ran.</param>
    /// <param name="run">The run.</param>
    /// <returns>The run after every subtable.</returns>
    [<TailCall>]
    let rec private applyFrom (owned: bool) (plan: OtShapePlan) (font: Font) (horizontal: bool) (direction: Direction) (subtables: ImmutableArray<KernSubtable>) (index: int) (seenCrossStream: bool) (run: GlyphRun) =
        match index >= subtables.Length || not run.Successful with
        | true -> run
        | false ->
            let subtable = subtables[index]
            let info = KernTable.subtableInfoOf subtable
            match info.Variation || info.Horizontal <> horizontal || not (intersectsMachine subtable run) with
            | true -> applyFrom owned plan font horizontal direction subtables (index + 1) seenCrossStream run
            | false ->
                let charged = chargePass subtable run
                match charged.Successful with
                | false -> charged
                | true ->
                    let applied = applyCharged owned plan font horizontal direction info subtable seenCrossStream charged
                    applyFrom owned plan font horizontal direction subtables (index + 1) (seenCrossStream || info.CrossStream) applied

    /// <summary>Applies the legacy kern table, <c>hb_ot_layout_kern</c>, with the arrays of the run the caller's own or not.</summary>
    /// <param name="owned">Whether the run's arrays were made by the caller in this call and are shared with no one, so that the
    /// unsafe-to-concat marks and the kerned pairs are written in place.</param>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The kerned run.</returns>
    let private applyKernWith (owned: bool) (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        let kern = font.Ot.Kern.Value
        match kern.HasData with
        | false -> run
        | true ->
            let direction = run.SegmentProperties.Direction
            let horizontal = Direction.isHorizontal direction
            let reversed = Direction.isBackward direction
            let marked =
                match owned && run.Flags &&& BufferFlags.ProduceUnsafeToConcat <> BufferFlags.Default with
                | true ->
                    Sealed.markConcatOwned run.Infos
                    run
                | false -> markUnsafeToConcat run
            let turned = if reversed then GlyphRun.reverse marked else marked
            let applied = applyFrom owned plan font horizontal direction kern.Subtables 0 false turned
            if reversed then GlyphRun.reverse applied else applied

    /// <summary>Applies the legacy kern table, <c>hb_ot_layout_kern</c>.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The kerned run.</returns>
    let applyKern (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        applyKernWith false plan font run

    /// <summary>Applies the legacy kern table, <c>hb_ot_layout_kern</c>, on a run whose infos, properties and positions the caller made
    /// in this call and shares with no one: a pair that kerns writes them in place instead of copying them.</summary>
    /// <param name="plan">The plan.</param>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The kerned run.</returns>
    let applyKernOwned (plan: OtShapePlan) (font: Font) (run: GlyphRun) =
        applyKernWith true plan font run

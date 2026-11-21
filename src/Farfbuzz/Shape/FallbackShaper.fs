namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>The fallback shaper, <c>_hb_fallback_shape</c>: nominal glyphs, advances and origins, no layout.</summary>
[<RequireQualifiedAccess>]
module internal FallbackShaper =
    /// <summary>The space code point whose glyph replaces default ignorables, <c>hb-fallback-shape.cc</c>'s <c>' '</c>.</summary>
    [<Literal>]
    let private Space = 0x0020u

    /// <summary>The glyph a missing code point becomes, <c>get_nominal_glyph</c>'s zeroed output.</summary>
    [<Literal>]
    let private NotFoundGlyph = 0u

    /// <summary>A zeroed glyph position.</summary>
    let private zeroPosition =
        { XAdvance = 0; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }

    /// <summary>Whether a code point is replaced by the space glyph, <c>has_space</c> and <c>is_default_ignorable</c>.</summary>
    /// <param name="spaceGlyph">The space glyph, when the font maps one.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>True when the glyph becomes the space glyph with zero advances.</returns>
    let private isSpaceReplacement (spaceGlyph: uint32 voption) (codepoint: uint32) : bool =
        match spaceGlyph with
        | ValueNone -> false
        | ValueSome _ -> Unicode.isDefaultIgnorable codepoint

    /// <summary>Maps a code point to its glyph, a miss becoming glyph 0.</summary>
    /// <param name="font">The font.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>The glyph.</returns>
    let private nominalOrZero (font: Font) (codepoint: uint32) : uint32 =
        match Font.nominalGlyph font codepoint with
        | ValueSome glyph -> glyph
        | ValueNone -> NotFoundGlyph

    /// <summary>The final glyph of one code point: the space glyph for a default ignorable when one exists, else the nominal glyph.</summary>
    /// <param name="font">The font.</param>
    /// <param name="spaceGlyph">The space glyph, when the font maps one.</param>
    /// <param name="codepoint">The code point.</param>
    /// <returns>The glyph.</returns>
    let private finalGlyph (font: Font) (spaceGlyph: uint32 voption) (codepoint: uint32) : uint32 =
        match spaceGlyph with
        | ValueSome space ->
            match Unicode.isDefaultIgnorable codepoint with
            | true -> space
            | false -> nominalOrZero font codepoint
        | ValueNone -> nominalOrZero font codepoint

    /// <summary>The position of one horizontal fallback glyph.</summary>
    /// <param name="font">The font.</param>
    /// <param name="spaceGlyph">The space glyph, when the font maps one.</param>
    /// <param name="original">The original code point.</param>
    /// <param name="glyph">The final glyph.</param>
    /// <returns>The position.</returns>
    let private horizontalPosition (font: Font) (spaceGlyph: uint32 voption) (original: uint32) (glyph: uint32) : GlyphPosition =
        match isSpaceReplacement spaceGlyph original with
        | true -> zeroPosition
        | false -> { XAdvance = Font.hAdvance font glyph; YAdvance = 0; XOffset = 0; YOffset = 0; Var = 0u }

    /// <summary>The position of one vertical fallback glyph.</summary>
    /// <param name="font">The font.</param>
    /// <param name="spaceGlyph">The space glyph, when the font maps one.</param>
    /// <param name="original">The original code point.</param>
    /// <param name="glyph">The final glyph.</param>
    /// <param name="origin">The glyph's vertical origin.</param>
    /// <returns>The position.</returns>
    let private verticalPosition (font: Font) (spaceGlyph: uint32 voption) (original: uint32) (glyph: uint32) (origin: struct (int * int)) : GlyphPosition =
        match isSpaceReplacement spaceGlyph original with
        | true -> zeroPosition
        | false ->
            let struct (originX, originY) = origin
            { XAdvance = 0; YAdvance = Font.vAdvance font glyph; XOffset = -originX; YOffset = -originY; Var = 0u }

    /// <summary>The infos of a horizontal fallback run.</summary>
    /// <param name="font">The font.</param>
    /// <param name="spaceGlyph">The space glyph, when the font maps one.</param>
    /// <param name="run">The run.</param>
    /// <param name="infos">The replaced glyphs.</param>
    /// <returns>The run with positions.</returns>
    let private horizontalRun (font: Font) (spaceGlyph: uint32 voption) (run: GlyphRun) (infos: ImmutableArray<GlyphInfo>) : GlyphRun =
        let positions =
            Sealed.mapi (fun index _ -> horizontalPosition font spaceGlyph run.Infos[index].Codepoint infos[index].Codepoint) (run.Infos.AsSpan())
        { run with Infos = infos; Positions = positions; HavePositions = true }

    /// <summary>The infos of a vertical fallback run.</summary>
    /// <param name="font">The font.</param>
    /// <param name="spaceGlyph">The space glyph, when the font maps one.</param>
    /// <param name="run">The run.</param>
    /// <param name="infos">The replaced glyphs.</param>
    /// <returns>The run with positions, or a not-yet-ported vertical-origin source.</returns>
    let private verticalRun (font: Font) (spaceGlyph: uint32 voption) (run: GlyphRun) (infos: ImmutableArray<GlyphInfo>) : Result<GlyphRun, ShapeError> =
        let glyphs = Sealed.map (fun (info: GlyphInfo) -> info.Codepoint) (infos.AsSpan())
        match Origins.vOrigins font (glyphs.AsSpan()) with
        | Error error -> Error error
        | Ok origins ->
            let positions =
                Sealed.mapi (fun index _ -> verticalPosition font spaceGlyph run.Infos[index].Codepoint infos[index].Codepoint origins[index]) (run.Infos.AsSpan())
            Ok { run with Infos = infos; Positions = positions; HavePositions = true }

    /// <summary>Reverses a backward run, <c>hb_buffer_reverse</c>.</summary>
    /// <param name="run">The run.</param>
    /// <returns>The run, reversed when its direction is backward.</returns>
    let private finishRun (run: GlyphRun) : GlyphRun =
        match Direction.isBackward run.SegmentProperties.Direction with
        | true -> GlyphRun.reverse run
        | false -> run

    /// <summary>Shapes a run with the fallback shaper, <c>_hb_fallback_shape</c>.</summary>
    /// <param name="font">The font.</param>
    /// <param name="run">The run.</param>
    /// <returns>The shaped run, or a not-yet-ported vertical-origin source.</returns>
    let shape (font: Font) (run: GlyphRun) : Result<GlyphRun, ShapeError> =
        match run.Infos.Length with
        | 0 -> Ok run
        | _ ->
            let spaceGlyph = Font.nominalGlyph font Space
            let horizontal = Direction.isHorizontal run.SegmentProperties.Direction
            let infos =
                Sealed.mapi (fun _ (info: GlyphInfo) ->
                    { info with
                        Codepoint = finalGlyph font spaceGlyph info.Codepoint
                        Mask = info.Mask &&& ~~~(uint32 GlyphFlags.Defined) }) (run.Infos.AsSpan())
            let positioned =
                match horizontal with
                | true -> Ok (horizontalRun font spaceGlyph run infos)
                | false -> verticalRun font spaceGlyph run infos
            match positioned with
            | Error error -> Error error
            | Ok result -> Ok (finishRun result)

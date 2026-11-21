namespace Farfbuzz

open System

/// <summary>An approximate-membership filter over glyph ids, <c>hb_set_digest_t</c>.</summary>
[<Struct; NoComparison>]
type internal Digest =
    { M0: uint64  // shift 4
      M1: uint64  // shift 0
      M2: uint64 }  // shift 6

/// <summary>The three-mask glyph digest of <c>src/hb-set-digest.hh</c>.</summary>
[<RequireQualifiedAccess>]
module internal Digest =
    /// <summary>The first mask's shift, <c>hb_set_digest_shifts[0]</c>.</summary>
    [<Literal>]
    let private Shift0 = 4

    /// <summary>The second mask's shift, <c>hb_set_digest_shifts[1]</c>.</summary>
    [<Literal>]
    let private Shift1 = 0

    /// <summary>The third mask's shift, <c>hb_set_digest_shifts[2]</c>.</summary>
    [<Literal>]
    let private Shift2 = 6

    /// <summary>The number of bits one mask holds minus one, <c>mb1</c>.</summary>
    [<Literal>]
    let private MaskBits = 63u

    /// <summary>A mask with every bit set, <c>all</c>.</summary>
    let private maskAll = UInt64.MaxValue

    /// <summary>The empty digest, in which no glyph may be present.</summary>
    let empty = { M0 = 0UL; M1 = 0UL; M2 = 0UL }

    /// <summary>The full digest, in which every glyph may be present.</summary>
    let full = { M0 = UInt64.MaxValue; M1 = UInt64.MaxValue; M2 = UInt64.MaxValue }

    /// <summary>Sets the bit one glyph contributes to one mask.</summary>
    /// <param name="shift">The mask's shift.</param>
    /// <param name="glyph">The glyph.</param>
    /// <param name="mask">The mask.</param>
    /// <returns>The mask with the glyph's bit set.</returns>
    let private addToMask (shift) (glyph: uint32) (mask: uint64) =
        mask ||| (1UL <<< int ((glyph >>> shift) &&& MaskBits))

    /// <summary>Adds one glyph, <c>hb_set_digest_t::add</c>.</summary>
    /// <param name="glyph">The glyph.</param>
    /// <param name="digest">The digest.</param>
    /// <returns>The digest with the glyph added.</returns>
    let add (glyph: uint32) (digest) =
        { M0 = addToMask Shift0 glyph digest.M0
          M1 = addToMask Shift1 glyph digest.M1
          M2 = addToMask Shift2 glyph digest.M2 }

    /// <summary>Adds a closed range to one mask, <c>hb_set_digest_t::add_range</c>.</summary>
    /// <param name="shift">The mask's shift.</param>
    /// <param name="a">The first glyph.</param>
    /// <param name="b">The last glyph, inclusive.</param>
    /// <param name="mask">The mask.</param>
    /// <returns>The mask with the range added.</returns>
    let private addRangeToMask (shift) (a: uint32) (b: uint32) (mask: uint64) =
        match (b >>> shift) - (a >>> shift) >= MaskBits with
        | true -> maskAll
        | false ->
            let ma = 1UL <<< int ((a >>> shift) &&& MaskBits)
            let mb = 1UL <<< int ((b >>> shift) &&& MaskBits)
            mask ||| (mb + (mb - ma) - (if mb < ma then 1UL else 0UL))

    /// <summary>Adds a closed range, <c>hb_set_digest_t::add_range</c>.</summary>
    /// <param name="a">The first glyph.</param>
    /// <param name="b">The last glyph, inclusive.</param>
    /// <param name="digest">The digest.</param>
    /// <returns>The digest with the range added.</returns>
    let addRange (a: uint32) (b: uint32) (digest) =
        { M0 = addRangeToMask Shift0 a b digest.M0
          M1 = addRangeToMask Shift1 a b digest.M1
          M2 = addRangeToMask Shift2 a b digest.M2 }

    /// <summary>Ors two digests together, <c>hb_set_digest_t::union_</c>.</summary>
    /// <param name="a">The first digest.</param>
    /// <param name="b">The second digest.</param>
    /// <returns>The union.</returns>
    let union (a) (b) =
        { M0 = a.M0 ||| b.M0
          M1 = a.M1 ||| b.M1
          M2 = a.M2 ||| b.M2 }

    /// <summary>Whether a glyph may be present, <c>hb_set_digest_t::may_have</c>.</summary>
    /// <param name="glyph">The glyph.</param>
    /// <param name="digest">The digest.</param>
    /// <returns>False only when the glyph is certainly absent.</returns>
    let inline mayHave (glyph: uint32) (digest) =
        digest.M0 &&& (1UL <<< int ((glyph >>> Shift0) &&& MaskBits)) <> 0UL
        && digest.M1 &&& (1UL <<< int ((glyph >>> Shift1) &&& MaskBits)) <> 0UL
        && digest.M2 &&& (1UL <<< int ((glyph >>> Shift2) &&& MaskBits)) <> 0UL

    /// <summary>Whether a glyph may be present, <see cref="mayHave"/> as a call, for a caller in another assembly: an inline function that reads the
    /// private constants of its module cannot be inlined there.</summary>
    /// <param name="glyph">The glyph.</param>
    /// <param name="digest">The digest.</param>
    /// <returns>False only when the glyph is certainly absent.</returns>
    let mayHaveCall (glyph: uint32) (digest: Digest) =
        mayHave glyph digest

    /// <summary>Whether two digests share a possible glyph, <c>hb_set_digest_t::may_intersect</c>.</summary>
    /// <param name="a">The first digest.</param>
    /// <param name="b">The second digest.</param>
    /// <returns>False only when the digests certainly have no glyph in common.</returns>
    let inline mayIntersect (a) (b) =
        a.M0 &&& b.M0 <> 0UL
        && a.M1 &&& b.M1 <> 0UL
        && a.M2 &&& b.M2 <> 0UL

    /// <summary>Whether two digests share a possible glyph as a call, for an assembly that sees this one through <c>InternalsVisibleTo</c>: F# does not inline <see cref="mayIntersect"/> there.</summary>
    /// <param name="a">The first digest.</param>
    /// <param name="b">The second digest.</param>
    /// <returns>False only when the digests certainly have no glyph in common.</returns>
    let mayIntersectCall (a: Digest) (b: Digest) =
        mayIntersect a b


    /// <summary>Folds a buffer's glyph ids into a digest, <c>collect_codepoints</c>.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="index">The next index.</param>
    /// <param name="digest">The digest so far.</param>
    /// <returns>The digest of every glyph id.</returns>
    [<TailCall>]
    let rec private foldGlyphs (infos: ReadOnlySpan<GlyphInfo>) (index) (digest) =
        match index < infos.Length with
        | false -> digest
        | true -> foldGlyphs infos (index + 1) (add infos[index].Codepoint digest)

    /// <summary>Builds a digest from a buffer's glyph ids, <c>update_digest</c>.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <returns>The digest.</returns>
    let ofSpan (infos: ReadOnlySpan<GlyphInfo>) =
        foldGlyphs infos 0 empty

/// <summary>The digest active pattern, shared with the shaping pipeline.</summary>
[<AutoOpen>]
module internal DigestPatterns =
    /// <summary>Whether a glyph may be present in a digest, as a zero-allocation partial pattern.</summary>
    /// <param name="digest">The digest.</param>
    /// <param name="glyph">The glyph.</param>
    /// <returns>A unit when the glyph may be present.</returns>
    [<return: Struct>]
    let (|MayHave|_|) (digest: Digest) (glyph: uint32) =
        match Digest.mayHave glyph digest with
        | true -> ValueSome ()
        | false -> ValueNone

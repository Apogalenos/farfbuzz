namespace Farfbuzz

open System
open System.Text

/// <summary>Unit of measure marking a 32-bit OpenType tag.</summary>
[<Measure>]
type tag

/// <summary>An OpenType tag, <c>hb_tag_t</c>: four bytes packed into 32 bits, the first most significant.</summary>
type Tag = uint32<tag>

/// <summary>Well-known tags, as literals so that they can be matched directly.</summary>
[<RequireQualifiedAccess>]
module Tags =
    /// <summary>The empty tag, <c>HB_TAG_NONE</c>.</summary>
    [<Literal>]
    let Empty = 0u<tag>

    /// <summary>The largest tag value, <c>HB_TAG_MAX</c>.</summary>
    [<Literal>]
    let Max = 0xFFFFFFFFu<tag>

    /// <summary>The OpenType/CFF container signature.</summary>
    [<Literal>]
    let OpenType = 0x4F54544Fu<tag> // 'OTTO'

    /// <summary>The TrueType container signature.</summary>
    [<Literal>]
    let TrueType = 0x00010000u<tag>

    /// <summary>The obsolete Apple TrueType signature.</summary>
    [<Literal>]
    let True = 0x74727565u<tag> // 'true'

    /// <summary>The obsolete Apple Type 1 signature.</summary>
    [<Literal>]
    let Typ1 = 0x74797031u<tag> // 'typ1'

    /// <summary>The TrueType Collection signature.</summary>
    [<Literal>]
    let Collection = 0x74746366u<tag> // 'ttcf'

    /// <summary>The Mac resource fork signature.</summary>
    [<Literal>]
    let ResourceFork = 0x00000100u<tag>

    /// <summary>The Mac resource type carrying an sfnt.</summary>
    [<Literal>]
    let Sfnt = 0x73666E74u<tag> // 'sfnt'

    /// <summary>The <c>head</c> table.</summary>
    [<Literal>]
    let Head = 0x68656164u<tag> // 'head'

    /// <summary>The <c>maxp</c> table.</summary>
    [<Literal>]
    let Maxp = 0x6D617870u<tag> // 'maxp'

    /// <summary>The 24-bit <c>MAXP</c> table.</summary>
    [<Literal>]
    let UpperMaxp = 0x4D415850u<tag> // 'MAXP'

    /// <summary>The <c>hhea</c> table.</summary>
    [<Literal>]
    let Hhea = 0x68686561u<tag> // 'hhea'

    /// <summary>The 32-bit <c>HHEA</c> table.</summary>
    [<Literal>]
    let UpperHhea = 0x48484541u<tag> // 'HHEA'

    /// <summary>The <c>vhea</c> table.</summary>
    [<Literal>]
    let Vhea = 0x76686561u<tag> // 'vhea'

    /// <summary>The 32-bit <c>VHEA</c> table.</summary>
    [<Literal>]
    let UpperVhea = 0x56484541u<tag> // 'VHEA'

    /// <summary>The <c>hmtx</c> table.</summary>
    [<Literal>]
    let Hmtx = 0x686D7478u<tag> // 'hmtx'

    /// <summary>The 32-bit <c>HMTX</c> table.</summary>
    [<Literal>]
    let UpperHmtx = 0x484D5458u<tag> // 'HMTX'

    /// <summary>The <c>vmtx</c> table.</summary>
    [<Literal>]
    let Vmtx = 0x766D7478u<tag> // 'vmtx'

    /// <summary>The 32-bit <c>VMTX</c> table.</summary>
    [<Literal>]
    let UpperVmtx = 0x564D5458u<tag> // 'VMTX'

    /// <summary>The <c>OS/2</c> table.</summary>
    [<Literal>]
    let Os2 = 0x4F532F32u<tag> // 'OS/2'

    /// <summary>The <c>cmap</c> table.</summary>
    [<Literal>]
    let Cmap = 0x636D6170u<tag> // 'cmap'

    /// <summary>The <c>post</c> table.</summary>
    [<Literal>]
    let Post = 0x706F7374u<tag> // 'post'

    /// <summary>The CFF1 table.</summary>
    [<Literal>]
    let Cff = 0x43464620u<tag> // 'CFF '

    /// <summary>The CFF2 table.</summary>
    [<Literal>]
    let Cff2 = 0x43464632u<tag> // 'CFF2'

    /// <summary>The glyf table.</summary>
    [<Literal>]
    let Glyf = 0x676C7966u<tag> // 'glyf'

    /// <summary>The loca table.</summary>
    [<Literal>]
    let Loca = 0x6C6F6361u<tag> // 'loca'

    /// <summary>The VORG table.</summary>
    [<Literal>]
    let Vorg = 0x564F5247u<tag> // 'VORG'

    /// <summary>The sbix table.</summary>
    [<Literal>]
    let Sbix = 0x73626978u<tag> // 'sbix'

    /// <summary>The CBDT table.</summary>
    [<Literal>]
    let Cbdt = 0x43424454u<tag> // 'CBDT'

    /// <summary>The CBLC table.</summary>
    [<Literal>]
    let Cblc = 0x43424C43u<tag> // 'CBLC'

    /// <summary>The COLR table.</summary>
    [<Literal>]
    let Colr = 0x434F4C52u<tag> // 'COLR'

    /// <summary>The VARC table.</summary>
    [<Literal>]
    let Varc = 0x56415243u<tag> // 'VARC'

    /// <summary>The <c>fvar</c> table.</summary>
    [<Literal>]
    let Fvar = 0x66766172u<tag> // 'fvar'

    /// <summary>The <c>avar</c> table.</summary>
    [<Literal>]
    let Avar = 0x61766172u<tag> // 'avar'

    /// <summary>The <c>GDEF</c> table.</summary>
    [<Literal>]
    let Gdef = 0x47444546u<tag> // 'GDEF'

    /// <summary>The <c>GSUB</c> table.</summary>
    [<Literal>]
    let Gsub = 0x47535542u<tag> // 'GSUB'

    /// <summary>The <c>GPOS</c> table.</summary>
    [<Literal>]
    let Gpos = 0x47504F53u<tag> // 'GPOS'

    /// <summary>The <c>HVAR</c> table.</summary>
    [<Literal>]
    let Hvar = 0x48564152u<tag> // 'HVAR'

    /// <summary>The <c>VVAR</c> table.</summary>
    [<Literal>]
    let Vvar = 0x56564152u<tag> // 'VVAR'

    /// <summary>The <c>MVAR</c> table.</summary>
    [<Literal>]
    let Mvar = 0x4D564152u<tag> // 'MVAR'

    /// <summary>The <c>gvar</c> table.</summary>
    [<Literal>]
    let Gvar = 0x67766172u<tag> // 'gvar'

    /// <summary>The 32-bit <c>GVAR</c> table.</summary>
    [<Literal>]
    let UpperGvar = 0x47564152u<tag> // 'GVAR'

    /// <summary>The default OpenType script tag, <c>HB_OT_TAG_DEFAULT_SCRIPT</c>.</summary>
    [<Literal>]
    let Dflt = 0x44464C54u<tag> // 'DFLT'

    /// <summary>The pre-specification default script and language tag, <c>HB_OT_TAG_DEFAULT_LANGUAGE</c>.</summary>
    [<Literal>]
    let dflt = 0x64666C74u<tag> // 'dflt'

    /// <summary>The Latin script tag.</summary>
    [<Literal>]
    let Latn = 0x6C61746Eu<tag> // 'latn'

    /// <summary>The pre-specification Myanmar script tag.</summary>
    [<Literal>]
    let Mymr = 0x6D796D72u<tag> // 'mymr'

    /// <summary>The <c>frac</c> feature.</summary>
    [<Literal>]
    let Frac = 0x66726163u<tag> // 'frac'

    /// <summary>The <c>numr</c> feature.</summary>
    [<Literal>]
    let Numr = 0x6E756D72u<tag> // 'numr'

    /// <summary>The <c>dnom</c> feature.</summary>
    [<Literal>]
    let Dnom = 0x646E6F6Du<tag> // 'dnom'

    /// <summary>The <c>rtlm</c> feature.</summary>
    [<Literal>]
    let Rtlm = 0x72746C6Du<tag> // 'rtlm'

    /// <summary>The <c>vert</c> feature.</summary>
    [<Literal>]
    let Vert = 0x76657274u<tag> // 'vert'

    /// <summary>The horizontal <c>kern</c> feature.</summary>
    [<Literal>]
    let Kern = 0x6B65726Eu<tag> // 'kern'

    /// <summary>The vertical <c>vkrn</c> feature.</summary>
    [<Literal>]
    let Vkrn = 0x766B726Eu<tag> // 'vkrn'

    /// <summary>The <c>mark</c> feature.</summary>
    [<Literal>]
    let Mark = 0x6D61726Bu<tag> // 'mark'

    /// <summary>The <c>rvrn</c> feature.</summary>
    [<Literal>]
    let Rvrn = 0x7276726Eu<tag> // 'rvrn'

    /// <summary>The <c>ltra</c> feature.</summary>
    [<Literal>]
    let Ltra = 0x6C747261u<tag> // 'ltra'

    /// <summary>The <c>ltrm</c> feature.</summary>
    [<Literal>]
    let Ltrm = 0x6C74726Du<tag> // 'ltrm'

    /// <summary>The <c>rtla</c> feature.</summary>
    [<Literal>]
    let Rtla = 0x72746C61u<tag> // 'rtla'

    /// <summary>The <c>rand</c> feature.</summary>
    [<Literal>]
    let Rand = 0x72616E64u<tag> // 'rand'

    /// <summary>The <c>Harf</c> required feature.</summary>
    [<Literal>]
    let Harf = 0x48617266u<tag> // 'Harf'

    /// <summary>The <c>HARF</c> discretionary feature.</summary>
    [<Literal>]
    let HarfUpper = 0x48415246u<tag> // 'HARF'

    /// <summary>The <c>Buzz</c> required feature.</summary>
    [<Literal>]
    let Buzz = 0x42757A7Au<tag> // 'Buzz'

    /// <summary>The <c>BUZZ</c> discretionary feature.</summary>
    [<Literal>]
    let BuzzUpper = 0x42555A5Au<tag> // 'BUZZ'

    /// <summary>The <c>abvm</c> feature.</summary>
    [<Literal>]
    let Abvm = 0x6162766Du<tag> // 'abvm'

    /// <summary>The <c>blwm</c> feature.</summary>
    [<Literal>]
    let Blwm = 0x626C776Du<tag> // 'blwm'

    /// <summary>The <c>ccmp</c> feature.</summary>
    [<Literal>]
    let Ccmp = 0x63636D70u<tag> // 'ccmp'

    /// <summary>The <c>locl</c> feature.</summary>
    [<Literal>]
    let Locl = 0x6C6F636Cu<tag> // 'locl'

    /// <summary>The <c>mkmk</c> feature.</summary>
    [<Literal>]
    let Mkmk = 0x6D6B6D6Bu<tag> // 'mkmk'

    /// <summary>The <c>rlig</c> feature.</summary>
    [<Literal>]
    let Rlig = 0x726C6967u<tag> // 'rlig'

    /// <summary>The <c>calt</c> feature.</summary>
    [<Literal>]
    let Calt = 0x63616C74u<tag> // 'calt'

    /// <summary>The <c>clig</c> feature.</summary>
    [<Literal>]
    let Clig = 0x636C6967u<tag> // 'clig'

    /// <summary>The <c>curs</c> feature.</summary>
    [<Literal>]
    let Curs = 0x63757273u<tag> // 'curs'

    /// <summary>The <c>dist</c> feature.</summary>
    [<Literal>]
    let Dist = 0x64697374u<tag> // 'dist'

    /// <summary>The <c>liga</c> feature.</summary>
    [<Literal>]
    let Liga = 0x6C696761u<tag> // 'liga'

    /// <summary>The <c>rclt</c> feature.</summary>
    [<Literal>]
    let Rclt = 0x72636C74u<tag> // 'rclt'

    /// <summary>The four-space tag used for a required feature's lookup, <c>HB_TAG(' ',' ',' ',' ')</c>.</summary>
    [<Literal>]
    let Space = 0x20202020u<tag> // '    '

    /// <summary>The <c>aalt</c> feature.</summary>
    [<Literal>]
    let Aalt = 0x61616C74u<tag> // 'aalt'

    /// <summary>The <c>feat</c> table.</summary>
    [<Literal>]
    let Feat = 0x66656174u<tag> // 'feat'

    /// <summary>The <c>ltag</c> table.</summary>
    [<Literal>]
    let Ltag = 0x6C746167u<tag> // 'ltag'

    /// <summary>The <c>morx</c> table.</summary>
    [<Literal>]
    let Morx = 0x6D6F7278u<tag> // 'morx'

    /// <summary>The <c>mort</c> table.</summary>
    [<Literal>]
    let Mort = 0x6D6F7274u<tag> // 'mort'

    /// <summary>The <c>trak</c> table.</summary>
    [<Literal>]
    let Trak = 0x7472616Bu<tag> // 'trak'

    /// <summary>The <c>STAT</c> table.</summary>
    [<Literal>]
    let Stat = 0x53544154u<tag> // 'STAT'

    /// <summary>The <c>kerx</c> table.</summary>
    [<Literal>]
    let Kerx = 0x6B657278u<tag> // 'kerx'

    /// <summary>The <c>ankr</c> table.</summary>
    [<Literal>]
    let Ankr = 0x616E6B72u<tag> // 'ankr'

/// <summary>Conversions between OpenType tags and their four-character spellings.</summary>
[<RequireQualifiedAccess>]
module Tag =
    /// <summary>The byte a short tag is padded with.</summary>
    [<Literal>]
    let private Space = 0x20uy

    /// <summary>The number of bytes in a tag.</summary>
    [<Literal>]
    let Width = 4

    /// <summary>Packs four bytes into a tag, the first byte most significant, as <c>HB_TAG</c> does.</summary>
    /// <param name="a">The first character.</param>
    /// <param name="b">The second character.</param>
    /// <param name="c">The third character.</param>
    /// <param name="d">The fourth character.</param>
    /// <returns>The packed tag.</returns>
    let inline ofBytes (a: byte) (b: byte) (c: byte) (d: byte) : Tag =
        LanguagePrimitives.UInt32WithMeasure ((uint32 a <<< 24) ||| (uint32 b <<< 16) ||| (uint32 c <<< 8) ||| uint32 d)

    /// <summary>Packs four characters into a tag, each taken as one byte, as <c>HB_TAG</c> does.</summary>
    /// <param name="a">The first character.</param>
    /// <param name="b">The second character.</param>
    /// <param name="c">The third character.</param>
    /// <param name="d">The fourth character.</param>
    /// <returns>The packed tag.</returns>
    let inline make (a: char) (b: char) (c: char) (d: char) : Tag =
        ofBytes (byte a) (byte b) (byte c) (byte d)

    /// <summary>Counts the bytes that take part in a tag: at most four, stopping at the first NUL.</summary>
    /// <param name="bytes">The candidate bytes.</param>
    /// <returns>The number of significant bytes.</returns>
    let private significantLength (bytes: ReadOnlySpan<byte>) =
        let window = bytes.Slice(0, min Width bytes.Length)
        match window.IndexOf 0uy with
        | -1 -> window.Length
        | firstNul -> firstNul

    /// <summary>Reads byte <paramref name="index"/> of a tag, padding with spaces past its end.</summary>
    /// <param name="bytes">The tag bytes.</param>
    /// <param name="length">The number of significant bytes.</param>
    /// <param name="index">The position to read.</param>
    /// <returns>The byte at the position, or a space.</returns>
    let private paddedAt (bytes: ReadOnlySpan<byte>) (length: int) (index: int) =
        if index < length then bytes[index] else Space

    /// <summary>
    /// Converts UTF-8 bytes into a tag exactly as <c>hb_tag_from_string</c>: shorter input is padded with
    /// spaces, longer input truncated to four bytes, and empty input gives the empty tag.
    /// </summary>
    /// <param name="bytes">The UTF-8 spelling.</param>
    /// <returns>The tag.</returns>
    let ofUtf8 (bytes: ReadOnlySpan<byte>) : Tag =
        match significantLength bytes with
        | 0 -> Tags.Empty
        | length ->
            ofBytes
                (paddedAt bytes length 0)
                (paddedAt bytes length 1)
                (paddedAt bytes length 2)
                (paddedAt bytes length 3)

    /// <summary>Converts a string into a tag through its UTF-8 encoding, as <c>hb_tag_from_string</c>.</summary>
    /// <param name="text">The spelling of the tag.</param>
    /// <returns>The tag.</returns>
    let ofString (text: string) : Tag =
        ofUtf8 (ReadOnlySpan(Encoding.UTF8.GetBytes text))

    /// <summary>Returns one byte of a tag.</summary>
    /// <param name="tag">The tag.</param>
    /// <param name="index">The byte position, 0 being the most significant.</param>
    /// <returns>The byte.</returns>
    let inline byteAt (tag: Tag) (index: int) =
        byte (uint32 tag >>> (8 * (Width - 1 - index)))

    /// <summary>Spells a tag as four characters, each byte taken as a Latin-1 character, as <c>hb_tag_to_string</c>.</summary>
    /// <param name="tag">The tag.</param>
    /// <returns>The four-character spelling.</returns>
    let toString (tag: Tag) : string =
        String([| char (byteAt tag 0); char (byteAt tag 1); char (byteAt tag 2); char (byteAt tag 3) |])

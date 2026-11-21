namespace Farfbuzz

open System
open System.Collections.Immutable

/// <summary>OpenType script and language tags at run time, <c>hb-ot-tag.cc</c>.</summary>
[<RequireQualifiedAccess>]
module internal OtTag =
    /// <summary>The default script tag, <c>HB_OT_TAG_DEFAULT_SCRIPT</c>.</summary>
    [<Literal>]
    let private DefaultScript = 0x44464C54u<tag>

    /// <summary>The default language tag, <c>HB_OT_TAG_DEFAULT_LANGUAGE</c>.</summary>
    [<Literal>]
    let private DefaultLanguage = 0x64666C74u<tag>

    /// <summary>The math script tag, <c>HB_OT_TAG_MATH_SCRIPT</c>.</summary>
    [<Literal>]
    let private MathScript = 0x6D617468u<tag>

    /// <summary>The <c>kana</c> tag shared by Hiragana, Katakana and their collective Hrkt script.</summary>
    [<Literal>]
    let private Kana = 0x6B616E61u<tag>

    /// <summary>The collective Hrkt script value, <c>HB_TAG('H','r','k','t')</c>.</summary>
    [<Literal>]
    let private HrktScript = 0x48726B74u<script>

    /// <summary>The <c>lao </c> script tag with its trailing space.</summary>
    [<Literal>]
    let private LaoTag = 0x6C616F20u<tag>

    /// <summary>The <c>yi  </c> script tag with its trailing spaces.</summary>
    [<Literal>]
    let private YiTag = 0x79692020u<tag>

    /// <summary>The <c>nko </c> script tag with its trailing space.</summary>
    [<Literal>]
    let private NkoTag = 0x6E6B6F20u<tag>

    /// <summary>The <c>vai </c> script tag with its trailing space.</summary>
    [<Literal>]
    let private VaiTag = 0x76616920u<tag>

    /// <summary>The <c>bng2</c> script tag.</summary>
    [<Literal>]
    let private Bng2 = 0x626E6732u<tag>

    /// <summary>The <c>dev2</c> script tag.</summary>
    [<Literal>]
    let private Dev2 = 0x64657632u<tag>

    /// <summary>The <c>gjr2</c> script tag.</summary>
    [<Literal>]
    let private Gjr2 = 0x676A7232u<tag>

    /// <summary>The <c>gur2</c> script tag.</summary>
    [<Literal>]
    let private Gur2 = 0x67757232u<tag>

    /// <summary>The <c>knd2</c> script tag.</summary>
    [<Literal>]
    let private Knd2 = 0x6B6E6432u<tag>

    /// <summary>The <c>mlm2</c> script tag.</summary>
    [<Literal>]
    let private Mlm2 = 0x6D6C6D32u<tag>

    /// <summary>The <c>ory2</c> script tag.</summary>
    [<Literal>]
    let private Ory2 = 0x6F727932u<tag>

    /// <summary>The <c>tml2</c> script tag.</summary>
    [<Literal>]
    let private Tml2 = 0x746D6C32u<tag>

    /// <summary>The <c>tel2</c> script tag.</summary>
    [<Literal>]
    let private Tel2 = 0x74656C32u<tag>

    /// <summary>The <c>mym2</c> script tag, the one new tag without a <c>3</c> variant.</summary>
    [<Literal>]
    let private Mym2 = 0x6D796D32u<tag>

    /// <summary>The bit that lowercases the first letter of an old script tag.</summary>
    [<Literal>]
    let private LowerFirstLetter = 0x20000000u

    /// <summary>The mask that clears the first-letter lowercase bit.</summary>
    [<Literal>]
    let private UpperFirstLetterMask = 0xDFFFFFFFu

    /// <summary>The mask that uppercases the first three letters of an ISO 639-3 tag.</summary>
    [<Literal>]
    let private UpperIso6393Mask = 0xDFDFDFFFu

    /// <summary>The mask that uppercases all four letters of a tag.</summary>
    [<Literal>]
    let private UpperAllMask = 0xDFDFDFDFu

    /// <summary>The xor that flips the case of all four letters, <c>~HB_OT_TAG_DEFAULT_SCRIPT</c>.</summary>
    [<Literal>]
    let private FlipCaseXor = 0x20202020u

    /// <summary>The mask that maps a <c>2</c> or <c>3</c> suffix to <c>2</c>.</summary>
    [<Literal>]
    let private Tag2Mask = 0xFFFFFF32u

    /// <summary>The <c>3</c> suffix byte, <c>'3'</c>.</summary>
    [<Literal>]
    let private Char3 = 0x33u

    /// <summary>The digit <c>2</c>, <c>'2'</c>.</summary>
    [<Literal>]
    let private Digit2 = 0x32uy

    /// <summary>The digit <c>3</c>, <c>'3'</c>.</summary>
    [<Literal>]
    let private Digit3 = 0x33uy

    /// <summary>The mask of the third tag byte, <c>0x0000FF00</c>.</summary>
    [<Literal>]
    let private ThirdByteMask = 0x0000FF00u

    /// <summary>The space value of the third tag byte.</summary>
    [<Literal>]
    let private ThirdByteSpace = 0x00002000u

    /// <summary>The mask of the fourth tag byte, <c>0x000000FF</c>.</summary>
    [<Literal>]
    let private FourthByteMask = 0x000000FFu

    /// <summary>The space value of the fourth tag byte.</summary>
    [<Literal>]
    let private FourthByteSpace = 0x00000020u

    /// <summary>The ASCII space character.</summary>
    [<Literal>]
    let private Space = 0x20uy

    /// <summary>Whether a character is an ASCII letter, <c>ISALPHA</c>.</summary>
    /// <param name="c">The character.</param>
    /// <returns>True for a letter.</returns>
    let private isAlpha (c: char) =
        match c with
        | c when c >= 'a' && c <= 'z' -> true
        | c when c >= 'A' && c <= 'Z' -> true
        | _ -> false

    /// <summary>Whether a character is an ASCII letter or digit, <c>ISALNUM</c>.</summary>
    /// <param name="c">The character.</param>
    /// <returns>True for a letter or digit.</returns>
    let private isAlnum (c: char) =
        match c with
        | c when c >= 'a' && c <= 'z' -> true
        | c when c >= 'A' && c <= 'Z' -> true
        | c when c >= '0' && c <= '9' -> true
        | _ -> false

    /// <summary>Lowercases an ASCII character, <c>TOLOWER</c>.</summary>
    /// <param name="c">The character.</param>
    /// <returns>The lowercase character.</returns>
    let private toLower (c: char) =
        match c with
        | c when c >= 'A' && c <= 'Z' -> char (int c + 32)
        | _ -> c

    /// <summary>Uppercases an ASCII character, <c>TOUPPER</c>.</summary>
    /// <param name="c">The character.</param>
    /// <returns>The uppercase character.</returns>
    let private toUpper (c: char) =
        match c with
        | c when c >= 'a' && c <= 'z' -> char (int c - 32)
        | _ -> c

    /// <summary>The value of an ASCII hex digit, or -1, <c>FROMHEX</c>.</summary>
    /// <param name="c">The character.</param>
    /// <returns>The value 0..15, or -1.</returns>
    let private hexValue (c: char) =
        match c with
        | c when c >= '0' && c <= '9' -> int c - int '0'
        | c when c >= 'a' && c <= 'f' -> int c - int 'a' + 10
        | c when c >= 'A' && c <= 'F' -> int c - int 'A' + 10
        | _ -> -1

    /// <summary>Reads two hex digits as one byte.</summary>
    /// <param name="text">The string.</param>
    /// <param name="index">The first digit.</param>
    /// <returns>The byte.</returns>
    let private hexByte (text: string) (index: int) =
        byte ((hexValue text[index] <<< 4) ||| hexValue text[index + 1])

    /// <summary>The distance from a position to the limit, huge when the limit is null or before the position.</summary>
    /// <param name="limit">The limit index, or none for the C null pointer.</param>
    /// <param name="position">The position.</param>
    /// <returns>The unsigned distance.</returns>
    let private distanceToLimit (limit: int voption) (position: int) =
        match limit with
        | ValueNone -> Int32.MaxValue
        | ValueSome i ->
            match i >= position with
            | true -> i - position
            | false -> Int32.MaxValue

    /// <summary>Whether an index is at or past the limit, mirroring a C pointer comparison with null.</summary>
    /// <param name="limit">The limit index, or none for the C null pointer.</param>
    /// <param name="index">The index.</param>
    /// <returns>True when the index is at or past the limit.</returns>
    let private atOrPastLimit (limit: int voption) (index: int) =
        match limit with
        | ValueNone -> true
        | ValueSome i -> index >= i

    /// <summary>Returns the limit, substituting a fallback when it is null.</summary>
    /// <param name="limit">The limit index, or none.</param>
    /// <param name="fallback">The fallback.</param>
    /// <returns>The limit.</returns>
    let private firstLimit (limit: int voption) (fallback: int) =
        match limit with
        | ValueSome _ -> limit
        | ValueNone -> ValueSome fallback

    /// <summary>Whether the byte after an index is an ASCII letter or digit.</summary>
    /// <param name="text">The string.</param>
    /// <param name="index">The index.</param>
    /// <returns>True when the following byte is alphanumeric.</returns>
    let private followedByAlnum (text: string) (index: int) =
        index < text.Length && isAlnum text[index]

    /// <summary>The up to three candidate script tags of a script.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private ScriptCandidates =
        { Count: int; Tag0: Tag; Tag1: Tag; Tag2: Tag }

    /// <summary>The new-style script tag of a script, <c>hb_ot_new_tag_from_script</c>.</summary>
    /// <param name="script">The script.</param>
    /// <returns>The tag, or the default script tag.</returns>
    let private newTagFromScript (script: Script) =
        match script with
        | Scripts.Bengali -> Bng2
        | Scripts.Devanagari -> Dev2
        | Scripts.Gujarati -> Gjr2
        | Scripts.Gurmukhi -> Gur2
        | Scripts.Kannada -> Knd2
        | Scripts.Malayalam -> Mlm2
        | Scripts.Oriya -> Ory2
        | Scripts.Tamil -> Tml2
        | Scripts.Telugu -> Tel2
        | Scripts.Myanmar -> Mym2
        | _ -> DefaultScript

    /// <summary>The script of a new-style tag, <c>hb_ot_new_tag_to_script</c>.</summary>
    /// <param name="tag">The tag.</param>
    /// <returns>The script, or <c>Scripts.Unknown</c>.</returns>
    let private newTagToScript (tag: Tag) =
        match tag with
        | Bng2 -> Scripts.Bengali
        | Dev2 -> Scripts.Devanagari
        | Gjr2 -> Scripts.Gujarati
        | Gur2 -> Scripts.Gurmukhi
        | Knd2 -> Scripts.Kannada
        | Mlm2 -> Scripts.Malayalam
        | Ory2 -> Scripts.Oriya
        | Tml2 -> Scripts.Tamil
        | Tel2 -> Scripts.Telugu
        | Mym2 -> Scripts.Myanmar
        | _ -> Scripts.Unknown

    /// <summary>The old-style script tag of a script, <c>hb_ot_old_tag_from_script</c>.</summary>
    /// <param name="script">The script.</param>
    /// <returns>The tag.</returns>
    let private oldTagFromScript (script: Script) =
        match script with
        | Scripts.Invalid -> DefaultScript
        | Scripts.Math -> MathScript
        | Scripts.Hiragana -> Kana
        | HrktScript -> Kana
        | Scripts.Lao -> LaoTag
        | Scripts.Yi -> YiTag
        | Scripts.Nko -> NkoTag
        | Scripts.Vai -> VaiTag
        | _ -> LanguagePrimitives.UInt32WithMeasure (uint32 script ||| LowerFirstLetter)

    /// <summary>The script of an old-style tag, <c>hb_ot_old_tag_to_script</c>.</summary>
    /// <param name="tag">The tag.</param>
    /// <returns>The script.</returns>
    let private oldTagToScript (tag: Tag) =
        match tag with
        | DefaultScript -> Scripts.Invalid
        | MathScript -> Scripts.Math
        | _ ->
            let value = uint32 tag
            let value =
                match (value &&& ThirdByteMask) = ThirdByteSpace with
                | true -> value ||| ((value >>> 8) &&& ThirdByteMask)
                | false -> value
            let value =
                match (value &&& FourthByteMask) = FourthByteSpace with
                | true -> value ||| ((value >>> 8) &&& FourthByteMask)
                | false -> value
            LanguagePrimitives.UInt32WithMeasure (value &&& UpperFirstLetterMask)

    /// <summary>The candidate script tags of a script, <c>hb_ot_all_tags_from_script</c>.</summary>
    /// <param name="script">The script.</param>
    /// <returns>The up to three tags in order.</returns>
    let private scriptCandidates (script: Script) : ScriptCandidates =
        let newTag = newTagFromScript script
        let oldTag = oldTagFromScript script
        let three = LanguagePrimitives.UInt32WithMeasure (uint32 newTag ||| Char3)
        match newTag = DefaultScript with
        | true ->
            match oldTag = DefaultScript with
            | true -> { Count = 0; Tag0 = Tags.Empty; Tag1 = Tags.Empty; Tag2 = Tags.Empty }
            | false -> { Count = 1; Tag0 = oldTag; Tag1 = Tags.Empty; Tag2 = Tags.Empty }
        | false ->
            match newTag = Mym2 with
            | true ->
                match oldTag = DefaultScript with
                | true -> { Count = 1; Tag0 = newTag; Tag1 = Tags.Empty; Tag2 = Tags.Empty }
                | false -> { Count = 2; Tag0 = newTag; Tag1 = oldTag; Tag2 = Tags.Empty }
            | false ->
                match oldTag = DefaultScript with
                | true -> { Count = 2; Tag0 = three; Tag1 = newTag; Tag2 = Tags.Empty }
                | false -> { Count = 3; Tag0 = three; Tag1 = newTag; Tag2 = oldTag }

    /// <summary>The script tags of a script, capped at a capacity.</summary>
    /// <param name="script">The script.</param>
    /// <param name="capacity">The maximum number of tags.</param>
    /// <returns>The tags.</returns>
    let private scriptTags (script: Script) (capacity: int) : ImmutableArray<Tag> =
        let candidates = scriptCandidates script
        let count = min capacity candidates.Count
        Sealed.init count (fun index ->
            match index with
            | 0 -> candidates.Tag0
            | 1 -> candidates.Tag1
            | _ -> candidates.Tag2)

    /// <summary>The stages of a language-tag lookup.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private LanguageStage =
        | Ambiguous
        | Languages2
        | Languages3
        | Languages3Multi

    /// <summary>Returns the first tag of a capped tag list.</summary>
    /// <param name="tag">The tag.</param>
    /// <returns>The one-tag array.</returns>
    let private emitOne (tag: Tag) =
        Sealed.init 1 (fun _ -> tag)

    /// <summary>Caps a tag list at a capacity.</summary>
    /// <param name="capacity">The maximum number of tags.</param>
    /// <param name="tags">The tags.</param>
    /// <returns>The capped tags.</returns>
    let private emitTags (capacity: int) (tags: ImmutableArray<Tag>) =
        let count = min capacity tags.Length
        Sealed.init count (fun index -> tags[index])

    /// <summary>Upper-cases the first three letters of a tag, <c>lang_tag &amp; ~0x20202000u</c>.</summary>
    /// <param name="tag">The tag.</param>
    /// <returns>The upper-cased tag.</returns>
    let private uppercaseIso6393 (tag: Tag) =
        LanguagePrimitives.UInt32WithMeasure (uint32 tag &&& UpperIso6393Mask)

    /// <summary>Finds a two-letter language tag in a sorted array, <c>hb_bsearch</c>.</summary>
    /// <param name="array">The language tags.</param>
    /// <param name="lo">The first index.</param>
    /// <param name="hi">The last index.</param>
    /// <param name="language">The language to find.</param>
    /// <returns>The matching index, or none.</returns>
    [<TailCall>]
    let rec private findLanguage (array: ImmutableArray<LangTag>) (lo: int) (hi: int) (language: Tag) : int voption =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = lo + (hi - lo) / 2
            let value = array[mid].Language
            match language < value, language > value with
            | true, _ -> findLanguage array lo (mid - 1) language
            | false, true -> findLanguage array (mid + 1) hi language
            | false, false -> ValueSome mid

    /// <summary>Finds a language in a sorted multi-value range array.</summary>
    /// <param name="array">The ranges.</param>
    /// <param name="lo">The first index.</param>
    /// <param name="hi">The last index.</param>
    /// <param name="language">The language to find.</param>
    /// <returns>The matching index, or none.</returns>
    [<TailCall>]
    let rec private findRange (array: ImmutableArray<LangTagRange>) (lo: int) (hi: int) (language: Tag) : int voption =
        match lo > hi with
        | true -> ValueNone
        | false ->
            let mid = lo + (hi - lo) / 2
            let value = array[mid].RangeLanguage
            match language < value, language > value with
            | true, _ -> findRange array lo (mid - 1) language
            | false, true -> findRange array (mid + 1) hi language
            | false, false -> ValueSome mid

    /// <summary>Whether a blocked language is present, <c>bfind_tag</c>.</summary>
    /// <param name="array">The sorted blocked tags.</param>
    /// <param name="lo">The first index.</param>
    /// <param name="hi">The end index, exclusive.</param>
    /// <param name="key">The key.</param>
    /// <returns>True when present.</returns>
    [<TailCall>]
    let rec private bfindTag (array: ImmutableArray<Tag>) (lo: int) (hi: int) (key: Tag) : bool =
        match lo >= hi with
        | true -> false
        | false ->
            let mid = lo + (hi - lo) / 2
            let value = array[mid]
            match key < value, key > value with
            | true, _ -> bfindTag array lo mid key
            | false, true -> bfindTag array (mid + 1) hi key
            | false, false -> true

    /// <summary>Backs up to the first entry sharing a language.</summary>
    /// <param name="array">The language tags.</param>
    /// <param name="index">A matching index.</param>
    /// <param name="language">The language.</param>
    /// <returns>The first index with that language.</returns>
    [<TailCall>]
    let rec private firstEqualLanguage (array: ImmutableArray<LangTag>) (index: int) (language: Tag) : int =
        match index = 0 || array[index - 1].Language <> language with
        | true -> index
        | false -> firstEqualLanguage array (index - 1) language

    /// <summary>Counts the consecutive two-letter tags sharing a language, bounded by the capacity.</summary>
    /// <param name="array">The language tags.</param>
    /// <param name="index">The first index.</param>
    /// <param name="language">The language.</param>
    /// <param name="capacity">The maximum count.</param>
    /// <param name="count">The count so far.</param>
    /// <returns>The count.</returns>
    [<TailCall>]
    let rec private countTwoLetter (array: ImmutableArray<LangTag>) (index: int) (language: Tag) (capacity: int) (count: int) : int =
        match count < capacity && index + count < array.Length && array[index + count].Language = language with
        | true -> countTwoLetter array index language capacity (count + 1)
        | false -> count

    /// <summary>The canonical language of a tag, read from its first letters.</summary>
    /// <param name="tag">The tag.</param>
    /// <param name="length">The number of significant letters.</param>
    /// <returns>The language.</returns>
    let private languageFromTag (tag: Tag) (length: int) =
        Language.ofString ((Tag.toString tag).Substring(0, length))

    /// <summary>Finds an ambiguous language tag in the generated map.</summary>
    /// <param name="tag">The tag.</param>
    /// <param name="index">The next index.</param>
    /// <returns>The language, or none.</returns>
    [<TailCall>]
    let rec private ambiguousLanguage (tag: Tag) (index: int) : Language voption =
        match index < OtTagTable.ambiguous.Length with
        | false -> ValueNone
        | true ->
            let struct (ambiguousTag, text) = OtTagTable.ambiguous[index]
            match ambiguousTag = tag with
            | true -> ValueSome (Language.ofString text)
            | false -> ambiguousLanguage tag (index + 1)

    /// <summary>Finds a two-letter tag in the generated array.</summary>
    /// <param name="tag">The tag.</param>
    /// <param name="index">The next index.</param>
    /// <returns>The language, or none.</returns>
    [<TailCall>]
    let rec private languages2Language (tag: Tag) (index: int) : Language voption =
        match index < OtTagTable.languages2.Length with
        | false -> ValueNone
        | true ->
            match OtTagTable.languages2[index].OtTag = tag with
            | true -> ValueSome (languageFromTag OtTagTable.languages2[index].Language 2)
            | false -> languages2Language tag (index + 1)

    /// <summary>Finds a three-letter tag in the generated array.</summary>
    /// <param name="tag">The tag.</param>
    /// <param name="index">The next index.</param>
    /// <returns>The language, or none.</returns>
    [<TailCall>]
    let rec private languages3Language (tag: Tag) (index: int) : Language voption =
        match index < OtTagTable.languages3.Length with
        | false -> ValueNone
        | true ->
            match OtTagTable.languages3[index].OtTag = tag with
            | true -> ValueSome (languageFromTag OtTagTable.languages3[index].Language 3)
            | false -> languages3Language tag (index + 1)

    /// <summary>Whether a multi-value range contains a tag.</summary>
    /// <param name="range">The range.</param>
    /// <param name="tag">The tag.</param>
    /// <param name="j">The next value.</param>
    /// <returns>The value index, or none.</returns>
    [<TailCall>]
    let rec private multiValueIndex (range: LangTagRange) (tag: Tag) (j: int) : int voption =
        match j < int range.Count with
        | false -> ValueNone
        | true ->
            match OtTagTable.languages3MultiValues[int range.Offset + j] = tag with
            | true -> ValueSome (int range.Offset + j)
            | false -> multiValueIndex range tag (j + 1)

    /// <summary>Finds a three-letter multi-value tag in the generated array.</summary>
    /// <param name="tag">The tag.</param>
    /// <param name="index">The next range index.</param>
    /// <returns>The language, or none.</returns>
    [<TailCall>]
    let rec private languages3MultiLanguage (tag: Tag) (index: int) : Language voption =
        match index < OtTagTable.languages3Multi.Length with
        | false -> ValueNone
        | true ->
            let range = OtTagTable.languages3Multi[index]
            match multiValueIndex range tag 0 with
            | ValueSome _ -> ValueSome (languageFromTag range.RangeLanguage 3)
            | ValueNone -> languages3MultiLanguage tag (index + 1)

    /// <summary>The <c>x-hbot-XXXXXXXX</c> fallback language of a tag.</summary>
    /// <param name="tag">The tag.</param>
    /// <returns>The language.</returns>
    let private xhbotLanguage (tag: Tag) =
        let value = uint32 tag
        let b0 = byte value
        let b1 = byte (value >>> 8)
        let b2 = byte (value >>> 16)
        let b3 = byte (value >>> 24)
        let prefix =
            match isAlpha (char b3) && isAlpha (char b2) && isAlpha (char b1) && b0 = Space with
            | true -> $"{toLower (char b3)}{toLower (char b2)}{toLower (char b1)}-"
            | false -> String.Empty
        Language.ofString (prefix + "x-hbot-" + value.ToString("x8"))

    /// <summary>Looks a tag up in the language arrays in order, <c>hb_ot_tag_to_language</c>.</summary>
    /// <param name="tag">The tag.</param>
    /// <param name="stage">The next array to scan.</param>
    /// <returns>The language.</returns>
    [<TailCall>]
    let rec private tagToLanguageStage (tag: Tag) (stage: LanguageStage) : Language =
        match stage with
        | Ambiguous ->
            match ambiguousLanguage tag 0 with
            | ValueSome language -> language
            | ValueNone -> tagToLanguageStage tag Languages2
        | Languages2 ->
            match languages2Language tag 0 with
            | ValueSome language -> language
            | ValueNone -> tagToLanguageStage tag Languages3
        | Languages3 ->
            match languages3Language tag 0 with
            | ValueSome language -> language
            | ValueNone -> tagToLanguageStage tag Languages3Multi
        | Languages3Multi ->
            match languages3MultiLanguage tag 0 with
            | ValueSome language -> language
            | ValueNone -> xhbotLanguage tag

    /// <summary>Scans for a subtag, continuing past alphanumeric neighbours, <c>subtag_matches</c>.</summary>
    /// <param name="text">The string.</param>
    /// <param name="start">The first search position.</param>
    /// <param name="limit">The limit index, or none.</param>
    /// <param name="subtag">The subtag.</param>
    /// <returns>True when a whole subtag matches.</returns>
    [<TailCall>]
    let rec private subtagScan (text: string) (start: int) (limit: int voption) (subtag: string) : bool =
        match text.IndexOf(subtag, start) with
        | -1 -> false
        | s when atOrPastLimit limit s -> false
        | s ->
            match followedByAlnum text (s + subtag.Length) with
            | true -> subtagScan text (s + subtag.Length) limit subtag
            | false -> true

    /// <summary>Tests a subtag match within a bounded window, <c>subtag_matches</c>.</summary>
    /// <param name="text">The string.</param>
    /// <param name="start">The first search position.</param>
    /// <param name="limit">The limit index, or none.</param>
    /// <param name="subtag">The subtag.</param>
    /// <returns>True when the subtag matches.</returns>
    let private subtagMatches (text: string) (start: int) (limit: int voption) (subtag: string) =
        match distanceToLimit limit start < subtag.Length with
        | true -> false
        | false -> subtagScan text start limit subtag

    /// <summary>Tests every subtag of a rule, <c>subtag_matches</c> chained with <c>&amp;&amp;</c>.</summary>
    /// <param name="text">The string.</param>
    /// <param name="start">The first search position.</param>
    /// <param name="limit">The limit index, or none.</param>
    /// <param name="subtags">The subtags.</param>
    /// <param name="index">The next subtag.</param>
    /// <returns>True when every subtag matches.</returns>
    [<TailCall>]
    let rec private subtagsMatch (text: string) (start: int) (limit: int voption) (subtags: ImmutableArray<string>) (index: int) : bool =
        match index < subtags.Length with
        | false -> true
        | true ->
            match subtagMatches text start limit subtags[index] with
            | true -> subtagsMatch text start limit subtags (index + 1)
            | false -> false

    /// <summary>Tests a whole language prefix match, <c>lang_matches</c>.</summary>
    /// <param name="text">The string.</param>
    /// <param name="limit">The limit index, or none.</param>
    /// <param name="spec">The specification after the first letter.</param>
    /// <returns>True when the specification matches.</returns>
    let private langMatches (text: string) (limit: int voption) (spec: string) =
        match distanceToLimit limit 1 < spec.Length with
        | true -> false
        | false ->
            match text.Length >= 1 + spec.Length && String.CompareOrdinal(text, 1, spec, 0, spec.Length) = 0 with
            | false -> false
            | true -> 1 + spec.Length = text.Length || text[1 + spec.Length] = '-'

    /// <summary>Checks the und-script rules of the complex-language matcher.</summary>
    /// <param name="text">The string.</param>
    /// <param name="p">The first dash.</param>
    /// <param name="limit">The limit index.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="capacity">The maximum number of tags.</param>
    /// <returns>The tags, or none.</returns>
    [<TailCall>]
    let rec private complexUnd (text: string) (p: int) (limit: int) (index: int) (capacity: int) : ImmutableArray<Tag> voption =
        match index < OtTagTable.complexRules.Length with
        | false -> ValueNone
        | true ->
            let rule = OtTagTable.complexRules[index]
            match rule.Head with
            | RuleHead.UndOnly ->
                match subtagsMatch text p (ValueSome limit) rule.Subtags 0 with
                | true -> ValueSome (emitTags capacity rule.Tags)
                | false -> complexUnd text p limit (index + 1) capacity
            | RuleHead.Exact _ | RuleHead.Prefix _ | RuleHead.LangMatches _ -> ValueNone

    /// <summary>Checks the head and subtags of one non-und complex rule.</summary>
    /// <param name="text">The string.</param>
    /// <param name="limit">The limit index, or none.</param>
    /// <param name="rule">The rule.</param>
    /// <param name="capacity">The maximum number of tags.</param>
    /// <returns>The tags, or none.</returns>
    let private trySwitchRuleHead (text: string) (limit: int voption) (rule: ComplexRule) (capacity: int) : ImmutableArray<Tag> voption =
        match rule.Head with
        | RuleHead.Exact exact ->
            match text.Length = 1 + exact.Length && String.CompareOrdinal(text, 1, exact, 0, exact.Length) = 0 with
            | true -> ValueSome (emitTags capacity rule.Tags)
            | false -> ValueNone
        | RuleHead.Prefix prefix ->
            match text.Length >= 1 + prefix.Length && String.CompareOrdinal(text, 1, prefix, 0, prefix.Length) = 0 && subtagsMatch text 0 limit rule.Subtags 0 with
            | true -> ValueSome (emitTags capacity rule.Tags)
            | false -> ValueNone
        | RuleHead.LangMatches spec ->
            match langMatches text limit spec with
            | true -> ValueSome (emitTags capacity rule.Tags)
            | false -> ValueNone
        | RuleHead.UndOnly -> ValueNone

    /// <summary>Checks one complex rule, including its group, <c>switch (lang_str[0])</c>.</summary>
    /// <param name="text">The string.</param>
    /// <param name="limit">The limit index, or none.</param>
    /// <param name="rule">The rule.</param>
    /// <param name="capacity">The maximum number of tags.</param>
    /// <returns>The tags, or none.</returns>
    let private trySwitchRule (text: string) (limit: int voption) (rule: ComplexRule) (capacity: int) : ImmutableArray<Tag> voption =
        match rule.Head with
        | RuleHead.UndOnly -> ValueNone
        | RuleHead.Exact _ | RuleHead.Prefix _ | RuleHead.LangMatches _ ->
            match rule.Group = text[0] with
            | false -> ValueNone
            | true -> trySwitchRuleHead text limit rule capacity

    /// <summary>Scans the non-und complex rules for the first match.</summary>
    /// <param name="text">The string.</param>
    /// <param name="limit">The limit index, or none.</param>
    /// <param name="index">The next rule.</param>
    /// <param name="capacity">The maximum number of tags.</param>
    /// <returns>The tags, or none.</returns>
    [<TailCall>]
    let rec private complexSwitch (text: string) (limit: int voption) (index: int) (capacity: int) : ImmutableArray<Tag> voption =
        match index < OtTagTable.complexRules.Length with
        | false -> ValueNone
        | true ->
            match trySwitchRule text limit (OtTagTable.complexRules[index]) capacity with
            | ValueSome tags -> ValueSome tags
            | ValueNone -> complexSwitch text limit (index + 1) capacity

    /// <summary>Runs the und block of the complex-language matcher.</summary>
    /// <param name="text">The string.</param>
    /// <param name="limit">The limit index, or none.</param>
    /// <param name="capacity">The maximum number of tags.</param>
    /// <returns>The tags, or none.</returns>
    let private complexUndBlock (text: string) (limit: int voption) (capacity: int) : ImmutableArray<Tag> voption =
        match limit with
        | ValueNone -> ValueNone
        | ValueSome limitIndex ->
            match limitIndex >= 7 with
            | false -> ValueNone
            | true ->
                match text.IndexOf('-') with
                | -1 -> ValueNone
                | p when p >= limitIndex || limitIndex - p < 5 -> ValueNone
                | p -> complexUnd text p limitIndex 0 capacity

    /// <summary>The complex-language match, <c>hb_ot_tags_from_complex_language</c>.</summary>
    /// <param name="text">The string.</param>
    /// <param name="limit">The limit index, or none.</param>
    /// <param name="capacity">The maximum number of tags.</param>
    /// <returns>The tags, or none.</returns>
    let private complexLanguage (text: string) (limit: int voption) (capacity: int) : ImmutableArray<Tag> voption =
        match complexUndBlock text limit capacity with
        | ValueSome tags -> ValueSome tags
        | ValueNone ->
            match text.Length with
            | 0 -> ValueNone
            | _ -> complexSwitch text limit 0 capacity

    /// <summary>Whether the next eight characters are hex digits.</summary>
    /// <param name="text">The string.</param>
    /// <param name="index">The first digit.</param>
    /// <param name="count">The digits checked so far.</param>
    /// <returns>True when all eight are hex.</returns>
    [<TailCall>]
    let rec private isHex8 (text: string) (index: int) (count: int) : bool =
        match count >= 8 with
        | true -> true
        | false ->
            match index + count < text.Length && hexValue text[index + count] >= 0 with
            | true -> isHex8 text index (count + 1)
            | false -> false

    /// <summary>Counts the leading alphanumeric characters, at most four.</summary>
    /// <param name="text">The string.</param>
    /// <param name="index">The first character.</param>
    /// <param name="count">The characters counted so far.</param>
    /// <returns>The count.</returns>
    [<TailCall>]
    let rec private alnumCount (text: string) (index: int) (count: int) : int =
        match count < 4 && index + count < text.Length && isAlnum text[index + count] with
        | true -> alnumCount text index (count + 1)
        | false -> count

    /// <summary>Normalises a character's case for a private-use tag.</summary>
    /// <param name="upper">Whether to uppercase.</param>
    /// <param name="c">The character.</param>
    /// <returns>The normalised character.</returns>
    let private normalizeChar (upper: bool) (c: char) =
        match upper with
        | true -> toUpper c
        | false -> toLower c

    /// <summary>Flips the case of a tag equal to <c>DFLT</c>, <c>parse_private_use_subtag</c>.</summary>
    /// <param name="tag">The tag.</param>
    /// <returns>The tag, possibly case-flipped.</returns>
    let private applyDfltFlip (tag: Tag) =
        match (uint32 tag &&& UpperAllMask) = uint32 DefaultScript with
        | true -> LanguagePrimitives.UInt32WithMeasure (uint32 tag ^^^ FlipCaseXor)
        | false -> tag

    /// <summary>Parses the eight hex digits of a private-use tag.</summary>
    /// <param name="text">The string.</param>
    /// <param name="index">The first digit.</param>
    /// <returns>The tag, or none.</returns>
    let private parsePrivateUseHex (text: string) (index: int) =
        match isHex8 text index 0 with
        | false -> ValueNone
        | true ->
            ValueSome (Tag.ofBytes
                (hexByte text index)
                (hexByte text (index + 2))
                (hexByte text (index + 4))
                (hexByte text (index + 6)))

    /// <summary>Parses the alphanumeric form of a private-use tag.</summary>
    /// <param name="text">The string.</param>
    /// <param name="index">The first character.</param>
    /// <param name="upper">Whether to uppercase.</param>
    /// <returns>The tag, or none.</returns>
    let private parsePrivateUseAlnum (text: string) (index: int) (upper: bool) =
        let count = alnumCount text index 0
        match count with
        | 0 -> ValueNone
        | _ ->
            let b0 = if 0 < count then normalizeChar upper text[index] else ' '
            let b1 = if 1 < count then normalizeChar upper text[index + 1] else ' '
            let b2 = if 2 < count then normalizeChar upper text[index + 2] else ' '
            let b3 = if 3 < count then normalizeChar upper text[index + 3] else ' '
            ValueSome (applyDfltFlip (Tag.make b0 b1 b2 b3))

    /// <summary>Parses a private-use tag after its prefix.</summary>
    /// <param name="text">The string.</param>
    /// <param name="index">The position after the prefix.</param>
    /// <param name="upper">Whether to uppercase the alphanumeric form.</param>
    /// <returns>The tag, or none.</returns>
    let private parsePrivateUseTag (text: string) (index: int) (upper: bool) =
        match index < text.Length && text[index] = '-' with
        | true -> parsePrivateUseHex text (index + 1)
        | false -> parsePrivateUseAlnum text index upper

    /// <summary>Parses a <c>-hbsc</c> or <c>-hbot</c> private-use subtag.</summary>
    /// <param name="text">The string.</param>
    /// <param name="privateUseSubtag">The private-use position, or none.</param>
    /// <param name="prefix">The subtag prefix.</param>
    /// <param name="upper">Whether to uppercase the alphanumeric form.</param>
    /// <param name="capacity">The output capacity; a zero capacity never parses.</param>
    /// <returns>The tag, or none.</returns>
    let private parsePrivateUseSubtag (text: string) (privateUseSubtag: int voption) (prefix: string) (upper: bool) (capacity: int) =
        match privateUseSubtag, capacity >= 1 with
        | ValueNone, _ -> ValueNone
        | _, false -> ValueNone
        | ValueSome start, true ->
            match text.IndexOf(prefix, start) with
            | -1 -> ValueNone
            | s -> parsePrivateUseTag text (s + prefix.Length) upper

    /// <summary>Finds the private-use singleton subtag and the limit before it.</summary>
    /// <param name="text">The string.</param>
    /// <param name="index">The next position.</param>
    /// <param name="limit">The first limit, if any.</param>
    /// <returns>The limit and the private-use position.</returns>
    [<TailCall>]
    let rec private findPrivateUse (text: string) (index: int) (limit: int voption) : struct (int voption * int voption) =
        match index >= text.Length with
        | true -> struct (firstLimit limit text.Length, ValueNone)
        | false ->
            match text[index - 1] = '-' && index + 1 < text.Length && text[index + 1] = '-' with
            | false -> findPrivateUse text (index + 1) limit
            | true ->
                match text[index] with
                | 'x' -> struct (firstLimit limit (index - 1), ValueSome index)
                | _ -> findPrivateUse text (index + 1) (firstLimit limit (index - 1))

    /// <summary>The private-use subtag and the limit, <c>hb_ot_tags_from_script_and_language</c>.</summary>
    /// <param name="text">The string.</param>
    /// <returns>The limit and the private-use position.</returns>
    let private useCase (text: string) : struct (int voption * int voption) =
        match text.Length >= 2 && text[0] = 'x' && text[1] = '-' with
        | true -> struct (ValueNone, ValueSome 0)
        | false -> findPrivateUse text 1 ValueNone

    /// <summary>The length of the extended-language subtag after a dash.</summary>
    /// <param name="text">The string.</param>
    /// <param name="firstDash">The first dash.</param>
    /// <returns>The length.</returns>
    let private extlangLength (text: string) (firstDash: int) =
        match text.IndexOf('-', firstDash + 1) with
        | -1 -> text.Length - firstDash - 1
        | extlangEnd -> extlangEnd - firstDash - 1

    /// <summary>The position of the first language component, after the extlang rule.</summary>
    /// <param name="text">The string.</param>
    /// <param name="firstDash">The first dash.</param>
    /// <param name="limit">The limit index, or none.</param>
    /// <returns>The component position.</returns>
    let private languageStart (text: string) (firstDash: int) (limit: int voption) =
        match firstDash with
        | -1 -> 0
        | _ when distanceToLimit limit 0 < 6 -> 0
        | _ ->
            match extlangLength text firstDash = 3 && isAlpha text[firstDash + 1] with
            | true -> firstDash + 1
            | false -> 0

    /// <summary>The length of the first language component.</summary>
    /// <param name="text">The string.</param>
    /// <param name="langStr">The component position.</param>
    /// <param name="limit">The limit index, or none.</param>
    /// <returns>The length.</returns>
    let private firstComponentLength (text: string) (langStr: int) (limit: int voption) =
        match text.IndexOf('-', langStr) with
        | -1 -> distanceToLimit limit langStr
        | dash -> dash - langStr

    /// <summary>The two-letter language tags, <c>first_len == 2</c>.</summary>
    /// <param name="text">The string.</param>
    /// <param name="langStr">The component position.</param>
    /// <param name="capacity">The maximum number of tags.</param>
    /// <returns>The tags.</returns>
    let private tagsFromLanguage2 (text: string) (langStr: int) (capacity: int) : ImmutableArray<Tag> =
        let langTag = Tag.ofString (text.Substring(langStr, 2))
        match findLanguage OtTagTable.languages2 0 (OtTagTable.languages2.Length - 1) langTag with
        | ValueNone -> ImmutableArray<Tag>.Empty
        | ValueSome tagIdx ->
            let first = firstEqualLanguage OtTagTable.languages2 tagIdx langTag
            let count = countTwoLetter OtTagTable.languages2 first langTag capacity 0
            Sealed.init count (fun index -> OtTagTable.languages2[first + index].OtTag)

    /// <summary>The multi-value and blocked branches of the three-letter lookup.</summary>
    /// <param name="langTag">The language tag.</param>
    /// <param name="capacity">The maximum number of tags.</param>
    /// <returns>The tags.</returns>
    let private tagsFromLanguage3MultiOrBlocked (langTag: Tag) (capacity: int) : ImmutableArray<Tag> =
        match findRange OtTagTable.languages3Multi 0 (OtTagTable.languages3Multi.Length - 1) langTag with
        | ValueSome rangeIdx ->
            let range = OtTagTable.languages3Multi[rangeIdx]
            let count = min capacity (int range.Count)
            Sealed.init count (fun index -> OtTagTable.languages3MultiValues[int range.Offset + index])
        | ValueNone ->
            match bfindTag OtTagTable.languages3Blocked 0 OtTagTable.languages3Blocked.Length langTag with
            | true -> ImmutableArray<Tag>.Empty
            | false -> emitOne (uppercaseIso6393 langTag)

    /// <summary>The three-letter language tags, <c>first_len == 3</c>.</summary>
    /// <param name="text">The string.</param>
    /// <param name="langStr">The component position.</param>
    /// <param name="capacity">The maximum number of tags.</param>
    /// <returns>The tags.</returns>
    let private tagsFromLanguage3 (text: string) (langStr: int) (capacity: int) : ImmutableArray<Tag> =
        let langTag = Tag.ofString (text.Substring(langStr, 3))
        match findLanguage OtTagTable.languages3 0 (OtTagTable.languages3.Length - 1) langTag with
        | ValueSome tagIdx -> emitOne OtTagTable.languages3[tagIdx].OtTag
        | ValueNone -> tagsFromLanguage3MultiOrBlocked langTag capacity

    /// <summary>The first-component language lookup, <c>hb_ot_tags_from_language</c>.</summary>
    /// <param name="text">The string.</param>
    /// <param name="limit">The limit index, or none.</param>
    /// <param name="capacity">The maximum number of tags.</param>
    /// <returns>The tags.</returns>
    let private tagsFromLanguageComponent (text: string) (limit: int voption) (capacity: int) : ImmutableArray<Tag> =
        let firstDash = text.IndexOf('-')
        let langStr = languageStart text firstDash limit
        let firstLen = firstComponentLength text langStr limit
        match firstLen with
        | 2 -> tagsFromLanguage2 text langStr capacity
        | 3 -> tagsFromLanguage3 text langStr capacity
        | _ ->
            let endIndex = if firstDash >= 0 then firstDash else text.Length
            match endIndex - langStr = 3 with
            | true -> emitOne (uppercaseIso6393 (Tag.ofString (text.Substring(langStr, 3))))
            | false -> ImmutableArray<Tag>.Empty

    /// <summary>The language tags of a string, <c>hb_ot_tags_from_language</c>.</summary>
    /// <param name="text">The string.</param>
    /// <param name="limit">The limit index, or none.</param>
    /// <param name="capacity">The maximum number of tags.</param>
    /// <returns>The tags.</returns>
    let private tagsFromLanguage (text: string) (limit: int voption) (capacity: int) : ImmutableArray<Tag> =
        match complexLanguage text limit capacity with
        | ValueSome tags -> tags
        | ValueNone -> tagsFromLanguageComponent text limit capacity

    /// <summary>Appends the script suffix to a language, <c>hb_ot_tags_to_script_and_language</c>.</summary>
    /// <param name="language">The language.</param>
    /// <param name="primary">The primary script tags.</param>
    /// <param name="scriptTag">The input script tag.</param>
    /// <returns>The language with the suffix when needed.</returns>
    let private appendScriptTag (language: Language) (primary: ImmutableArray<Tag>) (scriptTag: Tag) =
        match language with
        | NoLanguage -> NoLanguage
        | Bcp47 text ->
            match primary.IsEmpty || primary[0] <> scriptTag with
            | false -> language
            | true ->
                let prefix =
                    match text.Length >= 2 && text[0] = 'x' && text[1] = '-' with
                    | true -> String.Empty
                    | false -> "-x"
                Language.ofString (text + prefix + "-hbsc-" + (uint32 scriptTag).ToString("x8"))

    /// <summary>Converts a script and a language to OpenType script and language tags, <c>hb_ot_tags_from_script_and_language</c>.</summary>
    /// <param name="script">The script.</param>
    /// <param name="language">The language.</param>
    /// <param name="scriptCapacity">The maximum number of script tags.</param>
    /// <param name="languageCapacity">The maximum number of language tags.</param>
    /// <returns>The script tags and the language tags.</returns>
    let tagsFromScriptAndLanguage (script: Script) (language: Language) (scriptCapacity: int) (languageCapacity: int) =
        match language with
        | NoLanguage -> struct (scriptTags script scriptCapacity, ImmutableArray<Tag>.Empty)
        | Bcp47 text ->
            let struct (limit, privateUseSubtag) = useCase text
            let hbsc = parsePrivateUseSubtag text privateUseSubtag "-hbsc" false scriptCapacity
            let hbot = parsePrivateUseSubtag text privateUseSubtag "-hbot" true languageCapacity
            let scriptResult =
                match hbsc with
                | ValueSome tag -> emitOne tag
                | ValueNone -> scriptTags script scriptCapacity
            let languageResult =
                match hbot with
                | ValueSome tag -> emitOne tag
                | ValueNone ->
                    match languageCapacity >= 1 with
                    | true -> tagsFromLanguage text limit languageCapacity
                    | false -> ImmutableArray<Tag>.Empty
            struct (scriptResult, languageResult)

    /// <summary>Converts a script tag to a script, <c>hb_ot_tag_to_script</c>.</summary>
    /// <param name="tag">The tag.</param>
    /// <returns>The script.</returns>
    let tagToScript (tag: Tag) =
        match byte (uint32 tag) with
        | Digit2 | Digit3 -> newTagToScript (LanguagePrimitives.UInt32WithMeasure (uint32 tag &&& Tag2Mask))
        | _ -> oldTagToScript tag

    /// <summary>Converts a language tag to a language, <c>hb_ot_tag_to_language</c>.</summary>
    /// <param name="tag">The tag.</param>
    /// <returns>The language.</returns>
    let tagToLanguage (tag: Tag) =
        match tag = DefaultLanguage with
        | true -> NoLanguage
        | false -> tagToLanguageStage tag Ambiguous

    /// <summary>Converts a script tag and a language tag to a script and a language, <c>hb_ot_tags_to_script_and_language</c>.</summary>
    /// <param name="scriptTag">The script tag.</param>
    /// <param name="languageTag">The language tag.</param>
    /// <returns>The script and the language.</returns>
    let tagsToScriptAndLanguage (scriptTag: Tag) (languageTag: Tag) =
        let scriptOut = tagToScript scriptTag
        let primary = scriptTags scriptOut 1
        let language = tagToLanguage languageTag
        struct (scriptOut, appendScriptTag language primary scriptTag)

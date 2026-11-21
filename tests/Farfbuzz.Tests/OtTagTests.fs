module Farfbuzz.Tests.OtTagTests

open Expecto
open Farfbuzz

/// <summary>Checks a script tag round-trip, <c>test_simple_tags</c>.</summary>
/// <param name="spelling">The tag spelling.</param>
/// <param name="script">The script.</param>
let private simpleTags (spelling: string) (script: Script) =
    let tag = Tag.ofString spelling
    let struct (scriptTags, _) = OtTag.tagsFromScriptAndLanguage script Language.NoLanguage 2 0
    match scriptTags.Length with
    | 0 -> Expect.equal tag (Tag.ofString "DFLT") $"script tag for {spelling}"
    | _ -> Expect.equal scriptTags[0] tag $"script tag for {spelling}"
    Expect.equal (OtTag.tagToScript tag) script $"tag to script for {spelling}"

/// <summary>Checks an Indic script's three tags, <c>test_indic_tags</c>.</summary>
/// <param name="useTag">The USE tag.</param>
/// <param name="newTag">The new tag.</param>
/// <param name="oldTag">The old tag.</param>
/// <param name="script">The script.</param>
let private indicTags (useTag: string) (newTag: string) (oldTag: string) (script: Script) =
    let tag1 = Tag.ofString useTag
    let tag2 = Tag.ofString newTag
    let tag3 = Tag.ofString oldTag
    let struct (scriptTags, _) = OtTag.tagsFromScriptAndLanguage script Language.NoLanguage 3 0
    Expect.equal scriptTags.Length 3 $"script tag count for {useTag}"
    Expect.equal scriptTags[0] tag1 $"first tag for {useTag}"
    Expect.equal scriptTags[1] tag2 $"second tag for {useTag}"
    Expect.equal scriptTags[2] tag3 $"third tag for {useTag}"
    Expect.equal (OtTag.tagToScript tag1) script $"tag to script {useTag}"
    Expect.equal (OtTag.tagToScript tag2) script $"tag to script {newTag}"
    Expect.equal (OtTag.tagToScript tag3) script $"tag to script {oldTag}"

/// <summary>Checks a script tag produced from a language, <c>test_script_tags_from_language</c>.</summary>
/// <param name="spelling">The expected tag spelling, or none.</param>
/// <param name="language">The language spelling, or none.</param>
/// <param name="script">The script.</param>
let private scriptTagsFromLanguage (spelling: string voption) (language: string voption) (script: Script) =
    let tag =
        match spelling with
        | ValueSome text -> Tag.ofString text
        | ValueNone -> Tags.Empty
    let language =
        match language with
        | ValueSome text -> Language.ofString text
        | ValueNone -> NoLanguage
    let struct (scriptTags, _) = OtTag.tagsFromScriptAndLanguage script language 1 0
    match scriptTags.Length with
    | 0 -> ()
    | _ ->
        Expect.equal scriptTags.Length 1 "script tag count"
        Expect.equal scriptTags[0] tag "script tag"

/// <summary>Checks a language tag round-trip, <c>test_language_two_way</c>.</summary>
/// <param name="spelling">The tag spelling.</param>
/// <param name="language">The language spelling, or none.</param>
let private languageTwoWay (spelling: string) (language: string voption) =
    let tag = Tag.ofString spelling
    let language =
        match language with
        | ValueSome text -> Language.ofString text
        | ValueNone -> NoLanguage
    let struct (_, languageTags) = OtTag.tagsFromScriptAndLanguage Scripts.Invalid language 0 1
    match languageTags.Length with
    | 0 -> Expect.equal tag (Tag.ofString "dflt") $"language tag for {spelling}"
    | _ -> Expect.equal languageTags[0] tag $"language tag for {spelling}"
    Expect.equal (OtTag.tagToLanguage tag) language $"tag to language for {spelling}"

/// <summary>Checks the tag produced from a language, <c>test_tag_from_language</c>.</summary>
/// <param name="spelling">The expected tag spelling.</param>
/// <param name="language">The language spelling.</param>
let private tagFromLanguage (spelling: string) (language: string) =
    let tag = Tag.ofString spelling
    let struct (_, languageTags) = OtTag.tagsFromScriptAndLanguage Scripts.Invalid (Language.ofString language) 0 1
    match languageTags.Length with
    | 0 -> Expect.equal tag (Tag.ofString "dflt") $"language tag for {language}"
    | _ -> Expect.equal languageTags[0] tag $"language tag for {language}"

/// <summary>Checks the language of a tag, <c>test_tag_to_language</c>.</summary>
/// <param name="spelling">The tag spelling.</param>
/// <param name="language">The expected language spelling.</param>
let private tagToLanguageCase (spelling: string) (language: string) =
    Expect.equal (OtTag.tagToLanguage (Tag.ofString spelling)) (Language.ofString language) $"tag to language for {spelling}"

/// <summary>Checks the script and language of a tag pair, <c>test_tags_to_script_and_language</c>.</summary>
/// <param name="scriptTag">The script tag spelling.</param>
/// <param name="languageTag">The language tag spelling.</param>
/// <param name="script">The expected script spelling.</param>
/// <param name="language">The expected language spelling, or none.</param>
let private tagsToScriptAndLanguageCase (scriptTag: string) (languageTag: string) (script: string) (language: string voption) =
    let struct (scriptOut, languageOut) = OtTag.tagsToScriptAndLanguage (Tag.ofString scriptTag) (Tag.ofString languageTag)
    Expect.equal scriptOut (Script.ofString script) $"script for {scriptTag}"
    match language with
    | ValueSome text -> Expect.equal languageOut (Language.ofString text) $"language for {languageTag}"
    | ValueNone -> Expect.equal languageOut NoLanguage $"language for {languageTag}"

/// <summary>Checks the script and language tag lists, <c>test_tags</c>.</summary>
/// <param name="script">The script.</param>
/// <param name="language">The language spelling.</param>
/// <param name="scriptCapacity">The script capacity.</param>
/// <param name="languageCapacity">The language capacity.</param>
/// <param name="expectedScriptCount">The expected script count.</param>
/// <param name="expectedLanguageCount">The expected language count.</param>
/// <param name="expected">The expected tags in order.</param>
let private testTags (script: Script) (language: string) (scriptCapacity: int) (languageCapacity: int) (expectedScriptCount: int) (expectedLanguageCount: int) (expected: Tag list) =
    let struct (scriptTags, languageTags) = OtTag.tagsFromScriptAndLanguage script (Language.ofString language) scriptCapacity languageCapacity
    Expect.equal scriptTags.Length expectedScriptCount "script tag count"
    Expect.equal languageTags.Length expectedLanguageCount "language tag count"
    let actual = List.append (List.ofSeq scriptTags) (List.ofSeq languageTags)
    Expect.equal actual expected "tags"

/// <summary>The P3.S12 OpenType script and language tag tests.</summary>
[<Tests>]
let tests =
    testList "P3.S12" [
        test "script tags degenerate" {
            simpleTags "kana" Scripts.Katakana
            let struct (hiragana, _) = OtTag.tagsFromScriptAndLanguage Scripts.Hiragana Language.NoLanguage 2 0
            Expect.equal hiragana.Length 1 "hiragana count"
            Expect.equal hiragana[0] (Tag.ofString "kana") "hiragana tag"
            let hrkt = Script.ofString "Hrkt"
            let struct (hrktTags, _) = OtTag.tagsFromScriptAndLanguage hrkt Language.NoLanguage 2 0
            Expect.equal hrktTags.Length 1 "hrkt count"
            Expect.equal hrktTags[0] (Tag.ofString "kana") "hrkt tag"
            Expect.equal (OtTag.tagToScript (Tag.ofString "kana")) Scripts.Katakana "kana to script"
            simpleTags "DFLT" Scripts.Invalid
            Expect.equal (OtTag.tagToScript (Tag.ofString "be  ")) (Script.ofString "Beee") "spaces are replaced"
        }
        test "script tags simple" {
            simpleTags "wwyz" (Script.ofString "wWyZ")
            simpleTags "zyyy" Scripts.Common
            simpleTags "zinh" Scripts.Inherited
            simpleTags "zzzz" Scripts.Unknown
            simpleTags "arab" Scripts.Arabic
            simpleTags "copt" Scripts.Coptic
            simpleTags "kana" Scripts.Katakana
            simpleTags "latn" Scripts.Latin
            simpleTags "math" Scripts.Math
            simpleTags "lao " Scripts.Lao
            simpleTags "yi  " Scripts.Yi
            simpleTags "nko " Scripts.Nko
            simpleTags "vai " Scripts.Vai
            simpleTags "mtei" Scripts.MeeteiMayek
            simpleTags "mand" Scripts.Mandaic
        }
        test "script tags from language" {
            scriptTagsFromLanguage ValueNone ValueNone Scripts.Invalid
            scriptTagsFromLanguage ValueNone (ValueSome "en") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "copt") (ValueSome "en") Scripts.Coptic
            scriptTagsFromLanguage ValueNone (ValueSome "x-hbsc") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "copt") (ValueSome "x-hbsc") Scripts.Coptic
            scriptTagsFromLanguage ValueNone (ValueSome "x-hbsc-") Scripts.Invalid
            scriptTagsFromLanguage ValueNone (ValueSome "x-hbsc-1") Scripts.Invalid
            scriptTagsFromLanguage ValueNone (ValueSome "x-hbsc-1a") Scripts.Invalid
            scriptTagsFromLanguage ValueNone (ValueSome "x-hbsc-1a2b3c4x") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "2lon") (ValueSome "x-hbsc-326c6f6e67") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "abc ") (ValueSome "x-hbscabc") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "deva") (ValueSome "x-hbscdeva") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "dev2") (ValueSome "x-hbscdev2") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "dev3") (ValueSome "x-hbscdev3") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "dev3") (ValueSome "x-hbsc-64657633") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "copt") (ValueSome "x-hbotpap0-hbsccopt") Scripts.Invalid
            scriptTagsFromLanguage ValueNone (ValueSome "en-x-hbsc") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "copt") (ValueSome "en-x-hbsc") Scripts.Coptic
            scriptTagsFromLanguage (ValueSome "abc ") (ValueSome "en-x-hbscabc") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "deva") (ValueSome "en-x-hbscdeva") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "dev2") (ValueSome "en-x-hbscdev2") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "dev3") (ValueSome "en-x-hbscdev3") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "dev3") (ValueSome "en-x-hbsc-64657633") Scripts.Invalid
            scriptTagsFromLanguage (ValueSome "copt") (ValueSome "en-x-hbotpap0-hbsccopt") Scripts.Invalid
        }
        test "script tags Indic" {
            indicTags "bng3" "bng2" "beng" Scripts.Bengali
            indicTags "dev3" "dev2" "deva" Scripts.Devanagari
            indicTags "gjr3" "gjr2" "gujr" Scripts.Gujarati
            indicTags "gur3" "gur2" "guru" Scripts.Gurmukhi
            indicTags "knd3" "knd2" "knda" Scripts.Kannada
            indicTags "mlm3" "mlm2" "mlym" Scripts.Malayalam
            indicTags "ory3" "ory2" "orya" Scripts.Oriya
            indicTags "tml3" "tml2" "taml" Scripts.Tamil
            indicTags "tel3" "tel2" "telu" Scripts.Telugu
        }
        test "tags to script and language" {
            tagsToScriptAndLanguageCase "DFLT" "ENG" "" (ValueSome "en-x-hbsc-44464c54")
            tagsToScriptAndLanguageCase "latn" "ENG" "Latn" (ValueSome "en")
            tagsToScriptAndLanguageCase "deva" "MAR" "Deva" (ValueSome "mr-x-hbsc-64657661")
            tagsToScriptAndLanguageCase "dev2" "MAR" "Deva" (ValueSome "mr-x-hbsc-64657632")
            tagsToScriptAndLanguageCase "dev3" "MAR" "Deva" (ValueSome "mr")
            tagsToScriptAndLanguageCase "qaa" "QTZ0" "Qaaa" (ValueSome "x-hbot-51545a30-hbsc-71616120")
            tagsToScriptAndLanguageCase "DFLT" "dflt" "" ValueNone
        }
        test "language tags" {
            languageTwoWay "dflt" ValueNone
            languageTwoWay "ALT" (ValueSome "alt")
            languageTwoWay "ARA" (ValueSome "ar")
            languageTwoWay "AZE" (ValueSome "az")
            tagFromLanguage "AZE" "az-ir"
            tagFromLanguage "AZE" "az-az"
            languageTwoWay "ENG" (ValueSome "en")
            tagFromLanguage "ENG" "en_US"
            languageTwoWay "CJA" (ValueSome "cja-x-hbot-434a4120")
            languageTwoWay "CJM" (ValueSome "cjm-x-hbot-434a4d20")
            tagFromLanguage "CJM" "cjm"
            languageTwoWay "EVN" (ValueSome "eve")
            languageTwoWay "HAL" (ValueSome "cfm")
            tagFromLanguage "HAL" "flm"
            languageTwoWay "HYE0" (ValueSome "hy")
            languageTwoWay "HYE" (ValueSome "hyw")
            tagFromLanguage "QIN" "bgr"
            tagFromLanguage "QIN" "cbl"
            tagFromLanguage "QIN" "cka"
            tagFromLanguage "QIN" "cmr"
            tagFromLanguage "QIN" "cnb"
            tagFromLanguage "QIN" "cnh"
            tagFromLanguage "QIN" "cnk"
            tagFromLanguage "QIN" "cnw"
            tagFromLanguage "QIN" "csh"
            tagFromLanguage "QIN" "csy"
            tagFromLanguage "QIN" "ctd"
            tagFromLanguage "QIN" "czt"
            tagFromLanguage "QIN" "dao"
            tagFromLanguage "QIN" "hlt"
            tagFromLanguage "QIN" "mrh"
            tagFromLanguage "QIN" "pck"
            tagFromLanguage "QIN" "sez"
            tagFromLanguage "QIN" "tcp"
            tagFromLanguage "QIN" "tcz"
            tagFromLanguage "QIN" "yos"
            tagFromLanguage "QIN" "zom"
            tagToLanguageCase "QIN" "bgr"
            languageTwoWay "FAR" (ValueSome "fa")
            tagFromLanguage "FAR" "fa_IR"
            languageTwoWay "MNK" (ValueSome "man")
            languageTwoWay "SWA" (ValueSome "aii")
            languageTwoWay "SYR" (ValueSome "syr")
            tagFromLanguage "SYR" "amw"
            tagFromLanguage "SYR" "cld"
            tagFromLanguage "SYR" "syc"
            languageTwoWay "TUA" (ValueSome "tru")
            tagFromLanguage "ZHS" "zh"
            tagFromLanguage "ZHS" "zh-cn"
            tagFromLanguage "ZHS" "zh-sg"
            tagFromLanguage "ZHTM" "zh-mo"
            tagFromLanguage "ZHTM" "zh-hant-mo"
            tagFromLanguage "ZHS" "zh-hans-mo"
            languageTwoWay "ZHH" (ValueSome "zh-HK")
            tagFromLanguage "ZHH" "zH-HanT-hK"
            tagFromLanguage "ZHS" "zH-HanS-hK"
            tagFromLanguage "ZHT" "zh-tw"
            languageTwoWay "ZHS" (ValueSome "zh-Hans")
            languageTwoWay "ZHT" (ValueSome "zh-Hant")
            tagFromLanguage "ZHS" "zh-xx"
            tagFromLanguage "ZHS" "zh-Hans-TW"
            tagFromLanguage "ZHH" "yue"
            tagFromLanguage "ZHH" "yue-Hant"
            tagFromLanguage "ZHS" "yue-Hans"
            languageTwoWay "ABC" (ValueSome "abc-x-hbot-41424320")
            languageTwoWay "ABCD" (ValueSome "x-hbot-41424344")
            tagFromLanguage "ABC" "asdf-asdf-wer-x-hbotabc-zxc"
            tagFromLanguage "ABC" "asdf-asdf-wer-x-hbotabc"
            tagFromLanguage "ABCD" "asdf-asdf-wer-x-hbotabcd"
            tagFromLanguage "ABC" "asdf-asdf-wer-x-hbot-41424320-zxc"
            tagFromLanguage "ABC" "asdf-asdf-wer-x-hbot-41424320"
            tagFromLanguage "ABCD" "asdf-asdf-wer-x-hbot-41424344"
            tagFromLanguage "dflt" "asdf-asdf-wer-x-hbot"
            tagFromLanguage "dflt" "asdf-asdf-wer-x-hbot-zxc"
            tagFromLanguage "dflt" "asdf-asdf-wer-x-hbot-zxc-414243"
            tagFromLanguage "dflt" "asdf-asdf-wer-x-hbot-414243"
            tagFromLanguage "dflt" "asdf-asdf-wer-x-hbot-4142432"
            tagFromLanguage "dflt" "xy"
            tagFromLanguage "XYZ" "xyz"
            tagFromLanguage "XYZ" "xyz-qw"
            tagFromLanguage "IPPH" "-fonipa"
            tagFromLanguage "ENG" "en-fonipax"
            tagFromLanguage "ENG" "en-x-fonipa"
            tagFromLanguage "ENG" "en-a-fonipa"
            tagFromLanguage "ENG" "en-a-qwe-b-fonipa"
            tagFromLanguage "IPPH" "en-fonipa"
            tagFromLanguage "IPPH" "en-fonipax-fonipa"
            tagFromLanguage "IPPH" "rm-CH-fonipa-sursilv-x-foobar"
            languageTwoWay "IPPH" (ValueSome "und-fonipa")
            tagFromLanguage "IPPH" "zh-fonipa"
            tagFromLanguage "APPH" "en-fonnapa"
            tagFromLanguage "APPH" "chr-fonnapa"
            languageTwoWay "APPH" (ValueSome "und-fonnapa")
            tagFromLanguage "KGE" "ka-Geok"
            languageTwoWay "KGE" (ValueSome "und-Geok")
            tagFromLanguage "IRT" "ga-Latg"
            languageTwoWay "IRT" (ValueSome "ghc")
            languageTwoWay "MOL" (ValueSome "ro-MD")
            languageTwoWay "PGR" (ValueSome "el-polyton")
            tagFromLanguage "PGR" "el-CY-polyton"
            tagFromLanguage "SYRE" "aii-Syre"
            tagFromLanguage "SYRE" "de-Syre"
            tagFromLanguage "SYRE" "syr-Syre"
            languageTwoWay "SYRE" (ValueSome "und-Syre")
            tagFromLanguage "SYRJ" "aii-Syrj"
            tagFromLanguage "SYRJ" "de-Syrj"
            tagFromLanguage "SYRJ" "syr-Syrj"
            languageTwoWay "SYRJ" (ValueSome "und-Syrj")
            tagFromLanguage "SYRN" "aii-Syrn"
            tagFromLanguage "SYRN" "de-Syrn"
            tagFromLanguage "SYRN" "syr-Syrn"
            languageTwoWay "SYRN" (ValueSome "und-Syrn")
            tagFromLanguage "ABC" "fa-x-hbotabc-hbot-41686121-zxc"
            tagFromLanguage "ABC" "fa-ir-x-hbotabc-hbot-41686121-zxc"
            tagFromLanguage "ABC" "zh-x-hbotabc-hbot-41686121-zxc"
            tagFromLanguage "ABC" "zh-cn-x-hbotabc-hbot-41686121-zxc"
            tagFromLanguage "ABC" "zh-xy-x-hbotabc-hbot-41686121-zxc"
            tagFromLanguage "ABC" "xyz-xy-x-hbotabc-hbot-41686121-zxc"
            tagFromLanguage "Aha!" "fa-x-hbot-41686121-hbotabc-zxc"
            tagFromLanguage "Aha!" "fa-ir-x-hbot-41686121-hbotabc-zxc"
            tagFromLanguage "Aha!" "zh-x-hbot-41686121-hbotabc-zxc"
            tagFromLanguage "Aha!" "zh-cn-x-hbot-41686121-hbotabc-zxc"
            tagFromLanguage "Aha!" "zh-xy-x-hbot-41686121-hbotabc-zxc"
            tagFromLanguage "Aha!" "xyz-xy-x-hbot-41686121-hbotabc-zxc"
            tagFromLanguage "dflt" "x-hbot"
            tagFromLanguage "dflt" "x-hbot-"
            tagFromLanguage "dflt" "x-hbot-1"
            tagFromLanguage "dflt" "x-hbot-1a"
            tagFromLanguage "dflt" "x-hbot-1a2b3c4x"
            tagFromLanguage "2lon" "x-hbot-326c6f6e67"
            tagFromLanguage "ARA" "ar-aao"
            tagFromLanguage "JBO" "art-lojban"
            tagFromLanguage "KOK" "kok-gom"
            tagFromLanguage "LTZ" "i-lux"
            tagFromLanguage "MNG" "drh"
            tagFromLanguage "MOR" "ar-ary"
            tagFromLanguage "MOR" "ar-ary-DZ"
            tagFromLanguage "NOR" "no-bok"
            tagFromLanguage "NYN" "no-nyn"
            tagFromLanguage "ZHS" "i-hak"
            tagFromLanguage "ZHS" "zh-guoyu"
            tagFromLanguage "ZHS" "zh-min"
            tagFromLanguage "ZHS" "zh-min-nan"
            tagFromLanguage "ZHS" "zh-xiang"
            tagFromLanguage "SQI" "als"
            tagFromLanguage "dflt" "far"
            tagFromLanguage "ARA" "ar-001"
            tagFromLanguage "TRK" "tr@foo=bar"
        }
        test "full tags" {
            testTags Scripts.Invalid "en" 3 3 0 1 [ Tag.ofString "ENG" ]
            testTags Scripts.Invalid "en-x-hbscdflt" 3 3 1 1 [ Tag.ofString "DFLT"; Tag.ofString "ENG" ]
            testTags Scripts.Latin "en" 3 3 1 1 [ Tag.ofString "latn"; Tag.ofString "ENG" ]
            testTags (Script.ofString "Hrkt") "und" 3 0 1 0 [ Tag.ofString "kana" ]
            testTags Scripts.Latin "en" 0 0 0 0 []
            testTags Scripts.Invalid "und-fonnapa" 3 3 0 1 [ Tag.ofString "APPH" ]
            testTags Scripts.Invalid "en-fonnapa" 3 3 0 1 [ Tag.ofString "APPH" ]
            testTags Scripts.Invalid "x-hbot1234-hbsc5678" 3 3 1 1 [ Tag.ofString "5678"; Tag.ofString "1234" ]
            testTags Scripts.Invalid "x-hbsc5678-hbot1234" 3 3 1 1 [ Tag.ofString "5678"; Tag.ofString "1234" ]
            testTags Scripts.Malayalam "ml" 3 3 3 2 [ Tag.ofString "mlm3"; Tag.ofString "mlm2"; Tag.ofString "mlym"; Tag.ofString "MAL"; Tag.ofString "MLR" ]
            testTags Scripts.Malayalam "ml" 1 1 1 1 [ Tag.ofString "mlm3"; Tag.ofString "MAL" ]
            testTags Scripts.Myanmar "und" 3 0 2 0 [ Tag.ofString "mym2"; Tag.ofString "mymr" ]
            testTags Scripts.Invalid "xyz" 3 3 0 1 [ Tag.ofString "XYZ" ]
            testTags Scripts.Invalid "xy" 3 3 0 0 []
        }
    ]

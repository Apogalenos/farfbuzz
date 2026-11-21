module Farfbuzz.Tests.Shape.PropsTests

open System
open System.Runtime.InteropServices
open Expecto
open Farfbuzz
open Farfbuzz.Harness

/// <summary>Builds a run from code points, one per cluster.</summary>
/// <param name="codepoints">The code points.</param>
/// <returns>The run.</returns>
let private runOf (codepoints: uint32 array) =
    GlyphRun.addCodepoints (ReadOnlySpan codepoints) 0 -1 GlyphRun.empty |> TailCalls.settle

/// <summary>The properties of one isolated code point: <c>UnicodeProps.compute</c> plus the single-glyph continuation rules.</summary>
/// <param name="codepoint">The code point.</param>
/// <returns>The packed properties.</returns>
let private expectedSingle (codepoint: uint32) : uint16 =
    let struct (unicode, _) = UnicodeProps.compute codepoint
    let ruleContinuation =
        (Unicode.generalCategory codepoint = GeneralCategory.ModifierSymbol && codepoint >= 0x1F3FBu && codepoint <= 0x1F3FFu)
        || codepoint = 0x200Du
        || (codepoint >= 0xFF9Eu && codepoint <= 0xFF9Fu)
        || (codepoint >= 0xE0020u && codepoint <= 0xE007Fu)
    if ruleContinuation then unicode ||| UnicodeProps.Continuation else unicode

/// <summary>The P4.S02 Unicode-props accessors, synthesized classes and Sealed builders.</summary>
[<Tests>]
let tests =
    testList "P4.S02" [
        testProperty "setUnicodeProps equals compute plus the continuation rules for every code point" <| fun (codepoint: uint32) ->
            let codepoint = codepoint % 0x30000u
            let run = runOf [| codepoint |]
            let shaped = OtProps.setUnicodeProps 0xFFFFFFFFu run
            Expect.equal shaped.Props[0].UnicodeProps (expectedSingle codepoint) $"props for U+{codepoint:X4}"
        test "four regional indicators alternate their continuation" {
            let run = runOf [| 0x1F1E6u; 0x1F1E7u; 0x1F1E8u; 0x1F1E9u |]
            let shaped = OtProps.setUnicodeProps 0xFFFFFFFFu run
            let flags = shaped.Props |> Seq.map UnicodeProps.isContinuation |> Seq.toArray
            Expect.equal flags [| false; true; false; true |] "continuation flags"
        }
        test "ZWJ and Extended_Pictographic both become continuations" {
            let run = runOf [| 0x1F469u; 0x200Du; 0x1F4BBu |]
            let shaped = OtProps.setUnicodeProps 0xFFFFFFFFu run
            Expect.isFalse (UnicodeProps.isContinuation shaped.Props[0]) "woman"
            Expect.isTrue (UnicodeProps.isContinuation shaped.Props[1]) "ZWJ"
            Expect.isTrue (UnicodeProps.isContinuation shaped.Props[2]) "laptop"
        }
        test "an emoji modifier is a continuation" {
            let run = runOf [| 0x1F44Du; 0x1F3FDu |]
            let shaped = OtProps.setUnicodeProps 0xFFFFFFFFu run
            Expect.isFalse (UnicodeProps.isContinuation shaped.Props[0]) "thumbs up"
            Expect.isTrue (UnicodeProps.isContinuation shaped.Props[1]) "skin tone"
        }
        test "a halfwidth katakana voiced mark is a continuation" {
            let run = runOf [| 0xFF76u; 0xFF9Eu |]
            let shaped = OtProps.setUnicodeProps 0xFFFFFFFFu run
            Expect.isFalse (UnicodeProps.isContinuation shaped.Props[0]) "ka"
            Expect.isTrue (UnicodeProps.isContinuation shaped.Props[1]) "voiced mark"
        }
        test "an emoji tag sequence makes every tag character a continuation" {
            let run = runOf [| 0x1F3F4u; 0xE0067u; 0xE0062u; 0xE0073u; 0xE0063u; 0xE0074u; 0xE007Fu |]
            let shaped = OtProps.setUnicodeProps 0xFFFFFFFFu run
            Expect.isFalse (UnicodeProps.isContinuation shaped.Props[0]) "black flag"
            for index in 1 .. 6 do
                Expect.isTrue (UnicodeProps.isContinuation shaped.Props[index]) $"tag {index}"
        }
        test "a fraction slash sets the scratch flag" {
            let run = runOf [| 0x2044u |]
            let shaped = OtProps.setUnicodeProps 0xFFFFFFFFu run
            Expect.isTrue ((shaped.ScratchFlags &&& ScratchFlags.HasFractionSlash) = ScratchFlags.HasFractionSlash) "has fraction slash"
        }
        testProperty "the global mask lands on every glyph" <| fun (codepoints: uint32 array) ->
            let run = runOf codepoints
            let mask = 0x80000000u
            let shaped = OtProps.setUnicodeProps mask run
            Expect.isTrue (shaped.Infos |> Seq.forall (fun info -> info.Mask = mask)) "mask on every glyph"
        test "synthesized glyph classes mark only non-ignorable non-spacing marks" {
            let run = runOf [| 0x0301u; 0x034Fu; 0x0903u |]
            let shaped = run |> OtProps.setUnicodeProps 0xFFFFFFFFu |> OtProps.synthesizeGlyphClasses
            Expect.equal shaped.Props[0].GlyphClass GlyphPropsBits.Mark "U+0301 mark"
            Expect.equal shaped.Props[1].GlyphClass GlyphPropsBits.Base "U+034F base (ignorable)"
            Expect.equal shaped.Props[2].GlyphClass GlyphPropsBits.Base "U+0903 Mc base"
        }
        test "mapIfChanged identity returns the same array" {
            let items = Sealed.init 100 id
            let result = Sealed.mapIfChanged id items
            Expect.isTrue (Object.ReferenceEquals(ImmutableCollectionsMarshal.AsArray items, ImmutableCollectionsMarshal.AsArray result)) "reference-equal"
        }
    ]

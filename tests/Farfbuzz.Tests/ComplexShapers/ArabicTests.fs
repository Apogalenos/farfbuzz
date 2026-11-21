module Farfbuzz.Tests.ComplexShapers.ArabicTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.ComplexShapers.ShaperTestSupport

[<Literal>]
let private Beh = 0x0628u

[<Literal>]
let private Alef = 0x0627u

[<Literal>]
let private Dal = 0x062Fu

[<Literal>]
let private Lam = 0x0644u

[<Literal>]
let private Fatha = 0x064Eu

[<Literal>]
let private Shadda = 0x0651u

[<Literal>]
let private HamzaAbove = 0x0654u

[<Literal>]
let private Zwnj = 0x200Cu

[<Literal>]
let private Alaph = 0x0710u

[<Literal>]
let private Dalath = 0x0715u

[<Literal>]
let private MongolianA = 0x1820u

[<Literal>]
let private FreeVariationSelector1 = 0x180Bu

[<Literal>]
let private GlobalMask = 0x80000000u

/// <summary>The plan data of the tests: one bit for each action above the two low bits, which hold the glyph flags, in the order of the actions, and none for no action.</summary>
let private data : ArabicPlan =
    { MaskArray = ImmutableArray.CreateRange [ 0x10u; 0x20u; 0x40u; 0x80u; 0x100u; 0x200u; 0x400u; 0u ]
      HasStch = true
      DoFallback = true }

/// <summary>The actions of the glyphs of a run after the masks are set.</summary>
/// <param name="run">The run.</param>
/// <returns>The action bytes.</returns>
let private actionsOf (run: GlyphRun) : byte array = [| for props in run.Props -> props.ShaperAuxiliary |]

/// <summary>Sets the joining of a text for a script, with an item context.</summary>
/// <param name="script">The script.</param>
/// <param name="before">The context before, nearest first.</param>
/// <param name="after">The context after, nearest first.</param>
/// <param name="text">The text.</param>
/// <returns>The run with its masks set.</returns>
let private joined (script: Script) (before: uint32 array) (after: uint32 array) (text: uint32 array) : GlyphRun =
    let run = runWith ClusterLevel.MonotoneGraphemes GlobalMask text
    Arabic.setupMasks data script { run with ContextBefore = ImmutableArray.CreateRange before; ContextAfter = ImmutableArray.CreateRange after }

/// <summary>The actions the joining machine gives a text.</summary>
/// <param name="text">The text.</param>
/// <returns>The actions, 0 isolated, 1 final, 2 final two, 3 final three, 4 medial, 5 medial two, 6 initial, 7 none.</returns>
let private actionsFor (text: uint32 array) : byte array = actionsOf (joined Scripts.Arabic [||] [||] text)

/// <summary>The P6.S11 Arabic shaper tests: the record, the features and pauses, the joining machine with its context and its
/// Syriac and Mongolian cases, the stretch record, the reordering of the modifier combining marks and the Unicode fallback.</summary>
[<Tests>]
let tests =
    testList "P6.S11" [
        testCase "arabic shaper record" (fun _ ->
            let record = Categorize.profile ArabicShaper
            Expect.equal record.Kind ArabicShaper "kind"
            Expect.equal record.GposTag Tags.Empty "gpos tag"
            Expect.equal record.Normalization NormalizationAuto "normalization preference"
            Expect.equal record.ZeroWidthMarks ZeroWidthMarksByGdefLate "zero-width marks"
            Expect.isTrue record.FallbackPosition "fallback position")
        testCase "collect_features adds the pauses of the shaper in upstream's order, with the fallback for Arabic only" (fun _ ->
            let arabic = { Direction = Direction.Rtl; Script = Scripts.Arabic; Language = NoLanguage }
            let syriac = { arabic with Script = Scripts.Syriac }
            let pausesOf props hasRclt = [ for pause in (ShaperHooks.collectFeaturesFor (fun _ -> hasRclt) props ArabicShaper ImmutableArray.Empty).GsubPauses -> pause.StagePause ]
            let joining = List.replicate 7 NoPause
            Expect.equal (pausesOf arabic false) ([ NoPause; ArabicRecordStch; NoPause ] @ joining @ [ ArabicReleaseAction; ArabicFallbackShape; NoPause ]) "arabic"
            Expect.equal (pausesOf arabic true) ([ NoPause; ArabicRecordStch; NoPause ] @ joining @ [ ArabicReleaseAction; ArabicFallbackShape ]) "rclt present"
            Expect.equal (pausesOf syriac false) ([ NoPause; ArabicRecordStch; NoPause ] @ joining @ [ ArabicReleaseAction; NoPause ]) "syriac")
        testCase "the features land in the stages upstream's order gives them" (fun _ ->
            let props = { Direction = Direction.Rtl; Script = Scripts.Arabic; Language = NoLanguage }
            let collected = ShaperHooks.collectFeatures props ArabicShaper ImmutableArray.Empty
            let staged = [ for info in collected.Features -> Tag.toString info.InfoTag, info.GsubStage ]
            let stageOf (name: string) = staged |> List.find (fun (tag, _) -> tag = name) |> snd
            Expect.equal ([ "rvrn"; "stch"; "ccmp"; "locl" ] |> List.map stageOf) [ 0; 1; 2; 2 ] "before the joining features"
            Expect.equal ([ "isol"; "fina"; "fin2"; "fin3"; "medi"; "med2"; "init" ] |> List.map stageOf) [ 3; 4; 5; 6; 7; 8; 9 ] "one stage for each joining feature"
            Expect.equal ([ "rlig"; "calt"; "liga"; "clig"; "mset" ] |> List.map stageOf) [ 11; 12; 13; 13; 13 ] "after the release and the fallback")
        testCase "the joining features keep joiners, those the Unicode forms can stand in for have a fallback, the Syriac ones do not" (fun _ ->
            let props = { Direction = Direction.Rtl; Script = Scripts.Arabic; Language = NoLanguage }
            let collected = ShaperHooks.collectFeatures props ArabicShaper ImmutableArray.Empty
            let flagsOf (name: string) = collected.Features |> Seq.find (fun info -> Tag.toString info.InfoTag = name) |> fun info -> info.InfoFlags
            Expect.equal (flagsOf "init") (FeatureFlags.ManualZwj ||| FeatureFlags.HasFallback) "init"
            Expect.equal (flagsOf "fin2") FeatureFlags.ManualZwj "fin2"
            Expect.equal (flagsOf "rlig") (FeatureFlags.Global ||| FeatureFlags.ManualZwj ||| FeatureFlags.HasFallback) "rlig"
            Expect.equal (flagsOf "stch") FeatureFlags.Global "stch"
            Expect.equal (flagsOf "calt") (FeatureFlags.Global ||| FeatureFlags.ManualZwj) "calt")
        testCase "a lone letter is isolated and a word is initial, medial, final" (fun _ ->
            Expect.equal (actionsFor [| Beh |]) [| 0uy |] "isolated"
            Expect.equal (actionsFor [| Beh; Beh |]) [| 6uy; 1uy |] "initial, final"
            Expect.equal (actionsFor [| Beh; Beh; Beh |]) [| 6uy; 4uy; 1uy |] "initial, medial, final")
        testCase "a right-joining letter joins only to the one before it" (fun _ ->
            Expect.equal (actionsFor [| Alef; Beh |]) [| 0uy; 0uy |] "alef then beh"
            Expect.equal (actionsFor [| Beh; Alef |]) [| 6uy; 1uy |] "beh then alef"
            Expect.equal (actionsFor [| Beh; Dal; Beh |]) [| 6uy; 1uy; 0uy |] "beh, dal, beh")
        testCase "a transparent mark takes no action and joining goes through it" (fun _ ->
            Expect.equal (actionsFor [| Beh; Fatha; Beh |]) [| 6uy; 7uy; 1uy |] "mark between")
        testCase "a zero width non-joiner breaks the joining" (fun _ ->
            Expect.equal (actionsFor [| Beh; Zwnj; Beh |]) [| 0uy; 7uy; 0uy |] "broken")
        testCase "the masks are those of the actions on top of the glyphs' own" (fun _ ->
            let run = joined Scripts.Arabic [||] [||] [| Beh; Beh; Beh |]
            let masks = masksOf run
            let withoutGlyphFlags = masks |> Array.map (fun mask -> mask &&& ~~~0x3u)
            Expect.equal withoutGlyphFlags [| GlobalMask ||| 0x400u; GlobalMask ||| 0x100u; GlobalMask ||| 0x20u |] "initial, medial, final, the glyph flags aside"
            Expect.notEqual masks[1] withoutGlyphFlags[1] "the joined letters are marked unsafe to break")
        testCase "the context before the text decides the state the text starts in" (fun _ ->
            Expect.equal (actionsOf (joined Scripts.Arabic [| Beh |] [||] [| Beh |])) [| 1uy |] "a joining letter before: final"
            Expect.equal (actionsOf (joined Scripts.Arabic [| Alef |] [||] [| Beh |])) [| 0uy |] "a right-joining letter before: isolated"
            Expect.equal (actionsOf (joined Scripts.Arabic [| Fatha; Beh |] [||] [| Beh |])) [| 1uy |] "a transparent mark is looked through")
        testCase "the context after the text can turn the last letter initial" (fun _ ->
            Expect.equal (actionsOf (joined Scripts.Arabic [||] [| Beh |] [| Beh |])) [| 6uy |] "a joining letter after: initial"
            Expect.equal (actionsOf (joined Scripts.Arabic [||] [| Zwnj; Beh |] [| Beh |])) [| 0uy |] "a joiner after breaks it")
        testCase "syriac alaph and dalath have their own columns" (fun _ ->
            Expect.equal (actionsFor [| Alaph; Alaph |]) [| 0uy; 2uy |] "alaph, alaph: isolated, final two"
            Expect.equal (actionsFor [| Beh; Alaph; Alaph |]) [| 6uy; 5uy; 2uy |] "beh, alaph, alaph: initial, medial two, final two"
            Expect.equal (actionsFor [| Dalath; Alaph |]) [| 0uy; 3uy |] "dalath, alaph: isolated, final three")
        testCase "a Mongolian variation selector takes the action of the letter before it" (fun _ ->
            let run = joined Scripts.Mongolian [||] [||] [| MongolianA; MongolianA; FreeVariationSelector1 |]
            Expect.equal (actionsOf run) [| 6uy; 1uy; 1uy |] "initial, final, final"
            Expect.equal (actionsOf (joined Scripts.Arabic [||] [||] [| MongolianA; MongolianA; FreeVariationSelector1 |])) [| 6uy; 1uy; 7uy |] "not for other scripts")
        testCase "an empty run has nothing to join" (fun _ ->
            let run = joined Scripts.Arabic [||] [||] [||]
            Expect.equal run.Infos.Length 0 "no glyph")
        testCase "record_stch marks the glyphs a stch lookup multiplied by the parity of their component" (fun _ ->
            let run = runOf [| Beh; Beh; Beh; Beh |]
            let multiplied = [| 2uy; 3uy |]
            let props =
                Sealed.mapi (fun index (props: GlyphProps) ->
                    match index with
                    | 1 | 2 -> { props with GlyphClass = props.GlyphClass ||| GlyphPropsBits.Multiplied; LigProps = multiplied[index - 1] }
                    | _ -> props) (run.Props.AsSpan())
            let recorded = Arabic.recordStch data { run with Props = props }
            Expect.equal (actionsOf recorded) [| 0uy; 8uy; 9uy; 0uy |] "fixed, repeating"
            Expect.notEqual (recorded.ScratchFlags &&& ScratchFlags.Shaper0) ScratchFlags.Default "the flag"
            let without = Arabic.recordStch { data with HasStch = false } { run with Props = props }
            Expect.equal (actionsOf without) [| 0uy; 0uy; 0uy; 0uy |] "no stch in the plan: nothing recorded")
        testCase "a modifier combining mark goes in front of the marks of its class and is renumbered" (fun _ ->
            let run = runOf [| Shadda; HamzaAbove |]
            let reordered = Arabic.reorderMarks run 0 2
            Expect.equal (codepointsOf reordered) [| HamzaAbove; Shadda |] "hamza above first"
            Expect.equal (clustersOf reordered) [| 0u; 0u |] "one cluster"
            Expect.equal (UnicodeProps.modifiedCombiningClass reordered.Props[0]) 26uy "renumbered 26, the macro HB_MODIFIED_COMBINING_CLASS_CCC26"
            Expect.equal (UnicodeProps.modifiedCombiningClass reordered.Props[1]) 27uy "shadda keeps its modified class")
        testCase "a modifier combining mark of class 220 is renumbered 25, the macro HB_MODIFIED_COMBINING_CLASS_CCC22" (fun _ ->
            let run = runOf [| 0x0655u; 0x0656u |]
            let reordered = Arabic.reorderMarks run 0 2
            Expect.equal (codepointsOf reordered) [| 0x0655u; 0x0656u |] "already first"
            Expect.equal (UnicodeProps.modifiedCombiningClass reordered.Props[0]) 25uy "renumbered"
            Expect.equal (UnicodeProps.modifiedCombiningClass reordered.Props[1]) 220uy "the mark that is not one keeps its class")
        testCase "marks that are not modifier combining marks stay where they are" (fun _ ->
            let run = runOf [| Shadda; Fatha |]
            Expect.equal (codepointsOf (Arabic.reorderMarks run 0 2)) [| Shadda; Fatha |] "unchanged")
        testCase "the fallback joins with the Unicode presentation forms when the font has them and no joining lookups" (fun _ ->
            let font = fontMapping [ Beh, 1u; 0xFE8Fu, 2u; 0xFE90u, 3u; 0xFE91u, 4u; 0xFE92u, 5u ]
            let props = { Direction = Direction.Rtl; Script = Scripts.Arabic; Language = NoLanguage }
            let run =
                GlyphRun.addCodepoints (ReadOnlySpan [| Beh; Beh; Beh |]) 0 -1 GlyphRun.empty
                |> GlyphRun.withSegment props
                |> GlyphRun.withClusterLevel ClusterLevel.MonotoneGraphemes
                |> TailCalls.settle
            match Shape.shape font ImmutableArray<Feature>.Empty run with
            | Error error -> failtest $"expected a shaped run, got {error}"
            | Ok shapedRun -> Expect.equal [| for info in shapedRun.Infos -> info.Codepoint |] [| 3u; 5u; 4u |] "final, medial, initial, right to left")
        Conformance.files "conformance" [
            "in-house", "arabic-mark-order.tests"; "in-house", "arabic-normalization.tests"; "in-house", "digits.tests"
            "in-house", "mark-filtering-sets.tests"; "in-house", "nested-mark-filtering-sets.tests"; "in-house", "uniscribe.tests"
            "in-house", "coretext.tests"; "text-rendering-tests", "GPOS-5.tests"; "in-house", "arabic-stch.tests"
            "in-house", "arabic-fallback-shaping.tests" ]
    ]

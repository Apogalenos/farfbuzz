module Farfbuzz.Tests.Glyph.CffBoundsKernelTests

open System
open System.Collections.Immutable
open System.IO
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Tests.Allocation

[<Literal>]
let private CaseCount = 20000

/// <summary>A blend context with made-up scalars, for the property over arbitrary charstrings: the same numbers reach the
/// reference interpreter and the bounds interpreter.</summary>
[<Struct>]
type private StubBlend =
    { Blending: bool
      Regions: int
      Ivs: int }
    interface ICharStringBlend<StubBlend> with
        member _.Blending blend = blend.Blending
        member _.RegionCount (blend, ivs) = match ivs < 2 with | true -> blend.Regions | false -> 0
        member _.Scalar (_, ivs, index) = float32 (index + 1 + ivs) * 0.375f - 0.5f
        member _.Ivs blend = blend.Ivs
    interface ICffBlendSource<StubBlend> with
        member _.Blending blend = blend.Blending
        member _.RegionCount (blend, ivs) = match ivs < 2 with | true -> blend.Regions | false -> 0
        member _.Scalar (_, ivs, index) = float32 (index + 1 + ivs) * 0.375f - 0.5f

/// <summary>Whether two doubles have the same bits.</summary>
/// <param name="a">The first.</param>
/// <param name="b">The second.</param>
/// <returns>True when the bits are equal.</returns>
let private sameBits (a: double) (b: double) =
    BitConverter.DoubleToInt64Bits a = BitConverter.DoubleToInt64Bits b

/// <summary>Whether two bounds are bit for bit the same.</summary>
/// <param name="a">The first.</param>
/// <param name="b">The second.</param>
/// <returns>True when equal.</returns>
let private sameBounds (a: CffBounds) (b: CffBounds) =
    sameBits a.MinX b.MinX && sameBits a.MinY b.MinY && sameBits a.MaxX b.MaxX && sameBits a.MaxY b.MaxY

/// <summary>Whether the bounds interpreter answered exactly as the reference interpreter did.</summary>
/// <param name="reference">The reference answer.</param>
/// <param name="kernel">The bounds interpreter's answer.</param>
/// <returns>True when both fail or both give the same sink state, ending and budget.</returns>
let private agrees (reference: struct (Cff1Extents.SinkState * CharStringEnd * int64) voption) (kernel: CffBoundsResult) =
    match reference with
    | ValueNone -> not kernel.Valid
    | ValueSome (struct (sink, ending, budget)) ->
        let kernelBounds = { MinX = kernel.MinX; MinY = kernel.MinY; MaxX = kernel.MaxX; MaxY = kernel.MaxY }
        let sameEnding =
            match ending with
            | Ended -> not kernel.Seac
            | EndedWithSeac (adx, ady, baseCode, accentCode) ->
                kernel.Seac && sameBits adx kernel.Adx && sameBits ady kernel.Ady && baseCode = kernel.BaseCode && accentCode = kernel.AccentCode
        kernel.Valid && sink.PathOpen = kernel.PathOpen && sameBounds sink.Bounds kernelBounds && sameEnding && budget = kernel.Budget

/// <summary>Builds a CFF INDEX with two-byte offsets.</summary>
/// <param name="items">The item bytes.</param>
/// <param name="countSize">2 for CFF1, 4 for CFF2.</param>
/// <returns>The INDEX bytes.</returns>
let private indexBytes (items: byte array list) (countSize: int) =
    match items with
    | [] -> Array.zeroCreate countSize
    | _ ->
        let offsets = items |> List.scan (fun offset item -> offset + item.Length) 1
        let count = items.Length
        Array.concat
            [ (match countSize with | 2 -> [| byte (count >>> 8); byte count |] | _ -> [| 0uy; 0uy; byte (count >>> 8); byte count |])
              [| 2uy |]
              offsets |> List.map (fun offset -> [| byte (offset >>> 8); byte offset |]) |> Array.concat
              Array.concat items ]

/// <summary>A number operand in every encoding the interpreter reads.</summary>
let private number =
    Gen.frequency
        [ 8, Gen.map (fun value -> [| byte value |]) (Gen.choose (32, 246))
          2, Gen.map2 (fun opcode low -> [| byte opcode; byte low |]) (Gen.choose (247, 254)) (Gen.choose (0, 255))
          1, Gen.map2 (fun high low -> [| 28uy; byte high; byte low |]) (Gen.choose (0, 255)) (Gen.choose (0, 255))
          1, Gen.map (fun raw -> Array.append [| 255uy |] raw) (Gen.arrayOfLength 4 (Gen.choose (0, 255) |> Gen.map byte)) ]

/// <summary>Every operator, the escaped ones included, and the bytes the interpreter does not know.</summary>
let private operators =
    [| 1; 3; 4; 5; 6; 7; 8; 10; 11; 14; 15; 16; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 29; 30; 31; 0; 2; 9; 13; 17 |]
    |> Array.map (fun opcode -> [| byte opcode |])
    |> Array.append [| [| 12uy; 0uy |]; [| 12uy; 34uy |]; [| 12uy; 35uy |]; [| 12uy; 36uy |]; [| 12uy; 37uy |]; [| 12uy; 7uy |]; [| 12uy |] |]

/// <summary>An operator, weighted towards the path operators.</summary>
let private operator =
    Gen.frequency
        [ 1, Gen.elements operators
          6, Gen.elements [| [| 5uy |]; [| 6uy |]; [| 7uy |]; [| 8uy |]; [| 21uy |]; [| 22uy |]; [| 4uy |]; [| 24uy |]; [| 25uy |]; [| 26uy |]; [| 27uy |]; [| 30uy |]; [| 31uy |] |]
          2, Gen.elements [| [| 14uy |]; [| 1uy |]; [| 3uy |]; [| 18uy |]; [| 19uy |]; [| 20uy |]; [| 23uy |] |]
          1, Gen.elements [| [| 12uy; 34uy |]; [| 12uy; 35uy |]; [| 12uy; 36uy |]; [| 12uy; 37uy |] |]
          2, Gen.map2 (fun subr isGlobal -> [| byte (32 + subr); (match isGlobal with | true -> 29uy | false -> 10uy) |]) (Gen.choose (0, 4)) (Gen.elements [| true; false |]) ]

/// <summary>Operands in the counts an operator takes, with the operator: a well-formed group.</summary>
let private wellFormed =
    let counted (counts: int list) (op: byte array) = counts, op
    let operatorsWithCounts =
        [| counted [ 1; 2 ] [| 4uy |]
           counted [ 1; 2 ] [| 22uy |]
           counted [ 2; 3 ] [| 21uy |]
           counted [ 2; 4; 6; 8; 10 ] [| 5uy |]
           counted [ 1; 2; 3; 4; 5 ] [| 6uy |]
           counted [ 1; 2; 3; 4; 5 ] [| 7uy |]
           counted [ 6; 12; 18 ] [| 8uy |]
           counted [ 8; 14; 20 ] [| 24uy |]
           counted [ 8; 14 ] [| 25uy |]
           counted [ 4; 5; 8; 9; 12 ] [| 26uy |]
           counted [ 4; 5; 8; 9; 12 ] [| 27uy |]
           counted [ 4; 5; 8; 9; 12; 13; 16 ] [| 30uy |]
           counted [ 4; 5; 8; 9; 12; 13; 16 ] [| 31uy |]
           counted [ 7 ] [| 12uy; 34uy |]
           counted [ 13 ] [| 12uy; 35uy |]
           counted [ 9 ] [| 12uy; 36uy |]
           counted [ 11 ] [| 12uy; 37uy |]
           counted [ 2; 4; 6 ] [| 1uy |]
           counted [ 2; 4 ] [| 3uy |]
           counted [ 2; 4 ] [| 18uy |]
           counted [ 2 ] [| 23uy |] |]
    gen {
        let! counts, op = Gen.elements operatorsWithCounts
        let! count = Gen.elements counts
        let! operands = Gen.listOfLength count number
        return Array.concat (operands @ [ op ])
    }

/// <summary>One group: some operands and an operator, mostly well formed.</summary>
let private group =
    let random =
        gen {
            let! count = Gen.frequency [ 12, Gen.choose (0, 14); 1, Gen.choose (500, 530) ]
            let! operands = Gen.listOfLength count number
            let! op = operator
            return Array.concat (operands @ [ op ])
        }
    Gen.frequency [ 9, wellFormed; 1, random ]

/// <summary>A charstring: groups, then usually an end.</summary>
let private charString =
    gen {
        let! groups = Gen.listOf group
        let! ending = Gen.elements [| [| 14uy |]; [| 14uy |]; [| 14uy |]; [| 14uy |]; [| 11uy |]; [||] |]
        return Array.concat (groups @ [ ending ])
    }

/// <summary>A random program with its subroutines, the table around them, an optional corrupted byte and a budget.</summary>
type private Program =
    { Table: byte array
      CharLength: int
      Globals: CffIndex
      Locals: CffIndex
      Budget: int64
      Blend: StubBlend }

/// <summary>Builds a program from its parts.</summary>
/// <param name="countSize">The count field size.</param>
/// <param name="main">The charstring.</param>
/// <param name="globals">The global subroutines.</param>
/// <param name="locals">The local subroutines.</param>
/// <param name="corrupt">A byte to overwrite, as a position in the subroutine data and the new value.</param>
/// <param name="budget">The budget.</param>
/// <param name="blend">The blend context.</param>
/// <returns>The program.</returns>
let private assemble countSize (main: byte array) (globals: byte array list) (locals: byte array list) (corrupt: struct (int * byte) voption) budget blend =
    let globalBytes = indexBytes globals countSize
    let localBytes = indexBytes locals countSize
    let table = Array.concat [ main; globalBytes; localBytes ]
    match corrupt with
    | ValueSome (struct (at, value)) -> table[main.Length + at % (max 1 (table.Length - main.Length))] <- value
    | ValueNone -> ()
    let describe start (items: byte array list) =
        match items with
        | [] -> { Start = start; Count = 0u; OffSize = 0; CountSize = countSize }
        | _ -> { Start = start; Count = uint32 items.Length; OffSize = 2; CountSize = countSize }
    { Table = table
      CharLength = main.Length
      Globals = describe main.Length globals
      Locals = describe (main.Length + globalBytes.Length) locals
      Budget = budget
      Blend = blend }

/// <summary>The generator of programs.</summary>
/// <param name="countSize">The count field size.</param>
/// <returns>The generator.</returns>
let private programs countSize =
    gen {
        let! main = charString
        let! globalCount = Gen.choose (0, 3)
        let! globals = Gen.listOfLength globalCount charString
        let! localCount = Gen.choose (0, 3)
        let! locals = Gen.listOfLength localCount charString
        let! corrupt = Gen.frequency [ 6, Gen.constant ValueNone; 1, Gen.map2 (fun at value -> ValueSome (struct (at, byte value))) (Gen.choose (0, 255)) (Gen.choose (0, 255)) ]
        let! budget = Gen.frequency [ 8, Gen.constant 16777216L; 2, Gen.map int64 (Gen.choose (1, 400)); 1, Gen.map int64 (Gen.choose (0, 3)) ]
        let! blending = Gen.elements [| true; false |]
        let! regions = Gen.choose (0, 3)
        let! ivs = Gen.choose (0, 2)
        return assemble countSize main globals locals corrupt budget { Blending = blending; Regions = regions; Ivs = ivs }
    }

/// <summary>The reference interpreter on a program as CFF1.</summary>
/// <param name="program">The program.</param>
/// <returns>The answer.</returns>
let private referenceCff1 (program: Program) =
    CffCharString.interpret (Cff1Extents.ExtentsSink()) { Cff1Extents.SinkState.PathOpen = false; Bounds = Cff1Extents.empty } CffCharString.cff1Flavour 0 program.CharLength program.Globals program.Locals (ReadOnlySpan program.Table) program.Budget
    |> TailCalls.settle

/// <summary>The bounds interpreter on a program as CFF1.</summary>
/// <param name="program">The program.</param>
/// <returns>The answer.</returns>
let private kernelCff1 (program: Program) =
    CffKernel.interpret Cff1Blend.Cff1Blend false 0 (ReadOnlySpan program.Table) 0 program.CharLength (CffIndex.subrs program.Globals) (CffIndex.subrs program.Locals) program.Budget
    |> TailCalls.settle

/// <summary>The reference interpreter on a program as CFF2.</summary>
/// <param name="program">The program.</param>
/// <returns>The answer.</returns>
let private referenceCff2 (program: Program) =
    CffCharString.interpret (Cff1Extents.ExtentsSink()) { Cff1Extents.SinkState.PathOpen = false; Bounds = Cff1Extents.empty } (CffFlavour.Cff2 program.Blend) 0 program.CharLength program.Globals program.Locals (ReadOnlySpan program.Table) program.Budget
    |> TailCalls.settle

/// <summary>The bounds interpreter on a program as CFF2.</summary>
/// <param name="program">The program.</param>
/// <returns>The answer.</returns>
let private kernelCff2 (program: Program) =
    CffKernel.interpret program.Blend true program.Blend.Ivs (ReadOnlySpan program.Table) 0 program.CharLength (CffIndex.subrs program.Globals) (CffIndex.subrs program.Locals) program.Budget
    |> TailCalls.settle

/// <summary>The font files of the corpus that carry CFF or CFF2 outlines.</summary>
let private cffFaces =
    lazy (
        let upstream = RepositoryPaths.upstream ()
        let directories = [ "test/api/fonts"; "test/subset/data/fonts"; "perf/fonts" ]
        [| for directory in directories do
            for path in Directory.EnumerateFiles(Path.Combine(upstream, directory), "*.otf") |> Seq.sort do
                let bytes = File.ReadAllBytes path
                let face = Face.create (ReadOnlyMemory bytes) 0u
                match (Cff1.load face).Valid, (Cff2.load face).Valid with
                | false, false -> ()
                | cff1, cff2 -> yield struct (path, bytes, cff1, cff2) |])

/// <summary>The face and both accelerators of every corpus font, read once.</summary>
let private cffLoaded =
    lazy (
        cffFaces.Value
        |> Array.map (fun (struct (path, bytes, cff1, cff2)) ->
            let face = Face.create (ReadOnlyMemory bytes) 0u
            struct (path, bytes, face, Cff1.load face, Cff2.load face, cff1, cff2)))

/// <summary>The absolute offset of a table in the file.</summary>
/// <param name="face">The face.</param>
/// <param name="tag">The table tag.</param>
/// <returns>The offset, or 0 when the table is absent.</returns>
let private tableAt (face: Face) tag =
    match Face.tryFindTable face tag with
    | ValueSome record -> int (face.BaseOffset + record.Offset)
    | ValueNone -> 0

/// <summary>Random normalized coordinates for a font's axes, extremes included.</summary>
/// <param name="axes">The axis count.</param>
/// <returns>The generator.</returns>
let private coordinates axes =
    Gen.listOfLength axes (Gen.frequency [ 1, Gen.constant -16384; 1, Gen.constant 16384; 1, Gen.constant 0; 6, Gen.choose (-16384, 16384) ])

/// <summary>A rewritten charstring byte: a position in the glyph's charstring and the new value, or none.</summary>
let private rewrites =
    Gen.frequency [ 3, Gen.constant ValueNone; 1, Gen.map2 (fun at value -> ValueSome (struct (at, byte value))) (Gen.choose (0, 100000)) (Gen.choose (0, 255)) ]

/// <summary>A budget: the full glyph budget or a small one that runs out.</summary>
let private budgets =
    Gen.frequency [ 8, Gen.constant Cff1Extents.GlyphBudget; 2, Gen.map int64 (Gen.choose (1, 2000)) ]

/// <summary>A CFF1 case: a font, a glyph (past the glyph count too), a budget and optionally a rewritten charstring byte.</summary>
let private cff1Cases =
    gen {
        let fonts = cffLoaded.Value |> Array.indexed |> Array.filter (fun (_, struct (_, _, _, _, _, cff1, _)) -> cff1)
        let! pick = Gen.choose (0, fonts.Length - 1)
        let index, struct (_, _, _, cff, _, _, _) = fonts[pick]
        let count = int cff.GlyphCount
        let! glyph = Gen.frequency [ 12, Gen.choose (0, count - 1); 1, Gen.choose (count, count + 3) ]
        let! budget = budgets
        let! rewrite = rewrites
        return struct (index, uint32 glyph, budget, rewrite)
    }

/// <summary>Whether two optional bounds with their budgets are the same.</summary>
/// <param name="reference">The reference answer.</param>
/// <param name="fast">The bounds interpreter's answer.</param>
/// <returns>True when they agree.</returns>
let private sameBoundsResult (reference: struct (CffBounds * int64) voption) (fast: struct (CffBounds * int64) voption) =
    match reference, fast with
    | ValueNone, ValueNone -> true
    | ValueSome (struct (a, ra)), ValueSome (struct (b, rb)) -> sameBounds a b && ra = rb
    | ValueNone, ValueSome _ -> false
    | ValueSome _, ValueNone -> false

/// <summary>Whether the bounds interpreter answers as the reference does for one CFF1 case.</summary>
/// <param name="index">The font's place in the corpus.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="budget">The budget.</param>
/// <param name="rewrite">A charstring byte to overwrite, by position in the glyph's charstring.</param>
/// <returns>True when they agree.</returns>
let private cff1CaseAgrees index (glyph: uint32) budget (rewrite: struct (int * byte) voption) =
    let struct (_, bytes, face, loaded, _, _, _) = cffLoaded.Value[index]
    let cff =
        match rewrite with
        | ValueNone -> loaded
        | ValueSome (struct (at, value)) ->
            let struct (charOffset, charLength) = CffIndex.item loaded.Table.Span loaded.CharStrings glyph
            match charLength with
            | 0 -> loaded
            | _ ->
                let changed = Array.copy bytes
                changed[tableAt face Tags.Cff + charOffset + at % charLength] <- value
                Cff1.load (Face.create (ReadOnlyMemory changed) 0u)
    sameBoundsResult (Cff1Extents.bounds cff glyph budget) (Cff1Extents.boundsFast cff glyph budget)
    && Cff1Extents.glyph cff glyph = Cff1Extents.glyphFast cff glyph

/// <summary>A CFF2 case: a font, coordinates, a glyph, a budget and optionally a rewritten charstring byte.</summary>
let private cff2Cases =
    gen {
        let fonts = cffLoaded.Value |> Array.indexed |> Array.filter (fun (_, struct (_, _, _, _, _, _, cff2)) -> cff2)
        let! pick = Gen.choose (0, fonts.Length - 1)
        let index, struct (_, _, face, _, cff, _, _) = fonts[pick]
        let axes = (Font.create face).Ot.Variations.Value.Fvar.Axes.Length
        let! coords = coordinates axes
        let count = cff.NumGlyphs
        let! glyph = Gen.frequency [ 12, Gen.choose (0, count - 1); 1, Gen.choose (count, count + 3) ]
        let! budget = budgets
        let! rewrite = rewrites
        return struct (index, coords, uint32 glyph, budget, rewrite)
    }

/// <summary>Whether the bounds interpreter answers as the reference does for one CFF2 case.</summary>
/// <param name="index">The font's place in the corpus.</param>
/// <param name="coords">The normalized coordinates.</param>
/// <param name="glyph">The glyph.</param>
/// <param name="budget">The budget.</param>
/// <param name="rewrite">A charstring byte to overwrite, by position in the glyph's charstring.</param>
/// <returns>True when they agree.</returns>
let private cff2CaseAgrees index (coords: int list) (glyph: uint32) budget (rewrite: struct (int * byte) voption) =
    let struct (_, bytes, loadedFace, _, loaded, _, _) = cffLoaded.Value[index]
    let face =
        match rewrite with
        | ValueNone -> loadedFace
        | ValueSome (struct (at, value)) ->
            let struct (charOffset, charLength) = CffIndex.item loaded.Table.Span loaded.CharStrings glyph
            match charLength with
            | 0 -> loadedFace
            | _ ->
                let changed = Array.copy bytes
                changed[tableAt loadedFace Tags.Cff2 + charOffset + at % charLength] <- value
                Face.create (ReadOnlyMemory changed) 0u
    let cff = match rewrite with | ValueNone -> loaded | ValueSome _ -> Cff2.load face
    let font = Font.setVarCoordsNormalized (ImmutableArray.CreateRange coords) (Font.create face)
    let coordinates = font.Variations.Coords
    let store = font.Variations.Cff2.Value
    sameBoundsResult (Cff2Extents.boundsAt cff coordinates store glyph budget) (Cff2Extents.boundsAtFast cff coordinates store glyph budget)
    && Cff2Extents.glyph cff coordinates store glyph = Cff2Extents.glyphFast cff coordinates store glyph

/// <summary>The properties of the bounds interpreter: it answers exactly as the reference interpreter does.</summary>
[<Tests>]
let properties =
    testList "CFF bounds interpreter" [
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } "CFF1 programs: the bounds interpreter equals the reference, bounds, ending, path state and budget" <|
            Prop.forAll (Arb.fromGen (programs 2)) (fun program -> agrees (referenceCff1 program) (kernelCff1 program))
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = CaseCount } "CFF2 programs with blending: the bounds interpreter equals the reference" <|
            Prop.forAll (Arb.fromGen (programs 4)) (fun program -> agrees (referenceCff2 program) (kernelCff2 program))
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 8000 } "CFF1 corpus glyphs, rewritten charstrings and budgets: the bounds interpreter equals the reference" <|
            Prop.forAll (Arb.fromGen cff1Cases) (fun (struct (index, glyph, budget, rewrite)) -> cff1CaseAgrees index glyph budget rewrite)
        testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 8000 } "CFF2 corpus glyphs at random coordinates, rewritten charstrings and budgets: the bounds interpreter equals the reference" <|
            Prop.forAll (Arb.fromGen cff2Cases) (fun (struct (index, coords, glyph, budget, rewrite)) -> cff2CaseAgrees index coords glyph budget rewrite)
        test "every glyph of the small CFF corpus fonts agrees at the default coordinates" {
            let mutable compared = 0
            for index in 0 .. cffLoaded.Value.Length - 1 do
                let struct (path, bytes, face, _, _, cff1, cff2) = cffLoaded.Value[index]
                match bytes.Length < 400000 with
                | false -> ()
                | true ->
                    for glyph in 0u .. uint32 face.GlyphCount + 2u do
                        match cff1 with
                        | true -> Expect.isTrue (cff1CaseAgrees index glyph Cff1Extents.GlyphBudget ValueNone) $"{path} CFF1 glyph {glyph}"
                        | false -> ()
                        match cff2 with
                        | true -> Expect.isTrue (cff2CaseAgrees index [] glyph Cff1Extents.GlyphBudget ValueNone) $"{path} CFF2 glyph {glyph}"
                        | false -> ()
                        compared <- compared + 1
            Expect.isGreaterThan compared 1000 "the corpus has glyphs"
        }
        test "the generated programs are not vacuous: many succeed and many have bounds" {
            let samples = Gen.sample 3000 (programs 2) |> Seq.toArray
            let valid = samples |> Array.filter (fun program -> (kernelCff1 program).Valid)
            let withBounds = valid |> Array.filter (fun program -> let result = kernelCff1 program in result.MinX < result.MaxX)
            Expect.isGreaterThan valid.Length (samples.Length / 5) $"valid programs {valid.Length} of {samples.Length}"
            Expect.isGreaterThan withBounds.Length (samples.Length / 8) $"programs with bounds {withBounds.Length} of {samples.Length}"
        }
    ]

/// <summary>The bounds interpreter allocates nothing on a warm font.</summary>
[<Tests>]
let allocations =
    testList "CFF bounds interpreter allocation" [
        test "extents of CFF1 and CFF2 glyphs allocate no bytes" {
            let upstream = RepositoryPaths.upstream ()
            for relative in [ "test/subset/data/fonts/SourceSansPro-Regular.otf"; "test/subset/data/fonts/AdobeVFPrototype.otf" ] do
                let bytes = File.ReadAllBytes(Path.Combine(upstream, relative))
                let font = Font.create (Face.create (ReadOnlyMemory bytes) 0u)
                let font = Font.setVarCoordsNormalized (ImmutableArray.CreateRange [ 8000 ]) font |> fun varied -> match varied.Ot.Variations.Value.Fvar.Axes.Length with | 0 -> font | _ -> varied
                let allocated =
                    bytesAllocatedAtBest (fun () ->
                        for glyph in 0u .. 40u do
                            Extents.glyph font glyph |> ignore)
                Expect.equal allocated 0L $"{relative}: forty glyphs of extents allocate nothing"
        }
    ]

module Farfbuzz.Tests.Glyph.Cff1ExtentsTests

open System
open Expecto
open Farfbuzz
open Farfbuzz.Tests.Allocation
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Loads a CFF1 accelerator from a test font.</summary>
/// <param name="relative">The font path under <c>test/api/fonts</c>.</param>
/// <returns>The accelerator.</returns>
let private cffOf (relative: string) =
    Cff1.load (Face.create (read $"test/api/fonts/{relative}") 0u)

/// <summary>Reads one glyph's extents from a test font.</summary>
/// <param name="relative">The font path.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The extents.</returns>
let private extentsOf (relative: string) (glyph: uint32) : GlyphExtents =
    let font = Font.create (Face.create (read $"test/api/fonts/{relative}") 0u)
    match Extents.glyph font glyph with
    | Ok (ValueSome extents) -> extents
    | Ok ValueNone -> failtest $"no extents for glyph {glyph} of {relative}"
    | Error error -> failtest $"extents failed for glyph {glyph} of {relative}: %A{error}"

/// <summary>Writes a big-endian uint16.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let private be16 (value: uint32) = [| byte (value >>> 8); byte value |]

/// <summary>Builds a CFF INDEX with one-byte offsets.</summary>
/// <param name="items">The item bytes.</param>
/// <returns>The INDEX bytes.</returns>
let private index (items: byte array list) : byte array =
    match items with
    | [] -> be16 0u
    | _ ->
        let data = items |> Array.concat
        let offsets = items |> List.scan (fun offset item -> offset + item.Length) 1 |> List.map byte |> List.toArray
        Array.concat [ be16 (uint32 items.Length); [| 1uy |]; offsets; data ]

/// <summary>A sink that ignores every path operation.</summary>
[<Struct>]
type private NoopSink =
    interface ICharStringSink<int> with
        member _.MoveTo (state, _, _) = state
        member _.LineTo (state, _, _, _, _) = state
        member _.CurveTo (state, _, _, _, _, _, _, _, _) = state

/// <summary>The empty subroutine INDEX.</summary>
let private noSubrs = { Start = 0; Count = 0u; OffSize = 0; CountSize = 2 }

/// <summary>Runs one charstring through the interpreter with no subroutines.</summary>
/// <param name="charString">The charstring bytes.</param>
/// <returns>Whether the interpreter succeeded.</returns>
let private interpretOnly (charString: byte array) : bool =
    match CffCharString.interpret (NoopSink()) 0 CffCharString.cff1Flavour 0 charString.Length noSubrs noSubrs (ReadOnlySpan charString) 16777216L with
    | ValueSome _ -> true
    | ValueNone -> false

/// <summary>Builds a table with a charstring and a chain of subroutines that call the next, the last returning.</summary>
/// <param name="depth">The number of subroutines.</param>
/// <returns>The table, the charstring start and the global subroutine INDEX.</returns>
let private chainTable (depth: int) : struct (byte array * int * int * CffIndex) =
    let charString = [| 32uy; 29uy; 14uy |]
    let subrs =
        [ for i in 0 .. depth - 2 -> [| byte (i + 33); 29uy; 11uy |]
          if depth > 0 then yield [| 11uy |] ]
    let subrsIndex = index subrs
    let table = Array.concat [ charString; subrsIndex ]
    struct (table, 0, charString.Length, CffIndex.read (ReadOnlySpan table) charString.Length 2)

/// <summary>Runs a subroutine chain of the given depth.</summary>
/// <param name="depth">The number of subroutines.</param>
/// <returns>Whether the interpreter succeeded.</returns>
let private interpretChain (depth: int) : bool =
    let struct (table, start, length, globalSubrs) = chainTable depth
    match CffCharString.interpret (NoopSink()) 0 CffCharString.cff1Flavour start length globalSubrs noSubrs (ReadOnlySpan table) 16777216L with
    | ValueSome _ -> true
    | ValueNone -> false

/// <summary>The CFF1 extents and interpreter-limit tests, P7.S16.</summary>
[<Tests>]
let tests =
    testList "P7.S16" [
        test "test_extents_cff1" {
            let sourceSans = extentsOf "SourceSansPro-Regular.abc.otf" 1u
            Expect.equal (sourceSans.XBearing, sourceSans.YBearing, sourceSans.Width, sourceSans.Height) (52, 498, 381, -510) "glyph 1 of SourceSansPro"
            let sourceHan = extentsOf "SourceHanSans-Regular.41,3041,4C2E.otf" 3u
            Expect.equal (sourceHan.XBearing, sourceHan.YBearing, sourceHan.Width, sourceHan.Height) (34, 840, 920, -907) "glyph 3 of SourceHanSans"
        }
        test "test_extents_cff1_flex" {
            let extents = extentsOf "cff1_flex.otf" 1u
            Expect.equal (extents.XBearing, extents.YBearing, extents.Width, extents.Height) (-20, 520, 540, -540) "glyph 1 of cff1_flex"
        }
        test "test_extents_cff1_seac" {
            let agrave = extentsOf "cff1_seac.otf" 3u
            Expect.equal (agrave.XBearing, agrave.YBearing, agrave.Width, agrave.Height) (3, 861, 538, -861) "glyph 3 (Agrave)"
            let udieresis = extentsOf "cff1_seac.otf" 4u
            Expect.equal (udieresis.XBearing, udieresis.YBearing, udieresis.Width, udieresis.Height) (87, 827, 471, -839) "glyph 4 (Udieresis)"
        }
        test "eleven nested subroutine calls fail and ten pass" {
            Expect.isTrue (interpretChain 10) "ten nested calls succeed"
            Expect.isFalse (interpretChain 11) "eleven nested calls fail"
        }
        test "five hundred and fourteen arguments fail and five hundred and thirteen pass" {
            let thirteen = Array.concat [ Array.create 513 139uy; [| 14uy |] ]
            let fourteen = Array.concat [ Array.create 514 139uy; [| 14uy |] ]
            Expect.isTrue (interpretOnly thirteen) "513 arguments succeed"
            Expect.isFalse (interpretOnly fourteen) "514 arguments fail"
        }
    ]

/// <summary>The P7.S16 allocation test for the charstring interpreter.</summary>
[<Tests>]
let allocations =
    testList "P7.S16 allocation" [
        test "bounds of one glyph allocate nothing beyond the two fixed stacks" {
            let cff = cffOf "SourceSansPro-Regular.abc.otf"
            let once = bytesAllocatedAtBest (fun () -> Cff1Extents.glyph cff 1u |> ignore)
            let twice = bytesAllocatedAtBest (fun () -> Cff1Extents.glyph cff 1u |> ignore; Cff1Extents.glyph cff 1u |> ignore)
            Expect.isLessThanOrEqual once 5000L "one glyph allocates at most the two fixed stacks"
            Expect.isLessThanOrEqual (abs (twice - 2L * once)) 256L "interpreting twice allocates double the bytes of once"
        }
    ]

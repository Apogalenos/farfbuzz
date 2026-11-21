module Farfbuzz.Tests.Glyph.Cff2ExtentsTests

open System
open System.Collections.Immutable
open Expecto
open Farfbuzz
open Farfbuzz.Var
open Farfbuzz.Tests.Allocation
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Loads a CFF2 accelerator from a test font.</summary>
/// <param name="relative">The font path under <c>test/api/fonts</c>.</param>
/// <returns>The accelerator.</returns>
let private cffOf (relative: string) =
    Cff2.load (Face.create (read $"test/api/fonts/{relative}") 0u)

/// <summary>Reads one glyph's extents from a test font at its default instance.</summary>
/// <param name="relative">The font path.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The extents.</returns>
let private defaultExtentsOf (relative: string) (glyph: uint32) : GlyphExtents =
    let font = Font.create (Face.create (read $"test/api/fonts/{relative}") 0u)
    match Extents.glyph font glyph with
    | Ok (ValueSome extents) -> extents
    | Ok ValueNone -> failtest $"no extents for glyph {glyph} of {relative}"
    | Error error -> failtest $"extents failed for glyph {glyph} of {relative}: %A{error}"

/// <summary>Reads one glyph's extents from a test font at design coordinates.</summary>
/// <param name="relative">The font path.</param>
/// <param name="coords">The design coordinates.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The extents.</returns>
let private extentsAt (relative: string) (coords: float32 array) (glyph: uint32) : GlyphExtents =
    let font = Font.create (Face.create (read $"test/api/fonts/{relative}") 0u)
    let font = Font.setVarCoordsDesign (ImmutableArray.CreateRange coords) font
    match Extents.glyph font glyph with
    | Ok (ValueSome extents) -> extents
    | Ok ValueNone -> failtest $"no extents for glyph {glyph} of {relative}"
    | Error error -> failtest $"extents failed for glyph {glyph} of {relative}: %A{error}"

/// <summary>Reads one glyph's extents from a test font at a named instance.</summary>
/// <param name="relative">The font path.</param>
/// <param name="instance">The named instance.</param>
/// <param name="glyph">The glyph.</param>
/// <returns>The extents.</returns>
let private extentsAtInstance (relative: string) (instance: uint32) (glyph: uint32) : GlyphExtents =
    let font = Font.create (Face.create (read $"test/api/fonts/{relative}") 0u)
    let font = Font.setVarNamedInstance instance font
    match Extents.glyph font glyph with
    | Ok (ValueSome extents) -> extents
    | Ok ValueNone -> failtest $"no extents for glyph {glyph} of {relative}"
    | Error error -> failtest $"extents failed for glyph {glyph} of {relative}: %A{error}"

/// <summary>A sink that ignores every path operation.</summary>
[<Struct>]
type private NoopSink =
    interface ICharStringSink<int> with
        member _.MoveTo (state, _, _) = state
        member _.LineTo (state, _, _, _, _) = state
        member _.CurveTo (state, _, _, _, _, _, _, _, _) = state

/// <summary>The empty subroutine INDEX.</summary>
let private noSubrs = { Start = 0; Count = 0u; OffSize = 0; CountSize = 4 }

/// <summary>The no-blend store instance used by the interpreter-limit tests.</summary>
let private noStore = { Store = ItemVariationStore.empty; Scalars = ImmutableArray<float32>.Empty }

/// <summary>Runs one charstring through the interpreter with the CFF2 flavour and no blending.</summary>
/// <param name="charString">The charstring bytes.</param>
/// <returns>Whether the interpreter succeeded.</returns>
let private interpretCff2 (charString: byte array) : bool =
    let blend = { Blending = false; Store = noStore; Ivs = 0 }
    match CffCharString.interpret (NoopSink()) 0 (CffFlavour.Cff2 blend) 0 charString.Length noSubrs noSubrs (ReadOnlySpan charString) 16777216L with
    | ValueSome _ -> true
    | ValueNone -> false

/// <summary>The CFF2 extents and interpreter-limit tests, P7.S17.</summary>
[<Tests>]
let tests =
    testList "P7.S17" [
        test "test_extents_cff2" {
            let defaultExtents = defaultExtentsOf "AdobeVFPrototype.abc.otf" 1u
            Expect.equal (defaultExtents.XBearing, defaultExtents.YBearing, defaultExtents.Width, defaultExtents.Height) (46, 487, 455, -500) "glyph 1 at the default instance"
            let variedExtents = extentsAt "AdobeVFPrototype.abc.otf" [| 600f; 50f |] 1u
            Expect.equal (variedExtents.XBearing, variedExtents.YBearing, variedExtents.Width, variedExtents.Height) (38, 493, 480, -507) "glyph 1 at design 600 50"
        }
        test "test_extents_cff2_vsindex" {
            let glyph1 = extentsAt "AdobeVFPrototype_vsindex.otf" [| 800f; 50f |] 1u
            Expect.equal (glyph1.XBearing, glyph1.YBearing, glyph1.Width, glyph1.Height) (12, 655, 651, -655) "glyph 1"
            let glyph2 = extentsAt "AdobeVFPrototype_vsindex.otf" [| 800f; 50f |] 2u
            Expect.equal (glyph2.XBearing, glyph2.YBearing, glyph2.Width, glyph2.Height) (8, 669, 648, -669) "glyph 2"
        }
        test "test_extents_cff2_vsindex_named_instance" {
            let glyph1 = extentsAtInstance "AdobeVFPrototype_vsindex.otf" 6u 1u
            Expect.equal (glyph1.XBearing, glyph1.YBearing, glyph1.Width, glyph1.Height) (13, 652, 652, -652) "glyph 1"
            let glyph2 = extentsAtInstance "AdobeVFPrototype_vsindex.otf" 6u 2u
            Expect.equal (glyph2.XBearing, glyph2.YBearing, glyph2.Width, glyph2.Height) (6, 675, 647, -675) "glyph 2"
        }
        test "test_extents_cff2_private_blend_groups" {
            let zero = extentsAt "cff2-private-blend-groups.otf" [| 0f |] 1u
            Expect.equal (zero.XBearing, zero.YBearing, zero.Width, zero.Height) (10, 110, 40, -90) "coordinate 0"
            let half = extentsAt "cff2-private-blend-groups.otf" [| 0.5f |] 1u
            Expect.equal (half.XBearing, half.YBearing, half.Width, half.Height) (10, 110, 40, -90) "coordinate 0.5"
        }
        test "vsindex after a blend and a second vsindex both fail" {
            Expect.isFalse (interpretCff2 [| 139uy; 16uy; 139uy; 15uy; 14uy |]) "vsindex after blend"
            Expect.isFalse (interpretCff2 [| 139uy; 15uy; 139uy; 15uy; 14uy |]) "second vsindex"
        }
        test "a blend whose (k + 1) * n exceeds the stack fails" {
            Expect.isFalse (interpretCff2 [| 144uy; 16uy; 14uy |]) "five defaults exceed an empty stack"
        }
    ]

/// <summary>The P7.S17 allocation test for a blended CFF2 glyph.</summary>
[<Tests>]
let allocations =
    testList "P7.S17 allocation" [
        test "bounds of one blended glyph allocate nothing beyond the two fixed stacks" {
            let face = Face.create (read "test/api/fonts/AdobeVFPrototype.abc.otf") 0u
            let font = Font.setVarCoordsDesign (ImmutableArray.CreateRange [| 600f; 50f |]) (Font.create face)
            let cff = font.Ot.Cff2.Value
            let coords = font.Variations.Coords
            let store = font.Variations.Cff2.Value
            let once = bytesAllocatedAtBest (fun () -> Cff2Extents.glyph cff coords store 1u |> ignore)
            let twice = bytesAllocatedAtBest (fun () -> Cff2Extents.glyph cff coords store 1u |> ignore; Cff2Extents.glyph cff coords store 1u |> ignore)
            Expect.isLessThanOrEqual once 5000L "one glyph allocates at most the two fixed stacks"
            Expect.isLessThanOrEqual (abs (twice - 2L * once)) 256L "interpreting twice allocates double the bytes of once"
        }
    ]

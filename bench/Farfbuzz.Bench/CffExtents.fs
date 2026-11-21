module Farfbuzz.Bench.CffExtents

open System
open System.IO
open BenchmarkDotNet.Attributes
open Farfbuzz
open Farfbuzz.Harness
open Farfbuzz.Oracle

/// <summary>The CFF1 font this benchmark measures: NotoSansDuployan-Regular.otf, 11 529 glyphs.</summary>
let private cff1FontPath = Path.Combine(RepositoryPaths.upstream (), "perf", "fonts", "NotoSansDuployan-Regular.otf")

/// <summary>The CFF1 charstring interpreter, Farfbuzz against native <c>hb_font_get_glyph_extents</c>.</summary>
[<MemoryDiagnoser>]
type CffExtents() =
    let mutable face: Face = Unchecked.defaultof<Face>
    let mutable font: Font = Unchecked.defaultof<Font>
    let mutable nativeFont: nativeint = 0n
    let mutable glyphCount: int = 0

    /// <summary>Loads the font and builds both engines, then runs one pass to warm up.</summary>
    [<GlobalSetup>]
    member this.Setup() =
        NativeShaping.oracle.Force() |> ignore
        let bytes = File.ReadAllBytes cff1FontPath
        face <- Face.create (ReadOnlyMemory bytes) 0u
        font <- Font.create face
        glyphCount <- face.GlyphCount
        let blob = Native.hb_blob_create(bytes, uint32 bytes.Length, 0, 0n, 0n)
        let nativeFace = Native.hb_face_create(blob, 0u)
        nativeFont <- Native.hb_font_create nativeFace
        Native.hb_ot_font_set_funcs nativeFont
        Native.hb_face_destroy nativeFace
        Native.hb_blob_destroy blob
        this.Native() |> ignore
        this.Farfbuzz() |> ignore

    /// <summary>Computes every glyph's extents with native HarfBuzz.</summary>
    /// <returns>A checksum, so the work cannot be elided.</returns>
    [<Benchmark(Baseline = true)>]
    member _.Native() =
        let mutable total = 0
        let mutable extents = Unchecked.defaultof<Native.OracleGlyphExtents>
        for glyph in 0 .. glyphCount - 1 do
            Native.hb_font_get_glyph_extents(nativeFont, uint32 glyph, &extents) |> ignore
            total <- total + extents.XBearing + extents.YBearing + extents.Width + extents.Height
        total

    /// <summary>Computes every glyph's extents with Farfbuzz.</summary>
    /// <returns>A checksum, so the work cannot be elided.</returns>
    [<Benchmark>]
    member _.Farfbuzz() =
        let mutable total = 0
        for glyph in 0 .. glyphCount - 1 do
            match Extents.glyph font (uint32 glyph) with
            | Ok (ValueSome extents) -> total <- total + extents.XBearing + extents.YBearing + extents.Width + extents.Height
            | Ok ValueNone -> ()
            | Error _ -> ()
        total

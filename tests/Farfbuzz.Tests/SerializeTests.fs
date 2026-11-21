module Farfbuzz.Tests.SerializeTests

open System
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Harness

/// <summary>A glyph with only an id, a cluster and flag bits.</summary>
/// <param name="glyph">The glyph id.</param>
/// <param name="cluster">The cluster.</param>
/// <param name="mask">The flag bits.</param>
/// <returns>The glyph info.</returns>
let info glyph cluster mask = { Codepoint = glyph; Mask = mask; Cluster = cluster; Var1 = 0u; Var2 = 0u }

/// <summary>A position with advances and offsets.</summary>
/// <param name="xAdvance">Horizontal advance.</param>
/// <param name="yAdvance">Vertical advance.</param>
/// <param name="xOffset">Horizontal offset.</param>
/// <param name="yOffset">Vertical offset.</param>
/// <returns>The position.</returns>
let position xAdvance yAdvance xOffset yOffset = { XAdvance = xAdvance; YAdvance = yAdvance; XOffset = xOffset; YOffset = yOffset; Var = 0u }

/// <summary>No glyph names, as with <c>--no-glyph-names</c>.</summary>
let noNames (_: uint32) : string voption = ValueNone

/// <summary>No extents.</summary>
let noExtents (_: uint32) : GlyphExtents voption = ValueNone

/// <summary>Serialises arrays of glyphs.</summary>
/// <param name="flags">The flags.</param>
/// <param name="infos">The glyphs.</param>
/// <param name="positions">The positions.</param>
/// <returns>The text.</returns>
let serialize flags (infos: GlyphInfo array) (positions: GlyphPosition array) =
    Serialize.glyphs flags noNames noExtents (ReadOnlySpan infos) (ReadOnlySpan positions) |> TailCalls.settle

/// <summary>Deserialises with ids only.</summary>
/// <param name="text">The text.</param>
/// <returns>The glyphs and positions as arrays, or the error.</returns>
let deserialize text =
    match Serialize.parseGlyphs (fun _ -> ValueNone) (fun _ -> ValueNone) text with
    | Ok (struct (infos, positions)) -> Ok (infos.AsSpan().ToArray(), positions.AsSpan().ToArray())
    | Error error -> Error error

/// <summary>Glyphs that the default serialisation preserves: id, cluster, offsets, advances and flag bits.</summary>
let glyphRun =
    gen {
        let! count = Gen.choose (0, 12)
        let! glyphs =
            Gen.listOfLength count (
                gen {
                    let! glyph = Gen.choose (0, 65535)
                    let! cluster = Gen.choose (0, 1000)
                    let! mask = Gen.choose (0, 7)
                    let! xAdvance = Gen.choose (-3000, 3000)
                    let! yAdvance = Gen.choose (-3000, 3000)
                    let! xOffset = Gen.choose (-3000, 3000)
                    let! yOffset = Gen.choose (-3000, 3000)
                    return info (uint32 glyph) (uint32 cluster) (uint32 mask), position xAdvance yAdvance xOffset yOffset
                })
        return List.unzip glyphs |> fun (infos, positions) -> List.toArray infos, List.toArray positions
    }

/// <summary>Glyph serialisation tests, P1.Serialize.</summary>
[<Tests>]
let tests =
    testList "P1.Serialize" [
        test "ids, clusters, offsets and advances" {
            let text = serialize SerializeFlags.NoGlyphNames [| info 1u 0u 0u; info 2u 1u 0u |] [| position 0 -1000 -287 -880; position 500 0 0 0 |]
            Expect.equal text "[1=0@-287,-880+0,-1000|2=1+500]" "text"
        }
        test "names fall back to gidN" {
            let names glyph = if glyph = 1u then ValueSome "a" else ValueNone
            let text = Serialize.glyphs SerializeFlags.Default names noExtents (ReadOnlySpan [| info 1u 0u 0u; info 7u 1u 0u |]) (ReadOnlySpan [| position 10 0 0 0; position 20 0 0 0 |])
            Expect.equal text "[a=0+10|gid7=1+20]" "text"
        }
        test "no positions" {
            Expect.equal (serialize (SerializeFlags.NoGlyphNames ||| SerializeFlags.NoPositions) [| info 3u 0u 0u |] [| position 10 0 5 5 |]) "[3=0]" "text"
        }
        test "advances folded into offsets with --ned" {
            let flags = SerializeFlags.NoGlyphNames ||| SerializeFlags.NoClusters ||| SerializeFlags.NoAdvances
            Expect.equal (serialize flags [| info 1u 0u 0u; info 2u 1u 0u |] [| position 100 0 0 0; position 50 0 5 0 |]) "[1|2@105,0]" "text"
        }
        test "glyph flags in hexadecimal" {
            Expect.equal (serialize (SerializeFlags.NoGlyphNames ||| SerializeFlags.GlyphFlags ||| SerializeFlags.NoPositions) [| info 1u 0u 3u; info 2u 1u 0u |] [||]) "[1=0#3|2=1]" "text"
        }
        test "extents" {
            let extents _ = ValueSome { XBearing = 0; YBearing = 800; Width = 1000; Height = -800 }
            Expect.equal (Serialize.glyphs (SerializeFlags.NoGlyphNames ||| SerializeFlags.NoPositions ||| SerializeFlags.GlyphExtents) noNames extents (ReadOnlySpan [| info 1u 0u 0u |]) ReadOnlySpan.Empty) "[1=0<0,800,1000,-800>]" "text"
        }
        test "an empty buffer serialises to nothing" { Expect.equal (serialize SerializeFlags.Default [||] [||]) String.Empty "text" }
        test "deserialise reads every field" {
            Expect.equal (deserialize "[1=0@-287,-880+0,-1000#2|2=1+500]") (Ok ([| info 1u 0u 2u; info 2u 1u 0u |], [| position 0 -1000 -287 -880; position 500 0 0 0 |])) "glyphs"
        }
        test "deserialise accepts gidN and rejects empty input, as hb-buffer-serialize.cc:797 does" {
            Expect.equal (deserialize "[gid5=3]") (Ok ([| info 5u 3u 0u |], [| position 0 0 0 0 |])) "gidN"
            Expect.isError (deserialize String.Empty) "empty"
        }
        test "deserialise drops an item whose trailing number overflows at the end of the input" {
            Expect.equal (deserialize "[1=0|2=1@0,21474836477") (Ok ([| info 1u 0u 0u |], [| position 0 0 0 0 |])) "overflowing tail"
        }
        test "deserialise drops an item cut short by the end of the input" {
            Expect.equal (deserialize "[1=0|2=1") (Ok ([| info 1u 0u 0u |], [| position 0 0 0 0 |])) "truncated"
        }
        test "deserialise rejects an empty item and a stray byte" {
            Expect.isError (deserialize "[]") "[]"
            Expect.isError (deserialize "[1=0x]") "stray byte"
            Expect.isError (deserialize "[1=01]") "leading zero"
        }
        testProperty "serialise then deserialise gives the glyphs back" <| Prop.forAll (Arb.fromGen glyphRun) (fun (infos, positions) ->
            let flags = SerializeFlags.NoGlyphNames ||| SerializeFlags.GlyphFlags
            match deserialize (serialize flags infos positions), infos.Length with
            | Ok (readInfos, readPositions), _ -> readInfos = infos && readPositions = positions
            | Error _, 0 -> true
            | Error _, _ -> false)
    ]

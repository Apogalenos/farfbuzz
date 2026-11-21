module Farfbuzz.Tests.PostTests

open System
open System.Text
open Expecto
open FsCheck
open FsCheck.FSharp
open Farfbuzz
open Farfbuzz.Tests.SfntBuilder
open Farfbuzz.Tests.UpstreamFiles

/// <summary>Loads a post accelerator from a test font.</summary>
/// <param name="relative">The font path under <c>test/api/fonts</c>.</param>
/// <returns>The accelerator.</returns>
let private postOf (relative: string) =
    Post.load (Face.create (read $"test/api/fonts/{relative}") 0u)

/// <summary>Reads a name as a Latin-1 string.</summary>
/// <param name="name">The name bytes.</param>
/// <returns>The string.</returns>
let private nameText (name: ReadOnlyMemory<byte>) =
    Encoding.Latin1.GetString(name.Span)

/// <summary>Compares indices by an external key array.</summary>
[<Struct>]
type private KeyComparer =
    { Keys: int array }
    interface IIndexComparer with
        member this.Compare (a, b) = compare this.Keys[a] this.Keys[b]

/// <summary>Generates arrays of distinct keys.</summary>
let private introsortGen =
    gen {
        let! count = Gen.choose (0, 300)
        let! keys = Gen.listOfLength count (Gen.choose (0, 100000))
        return keys |> List.distinct |> List.toArray
    }

/// <summary>The post name index, P2.S08.</summary>
[<Tests>]
let tests =
    testList "P2.S08" [
        test "standard names cover glyph 0 and a standard index" {
            let table =
                Array.concat
                    [ be32 0x00020000u
                      Array.zeroCreate 28
                      be16 2u
                      be16 0u
                      be16 61u ]
            let face = Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Post, table ])) 0u
            let post = Post.load face
            match Post.glyphName post 0u with
            | ValueSome name -> Expect.equal (nameText name) ".notdef" "glyph 0"
            | ValueNone -> Expect.isTrue false "glyph 0 name"
            match Post.glyphName post 1u with
            | ValueSome name -> Expect.equal (nameText name) "Z" "standard index 61"
            | ValueNone -> Expect.isTrue false "glyph 1 name"
        }
        test "format 2 names come from the pool" {
            let post = postOf "adwaita.ttf"
            let expected = [ ".notdef"; ".space"; "icon0"; "icon0.0"; "icon0.1"; "icon0.2" ]
            let actual =
                [ for glyph in 0u .. 5u ->
                      match Post.glyphName post glyph with
                      | ValueSome name -> nameText name
                      | ValueNone -> String.Empty ]
            Expect.equal actual expected "names"
            Expect.equal (Post.glyphName post 100u) ValueNone "beyond last glyph"
        }
        test "names round-trip through the sorted index" {
            let post = postOf "adwaita.ttf"
            for glyph in 0u .. 5u do
                match Post.glyphName post glyph with
                | ValueSome name ->
                    match Post.glyphFromName post name.Span with
                    | ValueSome found ->
                        let roundTripped =
                            match Post.glyphName post found with
                            | ValueSome result -> nameText result
                            | ValueNone -> String.Empty
                        Expect.equal roundTripped (nameText name) "round trip"
                    | ValueNone -> Expect.isTrue false $"missing name for {glyph}"
                | ValueNone -> Expect.isTrue false $"missing glyph {glyph}"
        }
        test "an absent name misses" {
            let post = postOf "adwaita.ttf"
            Expect.equal (Post.glyphFromName post ReadOnlySpan<byte>.Empty) ValueNone "empty"
            Expect.equal (Post.glyphFromName post (ReadOnlySpan(Encoding.ASCII.GetBytes "nosuchname"))) ValueNone "absent"
        }
        testProperty "the introsort matches a reference sort" <|
            Prop.forAll (Arb.fromGen introsortGen) (fun keys ->
                let comparer = { Keys = keys }
                let actual = Sealed.sortInPlaceIntro keys.Length comparer |> Seq.map int |> Seq.toArray
                let expected = Array.init keys.Length id |> Array.sortBy (fun index -> keys[index])
                actual = expected)
        test "the introsort stays within its allocation budget" {
            let keys = Array.init 1000 (fun index -> index * 37)
            let comparer = { Keys = keys }
            let allocated = Allocation.bytesAllocatedBy (fun () -> Sealed.sortInPlaceIntro keys.Length comparer |> ignore)
            Expect.isLessThan allocated 1_000_000 "allocated bytes"
        }
    ]

/// <summary>The P2.R05 post format 1 case.</summary>
[<Tests>]
let reviewTests =
    testList "P2.R05" [
        test "format 1 uses the standard Macintosh names" {
            let post = Array.concat [ be32 0x00010000u; Array.zeroCreate 28 ]
            let face = Face.create (ReadOnlyMemory (sfnt 0x00010000u [ Tags.Post, post ])) 0u
            let accelerator = Post.load face
            Expect.equal (accelerator.Version) 0x00010000u "version"
            match Post.glyphName accelerator 0u with
            | ValueSome name -> Expect.equal (nameText name) ".notdef" "glyph 0"
            | ValueNone -> Expect.isTrue false "glyph 0 missing"
            match Post.glyphFromName accelerator (ReadOnlySpan(Encoding.ASCII.GetBytes "Z")) with
            | ValueSome glyph -> Expect.equal glyph 61u "Z"
            | ValueNone -> Expect.isTrue false "Z missing"
        }
    ]

/// <summary>The P2.R06 subrange heapsort case.</summary>
[<Tests>]
let sortingReviewTests =
    testList "P2.R06" [
        test "heapsort on a subrange respects its base index" {
            let keys = [| 9; 3; 7; 1; 3; 8; 2; 6; 0; 5; 4 |]
            let items = Array.init keys.Length uint16
            Sealed.heapSortRange items 2 6 { Keys = keys }
            let sorted = items[2..7] |> Array.map int
            let expected = [| 2..7 |] |> Array.sortBy (fun index -> keys[index]) |> Array.map int
            Expect.equal sorted expected "subrange"
            Expect.equal (items[0], items[1], items[8], items[10]) (0us, 1us, 8us, 10us) "outside untouched"
        }
    ]

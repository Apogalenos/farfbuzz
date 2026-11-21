module Farfbuzz.Gen.EmojiGen

open System
open System.IO
open Farfbuzz.Gen

/// <summary>Builds the generated Extended_Pictographic table body.</summary>
/// <param name="path">The emoji-data input.</param>
/// <returns>The generated body.</returns>
let generate (path: string) =
    let ranges =
        File.ReadAllLines path
        |> Array.choose (fun line ->
            let text = line.Trim()
            match text.Length > 0 && not (text.StartsWith "#") with
            | false -> None
            | true ->
                match text.Split(';') with
                | parts when parts.Length >= 2 && parts[1].Trim().StartsWith "Extended_Pictographic" ->
                    let ends = parts[0].Trim().Split("..")
                    let first = Int32.Parse(ends[0], Globalization.NumberStyles.HexNumber)
                    let last = if ends.Length > 1 then Int32.Parse(ends[1], Globalization.NumberStyles.HexNumber) else first
                    Some (first, last)
                | _ -> None)
    let values = Array.zeroCreate 0x110000
    for first, last in ranges do
        for codepoint in first .. last do values[codepoint] <- 1
    let table = Pack.trie values 0
    let index = table.Index |> Array.map (fun value -> string value + "us") |> String.concat "; "
    let data = table.Values |> Array.map (fun value -> string value + "uy") |> String.concat "; "
    let mask = (1 <<< table.Shift) - 1
    let blockSize = 1 <<< table.Shift
    String.Join("\n", [
        "module internal EmojiTables ="
        ""
        "    let private shift = " + string table.Shift
        "    let private index : ImmutableArray<uint16> ="
        "        Sealed.ofSpan (ReadOnlySpan [| " + index + " |])"
        "    let private values : ImmutableArray<byte> ="
        "        Sealed.ofSpan (ReadOnlySpan [| " + data + " |])"
        ""
        "    /// <summary>Returns whether a code point is Extended_Pictographic.</summary>"
        "    /// <param name=\"u\">The code point.</param>"
        "    /// <returns>True for the generated ranges.</returns>"
        "    let isExtendedPictographic (u: uint32) =" 
        "        match u < 0x110000u with"
        "        | false -> false"
        "        | true -> values[int index[int (u >>> shift)] * " + string blockSize + " + int (u &&& " + string mask + "u)] <> 0uy" ]) + "\n"

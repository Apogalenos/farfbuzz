namespace Farfbuzz.HarfBuzzSharp.Bridge

open System
open System.Runtime.InteropServices
open Farfbuzz

/// <summary>Runs for the adapters that drive the core as HarfBuzzSharp's callers do, without the facade: a text decoded into a run with the text around it kept as context, and the break pair
/// merge Avalonia's text shaper makes before it shapes.</summary>
[<AbstractClass; Sealed>]
type AdapterRuns =
    /// <summary>Decodes UTF-16 text into a run, <c>hb_buffer_add_utf16</c> of a part of a longer text: the clusters count from the start of the whole text and up to five code units around the part
    /// are its context.</summary>
    /// <param name="text">The whole text.</param>
    /// <param name="start">The first code unit of the part.</param>
    /// <param name="length">The code units of the part.</param>
    /// <returns>The run, in unicode content.</returns>
    static member Decode (text: ReadOnlySpan<char>, start: int, length: int) =
        GlyphRun.addUtf16 (MemoryMarshal.Cast<char, uint16> text) start length GlyphRun.empty |> Settled.value

    /// <summary>Merges the break at the end of a run into one zero-width non-joiner, as Avalonia does: a carriage return and line feed pair become two, the second taking the cluster of the first,
    /// and a lone break character becomes one; a run that does not end in a break is left alone.</summary>
    /// <param name="run">The decoded run.</param>
    /// <returns>The run with the break replaced.</returns>
    static member MergeBreakPair (run: GlyphRun) =
        let infos = run.Infos
        let length = infos.Length
        let isBreak (codepoint: uint32) =
            match codepoint with
            | 0x0Au | 0x0Bu | 0x0Cu | 0x0Du | 0x85u | 0x2028u | 0x2029u -> true
            | _ -> false
        match length with
        | 0 -> run
        | _ ->
            let second = infos[length - 1]
            match isBreak second.Codepoint with
            | false -> run
            | true ->
                let merged = infos.ToBuilder()
                match length > 1 && infos[length - 2].Codepoint = 0x0Du && second.Codepoint = 0x0Au with
                | true ->
                    merged[length - 2] <- { infos[length - 2] with Codepoint = 0x200Cu }
                    merged[length - 1] <- { second with Codepoint = 0x200Cu; Cluster = infos[length - 2].Cluster }
                | false -> merged[length - 1] <- { second with Codepoint = 0x200Cu }
                { run with Infos = merged.MoveToImmutable() }

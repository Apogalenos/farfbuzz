namespace Farfbuzz.HarfBuzzSharp.Bridge

open System
open System.Collections.Immutable
open System.Globalization
open Farfbuzz

module internal SerializationImpl =
    /// <summary>Adds two positions, saturating, <c>hb_saturate_add</c>.</summary>
    /// <param name="a">The first.</param>
    /// <param name="b">The second.</param>
    /// <returns>The sum, clamped to the range of an int.</returns>
    let add (a: int) (b: int) =
        int (max (int64 Int32.MinValue) (min (int64 Int32.MaxValue) (int64 a + int64 b)))

    /// <summary>The sum of the advances of the glyphs before the range, kept only when the serialisation leaves advances out.</summary>
    /// <param name="positions">The positions.</param>
    /// <param name="stop">The first glyph not summed.</param>
    /// <param name="index">The next glyph.</param>
    /// <param name="x">The horizontal sum so far.</param>
    /// <param name="y">The vertical sum so far.</param>
    /// <returns>The sums.</returns>
    [<TailCall>]
    let rec advanceBefore (positions: ImmutableArray<GlyphPosition>) (stop: int) (index: int) (x: int) (y: int) =
        match index >= stop with
        | true -> struct (x, y)
        | false -> advanceBefore positions stop (index + 1) (add x positions[index].XAdvance) (add y positions[index].YAdvance)

    /// <summary>Escapes a glyph name for JSON, <c>"</c> and <c>\</c> only.</summary>
    /// <param name="name">The name.</param>
    /// <returns>The escaped name.</returns>
    let escape (name: string) =
        name.Replace("\\", "\\\\").Replace("\"", "\\\"")

    /// <summary>One glyph of the JSON serialisation, <c>_hb_buffer_serialize_glyphs_json</c> (hb-buffer-serialize.cc:102).</summary>
    let item (flags: uint32) (font: Font) (infos: ImmutableArray<GlyphInfo>) (positions: ImmutableArray<GlyphPosition>) (stop: int) (index: int) (x: int) (y: int) =
        let info = infos[index]
        let inv (value: uint32) = value.ToString CultureInfo.InvariantCulture
        let invInt (value: int) = value.ToString CultureInfo.InvariantCulture
        let opener = if index > 0 then "," else "["
        let glyph =
            match flags &&& 4u with
            | 0u -> "\"" + escape (Font.glyphToString font info.Codepoint) + "\""
            | _ -> inv info.Codepoint
        let cluster = match flags &&& 1u with | 0u -> ",\"cl\":" + inv info.Cluster | _ -> String.Empty
        let placed =
            match flags &&& 2u, positions.IsDefaultOrEmpty with
            | 0u, false ->
                let position = positions[index]
                let offsets = $",\"dx\":{invInt (add x position.XOffset)},\"dy\":{invInt (add y position.YOffset)}"
                match flags &&& 32u with
                | 0u -> offsets + $",\"ax\":{invInt position.XAdvance},\"ay\":{invInt position.YAdvance}"
                | _ -> offsets
            | _ -> String.Empty
        let flagged =
            match flags &&& 16u, info.Mask &&& 7u with
            | 0u, _ -> String.Empty
            | _, 0u -> String.Empty
            | _, defined -> ",\"fl\":" + inv defined
        let extents =
            match flags &&& 8u with
            | 0u -> String.Empty
            | _ ->
                match Extents.glyph font info.Codepoint with
                | Ok (ValueSome e) -> $",\"xb\":{invInt e.XBearing},\"yb\":{invInt e.YBearing},\"w\":{invInt e.Width},\"h\":{invInt e.Height}"
                | Ok ValueNone | Error _ -> String.Empty
        opener + "{\"g\":" + glyph + cluster + placed + flagged + extents + "}" + (if index = stop - 1 then "]" else String.Empty)

    /// <summary>The whole JSON text of a range, the horizontal and vertical advance of earlier glyphs being added to the offsets when advances are left out.</summary>
    let jsonRange (flags: uint32) (font: Font) (infos: ImmutableArray<GlyphInfo>) (positions: ImmutableArray<GlyphPosition>) (start: int) (stop: int) =
        let struct (x, y) =
            match flags &&& 32u, flags &&& 2u, positions.IsDefaultOrEmpty with
            | 0u, _, _ | _, _, true -> struct (0, 0)
            | _, 2u, _ -> struct (0, 0)
            | _, _, false -> advanceBefore positions start 0 0 0
        let rec build (index: int) (x: int) (y: int) (acc: string list) =
            match index >= stop with
            | true -> List.rev acc
            | false ->
                let nextX, nextY = match flags &&& 32u, flags &&& 2u, positions.IsDefaultOrEmpty with | 0u, _, _ | _, _, true -> x, y | _, 2u, _ -> x, y | _, _, false -> add x positions[index].XAdvance, add y positions[index].YAdvance
                build (index + 1) nextX nextY (item flags font infos positions stop index x y :: acc)
        String.concat String.Empty (build start x y [])

    /// <summary>One token of the JSON grammar: its end offset, or none when it does not match.</summary>
    [<Struct>]
    type Cursor = { Text: string; At: int }

    let isSpace (c: char) = c = ' ' || c = '\t' || c = '\n' || c = '\011' || c = '\012' || c = '\r'

    [<TailCall>]
    let rec skip (text: string) (at: int) =
        match at < text.Length && isSpace text[at] with
        | true -> skip text (at + 1)
        | false -> at

    [<TailCall>]
    let rec digits (text: string) (at: int) =
        match at < text.Length && Char.IsAsciiDigit text[at] with
        | true -> digits text (at + 1)
        | false -> at

    /// <summary>Reads <c>unum</c> or <c>num</c> from an offset.</summary>
    let number (text: string) (at: int) (signed: bool) =
        let start = match signed && at < text.Length && text[at] = '-' with | true -> at + 1 | false -> at
        let stop = digits text start
        match stop > start && not (text[start] = '0' && stop > start + 1) with
        | true -> ValueSome (struct (Int64.Parse(text.Substring(at, stop - at), CultureInfo.InvariantCulture), stop))
        | false -> ValueNone

    /// <summary>One glyph object, <c>item</c> of the grammar, from the offset of its brace.</summary>
    /// <returns>The glyph, its position, the offset after it; incomplete when the text ends inside it; invalid when it does not parse.</returns>
    let rec element (font: Font) (text: string) (at: int) (info: GlyphInfo) (position: GlyphPosition) =
        let quoted (key: string) = at + key.Length + 2 <= text.Length && String.CompareOrdinal(text, at, "\"" + key + "\"", 0, key.Length + 2) = 0
        let afterKey (key: string) =
            let colon = skip text (at + key.Length + 2)
            match colon < text.Length && text[colon] = ':' with
            | true -> ValueSome (skip text (colon + 1))
            | false -> ValueNone
        let next (info: GlyphInfo) (position: GlyphPosition) (stop: int) =
            let after = skip text stop
            match after < text.Length with
            | false -> Ok ValueNone
            | true ->
                match text[after] with
                | ',' -> element font text (skip text (after + 1)) info position
                | '}' ->
                    let tail = skip text (after + 1)
                    match tail < text.Length with
                    | false -> Ok ValueNone
                    | true when text[tail] = ',' || text[tail] = ']' -> Ok (ValueSome (struct (info, position, skip text (tail + 1))))
                    | true -> Error ()
                | _ -> Error ()
        let key =
            [ "g"; "u"; "cl"; "dx"; "dy"; "ax"; "ay"; "fl"; "xb"; "yb"; "w"; "h" ] |> List.tryFind (fun candidate -> quoted candidate && ValueOption.isSome (afterKey candidate))
        match key with
        | None -> (if at >= text.Length then Ok ValueNone else Error ())
        | Some name ->
            let start = ValueOption.get (afterKey name)
            match name with
            | "g" when start < text.Length && text[start] = '"' ->
                let rec closing (i: int) = match i >= text.Length with | true -> -1 | false -> (match text[i] with | '\\' -> closing (i + 2) | '"' -> i | _ -> closing (i + 1))
                let close = closing (start + 1)
                match close with
                | -1 -> Ok ValueNone
                | _ ->
                    match Font.glyphFromString font (text.Substring(start + 1, close - start - 1)) with
                    | ValueSome glyph -> next { info with Codepoint = glyph } position (close + 1)
                    | ValueNone -> Error ()
            | "g" | "u" | "cl" | "fl" ->
                match number text start false with
                | ValueNone -> (if start >= text.Length then Ok ValueNone else Error ())
                | ValueSome (struct (value, _)) when value > int64 UInt32.MaxValue -> Error ()
                | ValueSome (struct (value, stop)) ->
                    match name with
                    | "cl" -> next { info with Cluster = uint32 value } position stop
                    | "fl" -> next { info with Mask = uint32 value } position stop
                    | _ -> next { info with Codepoint = uint32 value } position stop
            | _ ->
                match number text start true with
                | ValueNone -> (if start >= text.Length then Ok ValueNone else Error ())
                | ValueSome (struct (value, _)) when value > int64 Int32.MaxValue || value < int64 Int32.MinValue -> Error ()
                | ValueSome (struct (value, stop)) ->
                    match name with
                    | "dx" -> next info { position with XOffset = int value } stop
                    | "dy" -> next info { position with YOffset = int value } stop
                    | "ax" -> next info { position with XAdvance = int value } stop
                    | "ay" -> next info { position with YAdvance = int value } stop
                    | _ -> next info position stop

    /// <summary>The glyphs of a JSON text, <c>_hb_buffer_deserialize_json</c>: items that end in <c>,</c> or <c>]</c> are added, an item cut short by the end of the text is dropped.</summary>
    let rec items (font: Font) (text: string) (at: int) (found: (GlyphInfo * GlyphPosition) list) =
        match at >= text.Length with
        | true -> Ok (List.rev found)
        | false when text[at] <> '{' -> Error ()
        | false ->
            match element font text (skip text (at + 1)) Unchecked.defaultof<GlyphInfo> Unchecked.defaultof<GlyphPosition> with
            | Error () -> Error ()
            | Ok ValueNone -> Ok (List.rev found)
            | Ok (ValueSome (struct (info, position, stop))) -> items font text stop ((info, position) :: found)

/// <summary>Serialisation of glyphs for the HarfBuzzSharp shell: <c>hb-buffer-serialize.cc</c> and the deserialisers, over the core's text format.</summary>
[<AbstractClass; Sealed>]
type Serialization =
    /// <summary><c>hb_buffer_serialize_glyphs</c> over a range, the whole text in one piece (format 1 text, 2 JSON; flags as <c>hb_buffer_serialize_flags_t</c>).
    /// A range that starts after the first glyph begins with the separator, as upstream's chunks do; advances left out of a text range
    /// do not add the advances of the glyphs before it.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="positions">The positions, empty when the buffer has none.</param>
    /// <param name="start">The first glyph.</param>
    /// <param name="stop">The glyph after the last.</param>
    /// <param name="font">The core font.</param>
    /// <param name="format">The format value.</param>
    /// <param name="flags">The flag values.</param>
    /// <returns>The text.</returns>
    static member Serialize (infos: ImmutableArray<GlyphInfo>, positions: ImmutableArray<GlyphPosition>, start: int, stop: int, font: Font, format: uint32, flags: uint32) =
        match format with
        | 0x4A534F4Eu -> SerializationImpl.jsonRange flags font infos positions start stop
        | _ ->
            let range = infos.AsSpan().Slice(start, stop - start).ToArray()
            let moved = match positions.IsDefaultOrEmpty with | true -> [||] | false -> positions.AsSpan().Slice(start, stop - start).ToArray()
            let text = Serialize.glyphs (LanguagePrimitives.EnumOfValue<uint32, SerializeFlags> flags) (Font.glyphName font) (fun glyph -> match Extents.glyph font glyph with | Ok found -> found | Error _ -> ValueNone) (ReadOnlySpan range) (ReadOnlySpan moved)
            match start > 0 && text.StartsWith "[" with
            | true -> "|" + text.Substring 1
            | false -> text

    /// <summary><c>hb_buffer_deserialize_glyphs</c> (hb-buffer-serialize.cc:773-921).</summary>
    /// <param name="text">The text.</param>
    /// <param name="font">The core font, whose names resolve the glyphs.</param>
    /// <param name="format">The format value, text or JSON.</param>
    /// <returns>Whether the text parsed, the glyphs and their positions.</returns>
    static member Deserialize (text: string, font: Font, format: uint32) =
        match format with
        | 0x4A534F4Eu ->
            let start = SerializationImpl.skip text 0
            let first = match start < text.Length && text[start] = '[' with | true -> SerializationImpl.skip text (start + 1) | false -> start
            match SerializationImpl.items font text first [] with
            | Ok found -> struct (true, Sealed.ofList (found |> List.map fst), Sealed.ofList (found |> List.map snd))
            | Error () -> struct (false, ImmutableArray<GlyphInfo>.Empty, ImmutableArray<GlyphPosition>.Empty)
        | _ ->
            match Serialize.parseGlyphs (Font.glyphFromName font) (Font.nominalGlyph font) text with
            | Ok (struct (infos, positions)) -> struct (true, infos, positions)
            | Error _ -> struct (false, ImmutableArray<GlyphInfo>.Empty, ImmutableArray<GlyphPosition>.Empty)

    /// <summary><c>hb_buffer_normalize_glyphs</c> (hb-buffer.cc, <c>normalize_glyphs_cluster</c>): inside each cluster the advances move to one glyph, the offsets absorb the rest, and the glyphs but that one are stably sorted by glyph id.</summary>
    /// <param name="infos">The glyph infos.</param>
    /// <param name="positions">The positions.</param>
    /// <param name="backward">Whether the direction is backward.</param>
    /// <returns>The normalised infos and positions.</returns>
    static member Normalize (infos: ImmutableArray<GlyphInfo>, positions: ImmutableArray<GlyphPosition>, backward: bool) =
        let add = SerializationImpl.add
        let sub (a: int) (b: int) = int (max (int64 Int32.MinValue) (min (int64 Int32.MaxValue) (int64 a - int64 b)))
        let count = infos.Length
        let rec clusterEnd (start: int) (index: int) = match index < count && infos[index].Cluster = infos[start].Cluster with | true -> clusterEnd start (index + 1) | false -> index
        let rec clusters (start: int) (acc: (GlyphInfo * GlyphPosition) list) =
            match start >= count with
            | true -> List.rev acc
            | false ->
                let stop = clusterEnd start (start + 1)
                let members = [ for i in start .. stop - 1 -> infos[i], positions[i] ]
                let totalX = members |> List.fold (fun sum (_, p) -> add sum p.XAdvance) 0
                let totalY = members |> List.fold (fun sum (_, p) -> add sum p.YAdvance) 0
                let absorbed =
                    members
                    |> List.fold (fun (done', x, y) (info, p) -> ((info, { p with XOffset = add p.XOffset x; YOffset = add p.YOffset y; XAdvance = 0; YAdvance = 0 }) :: done', add x p.XAdvance, add y p.YAdvance)) ([], 0, 0)
                    |> fun (list, _, _) -> List.rev list
                let sortRange (items: (GlyphInfo * GlyphPosition) list) = items |> List.sortWith (fun (a, _) (b, _) -> compare a.Codepoint b.Codepoint)
                let ordered =
                    match backward with
                    | true ->
                        let lead = List.take (absorbed.Length - 1) absorbed
                        let last = List.last absorbed
                        sortRange lead @ [ (fst last, { snd last with XAdvance = totalX; YAdvance = totalY }) ]
                    | false ->
                        let first = List.head absorbed
                        let rest = absorbed |> List.tail |> List.map (fun (info, p) -> info, { p with XOffset = sub p.XOffset totalX; YOffset = sub p.YOffset totalY })
                        (fst first, { snd first with XAdvance = add (snd first).XAdvance totalX; YAdvance = add (snd first).YAdvance totalY }) :: sortRange rest
                clusters stop (List.rev ordered @ acc)
        let result = clusters 0 []
        struct (Sealed.ofList (result |> List.map fst), Sealed.ofList (result |> List.map snd))

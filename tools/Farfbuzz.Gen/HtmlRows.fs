#nowarn "3261"

module Farfbuzz.Gen.HtmlRows

open System
open System.Net

/// <summary>Appends text to the last cell of the current row.</summary>
/// <param name="cells">The current row's cells.</param>
/// <param name="text">The text to append.</param>
let private appendCell (cells: ResizeArray<string>) (text: string) =
    let last = cells.Count - 1
    cells[last] <- cells[last] + text

/// <summary>Whether a character or entity reference follows the ampersand.</summary>
/// <param name="body">The text between the ampersand and the semicolon.</param>
/// <returns>True for a numeric or named reference.</returns>
let private isReference (body: string) =
    match body.Length with
    | 0 -> false
    | _ ->
        match body[0] with
        | '#' ->
            body.Length > 1
            && (match body[1] with
                | 'x' | 'X' -> body.Length > 2 && body.Substring(2) |> Seq.forall Char.IsAsciiHexDigit
                | _ -> body.Substring(1) |> Seq.forall Char.IsAsciiDigit)
        | c when Char.IsAsciiLetterOrDigit c -> body |> Seq.forall Char.IsAsciiLetterOrDigit
        | _ -> false

/// <summary>Decodes the reference at the given ampersand, if any.</summary>
/// <param name="html">The page text.</param>
/// <param name="index">The ampersand index.</param>
/// <returns>The decoded text and the index after the reference, or none.</returns>
let private tryDecodeReference (html: string) (index: int) : (string * int) voption =
    let semi = html.IndexOf(';', index + 1)
    match semi with
    | -1 -> ValueNone
    | semi when semi - index > 24 -> ValueNone
    | semi ->
        let body = html.Substring(index + 1, semi - index - 1)
        match isReference body with
        | true ->
            let reference = html.Substring(index, semi - index + 1)
            ValueSome(WebUtility.HtmlDecode reference, semi + 1)
        | false -> ValueNone

/// <summary>Finds the greater-than that ends a start tag, respecting quoted attribute values.</summary>
/// <param name="html">The page text.</param>
/// <param name="openIndex">The index of the less-than.</param>
/// <returns>The index of the greater-than, or -1 when unterminated.</returns>
let private findTagEnd (html: string) (openIndex: int) : int =
    let mutable i = openIndex + 1
    let mutable quote = '\u0000'
    let mutable found = -1
    while i < html.Length && found < 0 do
        let c = html[i]
        match quote with
        | '\u0000' ->
            if c = '"' || c = '\'' then quote <- c
            elif c = '>' then found <- i
        | currentQuote ->
            if c = currentQuote then quote <- '\u0000'
        i <- i + 1
    found

/// <summary>Splits a start tag into its lowercased name and its decoded attribute pairs.</summary>
/// <param name="raw">The start tag text, including the angle brackets.</param>
/// <returns>The tag name and the attributes.</returns>
let private startTagParts (raw: string) : struct (string * (string * string) list) =
    let inner = raw.Substring(1, raw.Length - 2).Trim()
    let mutable i = 0
    let nameStart = i
    while i < inner.Length && not (Char.IsWhiteSpace inner[i]) && inner[i] <> '/' do
        i <- i + 1
    let name = inner.Substring(nameStart, i - nameStart).ToLowerInvariant()
    let attrs = ResizeArray<string * string>()
    let mutable scanning = true
    while scanning do
        while i < inner.Length && Char.IsWhiteSpace inner[i] do
            i <- i + 1
        if i >= inner.Length then
            scanning <- false
        elif inner[i] = '/' then
            scanning <- false
        else
            let attrStart = i
            while i < inner.Length && not (Char.IsWhiteSpace inner[i]) && inner[i] <> '=' && inner[i] <> '/' do
                i <- i + 1
            let attrName = inner.Substring(attrStart, i - attrStart).ToLowerInvariant()
            while i < inner.Length && Char.IsWhiteSpace inner[i] do
                i <- i + 1
            let value =
                match i < inner.Length && inner[i] = '=' with
                | false -> ""
                | true ->
                    i <- i + 1
                    while i < inner.Length && Char.IsWhiteSpace inner[i] do
                        i <- i + 1
                    match i < inner.Length && (inner[i] = '"' || inner[i] = '\'') with
                    | true ->
                        let quote = inner[i]
                        i <- i + 1
                        let valueStart = i
                        while i < inner.Length && inner[i] <> quote do
                            i <- i + 1
                        let text = inner.Substring(valueStart, i - valueStart)
                        if i < inner.Length then i <- i + 1
                        WebUtility.HtmlDecode text
                    | false ->
                        let valueStart = i
                        while i < inner.Length && not (Char.IsWhiteSpace inner[i]) && inner[i] <> '/' do
                            i <- i + 1
                        WebUtility.HtmlDecode(inner.Substring(valueStart, i - valueStart))
            attrs.Add(attrName, value)
    struct (name, List.ofSeq attrs)

/// <summary>Tokenizes the language-system-tag page into the rows the generator uses.</summary>
/// <param name="html">The page text.</param>
/// <returns>The raw updated_at meta start tag and each table row's cell texts.</returns>
let parse (html: string) : struct (string voption * string list list) =
    let mutable header = ValueNone
    let mutable td = false
    let mutable disengaged = false
    let currentTr = ResizeArray<string>()
    let rows = ResizeArray<string list>()
    let mutable i = 0
    while i < html.Length do
        match html[i] with
        | '&' ->
            match tryDecodeReference html i with
            | ValueSome(decoded, next) ->
                if td && not disengaged then appendCell currentTr decoded
                i <- next
            | ValueNone ->
                if td && not disengaged then appendCell currentTr "&"
                i <- i + 1
        | '<' ->
            match i + 3 < html.Length && html.Substring(i, 4) = "<!--" with
            | true ->
                let close = html.IndexOf("-->", i + 4, StringComparison.Ordinal)
                i <- if close < 0 then html.Length else close + 3
            | false ->
                match i + 1 < html.Length && html[i + 1] = '!' with
                | true ->
                    let close = html.IndexOf('>', i + 2)
                    i <- if close < 0 then html.Length else close + 1
                | false ->
                    match i + 1 < html.Length && html[i + 1] = '/' with
                    | true ->
                        let close = html.IndexOf('>', i + 2)
                        let raw = html.Substring(i, (if close < 0 then html.Length - i else close - i + 1))
                        let name = raw.Substring(2).TrimEnd([| '>'; ' '; '\t'; '\r'; '\n' |]).ToLowerInvariant()
                        match name with
                        | "td" ->
                            td <- false
                            disengaged <- false
                        | "tr" ->
                            if currentTr.Count > 0 then rows.Add(List.ofSeq currentTr)
                        | _ -> ()
                        i <- if close < 0 then html.Length else close + 1
                    | false ->
                        let close = findTagEnd html i
                        let raw = html.Substring(i, (if close < 0 then html.Length - i else close - i + 1))
                        let struct (name, attrs) = startTagParts raw
                        match name with
                        | "script" | "style" ->
                            let found = html.IndexOf("</" + name, i + 1, StringComparison.OrdinalIgnoreCase)
                            match found with
                            | -1 -> i <- html.Length
                            | found ->
                                let close = html.IndexOf('>', found)
                                i <- if close < 0 then html.Length else close + 1
                        | "a" ->
                            if currentTr.Count > 0 && not disengaged then
                                currentTr[currentTr.Count - 1] <- ""
                                disengaged <- true
                            i <- if close < 0 then html.Length else close + 1
                        | "br" ->
                            disengaged <- true
                            i <- if close < 0 then html.Length else close + 1
                        | "meta" ->
                            if attrs |> List.exists (fun (attrName, value) -> attrName = "name" && value = "updated_at") then
                                header <- ValueSome raw
                            i <- if close < 0 then html.Length else close + 1
                        | "td" ->
                            td <- true
                            currentTr.Add ""
                            i <- if close < 0 then html.Length else close + 1
                        | "tr" ->
                            disengaged <- false
                            currentTr.Clear()
                            i <- if close < 0 then html.Length else close + 1
                        | _ -> i <- if close < 0 then html.Length else close + 1
        | _ ->
            let amp = html.IndexOf('&', i)
            let lt = html.IndexOf('<', i)
            let next =
                match amp, lt with
                | -1, -1 -> html.Length
                | -1, l -> l
                | a, -1 -> a
                | a, l -> min a l
            if next > i && td && not disengaged then appendCell currentTr (html.Substring(i, next - i))
            i <- next
    struct (header, List.ofSeq rows)

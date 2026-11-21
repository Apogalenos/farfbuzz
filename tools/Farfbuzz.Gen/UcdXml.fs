module Farfbuzz.Gen.UcdXml

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Xml
open System.Xml.Linq

/// <summary>One code point's Unicode data.</summary>
type CodePointRecord =
    { Gc: string
      Ccc: int
      Bmg: uint32 voption
      Sc: string
      Dm: uint32 list
      Dt: string
      CompEx: bool }

/// <summary>Loads the grouped UCD XML from a zip.</summary>
/// <param name="zipPath">The zip path.</param>
/// <returns>The description and all 0x110000 records, or an error.</returns>
let load (zipPath: string) : Result<struct (string * CodePointRecord array), string> =
    try
        use archive = ZipFile.OpenRead zipPath
        let entry = archive.Entries |> Seq.find (fun entry -> entry.FullName.EndsWith(".xml", StringComparison.OrdinalIgnoreCase))
        use stream = entry.Open()
        use reader = XmlReader.Create(stream)
        let table = Array.create 0x110000 { Gc = "Cn"; Ccc = 0; Bmg = ValueNone; Sc = "Zzzz"; Dm = []; Dt = "none"; CompEx = false }
        let strings = Dictionary<string, string>()
        let intern (text: string) = match strings.TryGetValue text with | true, value -> value | false, _ -> strings.Add(text, text); text
        let mutable description = String.Empty
        while reader.Read() do
            match reader.NodeType with
            | XmlNodeType.Element when reader.LocalName = "description" -> description <- reader.ReadElementContentAsString()
            | XmlNodeType.Element when reader.LocalName = "group" ->
                use subtree = reader.ReadSubtree()
                let group = XElement.Load subtree
                let groupValue name = group.Attribute(XName.Get name) |> Option.ofObj |> Option.map (fun attribute -> attribute.Value)
                let valueOf (element: XElement) name = element.Attribute(XName.Get name) |> Option.ofObj |> Option.map (fun attribute -> attribute.Value) |> Option.orElse (groupValue name)
                let recordOf (element: XElement) =
                    let gc = valueOf element "gc" |> Option.defaultValue "Cn" |> intern
                    let ccc = valueOf element "ccc" |> Option.defaultValue "0" |> Int32.Parse
                    let bmg = valueOf element "bmg" |> Option.filter (fun text -> text.Length > 0) |> Option.map (fun text -> UInt32.Parse(text, Globalization.NumberStyles.HexNumber))
                    let sc = valueOf element "sc" |> Option.defaultValue "Zzzz" |> intern
                    let dt = valueOf element "dt" |> Option.defaultValue "none" |> intern
                    let dm = valueOf element "dm" |> Option.defaultValue "#" |> fun text -> if text = "#" then [] else text.Split(' ') |> Array.toList |> List.map (fun value -> UInt32.Parse(value, Globalization.NumberStyles.HexNumber))
                    let compEx = valueOf element "Comp_Ex" |> Option.exists ((=) "Y")
                    { Gc = gc; Ccc = ccc; Bmg = Option.toValueOption bmg; Sc = sc; Dm = dm; Dt = dt; CompEx = compEx }
                for child in group.Elements() do
                    let record = recordOf child
                    let cp = valueOf child "cp" |> Option.map (fun text -> Int32.Parse(text, Globalization.NumberStyles.HexNumber))
                    match cp with
                    | Some value when value >= 0 && value < table.Length -> table[value] <- record
                    | Some _ -> ()
                    | None ->
                        let first = valueOf child "first-cp" |> Option.map (fun text -> Int32.Parse(text, Globalization.NumberStyles.HexNumber))
                        let last = valueOf child "last-cp" |> Option.map (fun text -> Int32.Parse(text, Globalization.NumberStyles.HexNumber))
                        match first, last with
                        | Some first, Some last ->
                            for value in max 0 first .. min (table.Length - 1) last do table[value] <- record
                        | _ -> ()
            | _ -> ()
        Ok (struct (description, table))
    with error -> Error error.Message

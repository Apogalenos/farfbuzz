module Farfbuzz.Samples.Avalonia11.Browser.Program

open System.Runtime.Versioning
open System.Threading.Tasks
open Avalonia
open Avalonia.Browser
open Farfbuzz.Samples.Avalonia11

/// <summary>Starts the sample application in the browser's <c>out</c> element.</summary>
/// <returns>The start-up task.</returns>
[<SupportedOSPlatform("browser")>]
let start () : Task =
    AppBuilder.Configure<App>().StartBrowserAppAsync "out"

[<EntryPoint>]
let main _ =
    start () |> ignore
    0

module Farfbuzz.HarfBuzzSharp.Tests.OracleSession

open Farfbuzz.Oracle

/// <summary>Loads the HarfBuzz 15.1.0 oracle once for this process; an absent build is a failure, never a skip.</summary>
let private loaded =
    lazy
        (match OracleLibrary.ensureLoaded () with
         | Ok oracle -> oracle
         | Error reason -> failwith reason)

/// <summary>Makes sure the oracle's native library is bound before a native call.</summary>
let ensure () =
    loaded.Force() |> ignore

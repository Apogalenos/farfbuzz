// PerfReport: turns BenchmarkDotNet full JSON (and later the P9.S05/S06 runner JSON) into the phase-9 budget report.
// Usage: dotnet fsi tools/perf/PerfReport.fsx [--enforce] [--note <text>] <results-dir> <runner-json-dir>
// The results dir holds the `*-report-full.json` files BenchmarkDotNet writes with `--exporters fulljson`; the runner
// dir holds the JSON the P9.S05/S06 runner writes (none until those slices land, so it is accepted and reported as
// empty). Writes `docs/research/perf-report.md`. With `--enforce`, exits 1 when any cell is a GATE MISS or any benchmark has no statistics (failed to run).

open System
open System.IO
open System.Text.Json

/// <summary>The repository root, two levels above this script.</summary>
let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

/// <summary>The budget table of the phase handoff, machine-readable.</summary>
let budgetsPath = Path.Combine(root, "docs", "research", "perf-budgets.tsv")

/// <summary>The glyph counts `--tally` (P9.S01) appends as the "Upstream pairs" table.</summary>
let baselinePath = Path.Combine(root, "docs", "research", "perf-baseline.md")

/// <summary>The report this script writes.</summary>
let reportPath = Path.Combine(root, "docs", "research", "perf-report.md")

/// <summary>One pair's budget row: time-ratio gates and targets against native and HarfBuzzSharp, and the bytes-per-glyph gate and target.</summary>
type Budget = { NativeGate: float; NativeTarget: float; HbsGate: float; HbsTarget: float; BytesGate: float; BytesTarget: float }

/// <summary>One measured benchmark cell: the method, the pair, the job and the numbers BenchmarkDotNet reports.</summary>
type Measurement = { Pair: string; Method: string; Job: string; MeanNs: float; Bytes: float voption }

/// <summary>One report row: a pair's measured means, the computed ratios and the bytes per glyph.</summary>
type Row = { Pair: string; Runtime: string; FarfbuzzNs: float; NativeNs: float; HbsNs: float voption; BytesPerGlyph: float voption }

/// <summary>One runner result: the pair, the glyphs per pass, the median and minimum pass in milliseconds, the bytes per pass and the output hash.</summary>
type RunnerResult = { Pair: string; Glyphs: int; MedianMs: float; MinMs: float; BytesPerPass: float; Consistent: bool; Hash: string }

/// <summary>One runner run: the implementation, the runtime label, the architecture, the framework and the results.</summary>
type RunnerRun = { Impl: string; Runtime: string; Architecture: string; Framework: string; Vector128: bool; Processors: int; Results: RunnerResult list }

/// <summary>A cell verdict: within target, within the gate, over the gate, or not measured.</summary>
type Verdict = Pass | Gate | Miss | Unmeasured

/// <summary>Strips the file extensions of a `font/text` pair name, matching the keys of the budget table.</summary>
/// <param name="pair">The pair as the benchmark renders it, for example <c>Roboto-Regular.ttf/en-thelittleprince.txt</c>.</param>
/// <returns>The key, for example <c>Roboto-Regular/en-thelittleprince</c>.</returns>
let normalizePair (pair: string) =
    let parts = pair.Split('/')
    match parts.Length = 2 with
    | false -> pair
    | true ->
        let font = Path.GetFileNameWithoutExtension parts[0]
        let text = Path.GetFileNameWithoutExtension parts[1]
        $"{font}/{text}"

/// <summary>Reads the machine-readable budget table.</summary>
/// <returns>The budget by pair key.</returns>
let readBudgets () =
    match File.Exists budgetsPath with
    | false -> Map.empty
    | true ->
        File.ReadAllLines budgetsPath
        |> Array.filter (fun line -> line.Length > 0 && not (line.StartsWith "#"))
        |> Array.skip 1
        |> Array.fold (fun map line ->
            let fields = line.Split '\t'
            match fields.Length = 7 with
            | false -> map
            | true ->
                let budget =
                    { NativeGate = Double.Parse(fields[1], Globalization.CultureInfo.InvariantCulture)
                      NativeTarget = Double.Parse(fields[2], Globalization.CultureInfo.InvariantCulture)
                      HbsGate = Double.Parse(fields[3], Globalization.CultureInfo.InvariantCulture)
                      HbsTarget = Double.Parse(fields[4], Globalization.CultureInfo.InvariantCulture)
                      BytesGate = Double.Parse(fields[5], Globalization.CultureInfo.InvariantCulture)
                      BytesTarget = Double.Parse(fields[6], Globalization.CultureInfo.InvariantCulture) }
                Map.add fields[0] budget map) Map.empty

/// <summary>Reads the output hash column of the "Upstream pairs" table, the Farfbuzz hash of P9.S01.</summary>
/// <returns>The Farfbuzz hash by pair key.</returns>
let readFarfbuzzHashes () =
    match File.Exists baselinePath with
    | false -> Map.empty
    | true ->
        File.ReadAllLines baselinePath
        |> Array.filter (fun line -> line.StartsWith "|" && not (line.Contains "---") && line.Contains "/" && not (line.Contains "Pair"))
        |> Array.fold (fun map line ->
            let cells = line.Split('|') |> Array.map (fun cell -> cell.Trim()) |> Array.filter (fun cell -> cell.Length > 0)
            match cells.Length >= 6 with
            | true -> Map.add (normalizePair cells[0]) cells[5] map
            | false -> map) Map.empty

/// <summary>Reads the glyphs-per-pass column of the "Upstream pairs" table `--tally` appends to the baseline.</summary>
/// <returns>The glyph count by pair key.</returns>
let readGlyphCounts () =
    match File.Exists baselinePath with
    | false -> Map.empty
    | true ->
        File.ReadAllLines baselinePath
        |> Array.filter (fun line -> line.StartsWith "|" && not (line.Contains "---"))
        |> Array.filter (fun line -> line.Contains "/" && not (line.Contains "Pair"))
        |> Array.fold (fun map line ->
            let cells = line.Split('|') |> Array.map (fun cell -> cell.Trim()) |> Array.filter (fun cell -> cell.Length > 0)
            match cells.Length >= 4 with
            | false -> map
            | true ->
                match Int32.TryParse(cells[3], Globalization.NumberStyles.None, Globalization.CultureInfo.InvariantCulture) with
                | true, glyphs -> Map.add (normalizePair cells[0]) glyphs map
                | false, _ -> map) Map.empty

/// <summary>The job a benchmark row ran under, read from its display info.</summary>
/// <param name="displayInfo">The <c>DisplayInfo</c> field.</param>
/// <returns>The job id.</returns>
let jobOf (displayInfo: string) =
    match displayInfo.Contains "HarfBuzzSharp 14.4.0" with
    | true -> "HarfBuzzSharp 14.4.0"
    | false -> "HarfBuzz 15.1.0"

/// <summary>The pair name inside a benchmark's full name, the first quoted string.</summary>
/// <param name="fullName">The <c>FullName</c> field.</param>
/// <returns>The pair, or none when the full name has no quoted string.</returns>
let pairOf (fullName: string) =
    let first = fullName.IndexOf '"'
    let last = if first < 0 then -1 else fullName.IndexOf('"', first + 1)
    let marker = "(case: "
    let bare = fullName.IndexOf(marker, StringComparison.Ordinal)
    match first >= 0 && last > first, bare >= 0 && fullName.EndsWith ')' with
    | true, _ -> ValueSome (fullName.Substring(first + 1, last - first - 1))
    | false, true -> ValueSome (fullName.Substring(bare + marker.Length, fullName.Length - bare - marker.Length - 1))
    | false, false -> ValueNone

/// <summary>Whether a benchmark row is one of the three upstream-shape methods.</summary>
/// <param name="method">The <c>Method</c> field.</param>
/// <returns>True for Native, HarfBuzzSharp or Farfbuzz.</returns>
let isShapeMethod (method: string) =
    method = "Native" || method = "HarfBuzzSharp" || method = "Farfbuzz"

/// <summary>The allocated bytes of a benchmark row, when the memory diagnostic reported them.</summary>
/// <param name="memory">The <c>Memory</c> object, or none.</param>
/// <returns>The bytes allocated per operation, or none.</returns>
let bytesOf (memory: JsonElement voption) =
    match memory with
    | ValueNone -> ValueNone
    | ValueSome element ->
        match element.TryGetProperty "BytesAllocatedPerOperation" with
        | true, bytes -> ValueSome (bytes.GetDouble())
        | false, _ -> ValueNone

/// <summary>The mean of a benchmark row, when BenchmarkDotNet measured it: a row of a benchmark that failed to run has a null or missing <c>Statistics</c>.</summary>
/// <param name="row">One element of the report's <c>Benchmarks</c> array.</param>
/// <returns>The mean in nanoseconds, or none.</returns>
let meanOfRow (row: JsonElement) =
    match row.TryGetProperty "Statistics" with
    | false, _ -> ValueNone
    | true, statistics when statistics.ValueKind <> JsonValueKind.Object -> ValueNone
    | true, statistics ->
        match statistics.TryGetProperty "Mean" with
        | true, mean when mean.ValueKind = JsonValueKind.Number -> ValueSome (mean.GetDouble())
        | true, _ -> ValueNone
        | false, _ -> ValueNone

/// <summary>The text of a string field of a benchmark row, or the empty string when it is null or missing.</summary>
/// <param name="row">One element of the report's <c>Benchmarks</c> array.</param>
/// <param name="name">The field.</param>
/// <returns>The text.</returns>
let textOfRow (row: JsonElement) (name: string) =
    match row.TryGetProperty name with
    | true, element when element.ValueKind = JsonValueKind.String -> element.GetString() |> Option.ofObj |> Option.defaultValue String.Empty
    | true, _ -> String.Empty
    | false, _ -> String.Empty

/// <summary>Turns one benchmark row into an upstream-shape measurement, when it is one and was measured.</summary>
/// <param name="row">One element of the report's <c>Benchmarks</c> array.</param>
/// <returns>The measurement, or none.</returns>
let measurementOfRow (row: JsonElement) =
    let method = textOfRow row "Method"
    let fullName = textOfRow row "FullName"
    let memory =
        match row.TryGetProperty "Memory" with
        | true, element when element.ValueKind = JsonValueKind.Object -> ValueSome element
        | true, _ -> ValueNone
        | false, _ -> ValueNone
    match isShapeMethod method && fullName.Contains "UpstreamShape.", meanOfRow row, pairOf fullName with
    | true, ValueSome mean, ValueSome pair ->
        Some { Pair = normalizePair pair; Method = method; Job = jobOf (textOfRow row "DisplayInfo"); MeanNs = mean; Bytes = bytesOf memory }
    | true, ValueSome _, ValueNone -> None
    | true, ValueNone, _ -> None
    | false, _, _ -> None

/// <summary>The name of a benchmark row that has no statistics, with its job, or none when it was measured.</summary>
/// <param name="row">One element of the report's <c>Benchmarks</c> array.</param>
/// <returns>The name of the failed benchmark, or none.</returns>
let failureOfRow (row: JsonElement) =
    match meanOfRow row with
    | ValueSome _ -> None
    | ValueNone ->
        let fullName = textOfRow row "FullName"
        let job = jobOf (textOfRow row "DisplayInfo")
        Some $"{fullName} [{job}]"

/// <summary>Parses one `*-report-full.json` file into its upstream-shape measurements and the benchmarks that failed to run.</summary>
/// <param name="path">The report file.</param>
/// <returns>The measurements of the Native, HarfBuzzSharp and Farfbuzz methods of the upstream-shape benchmark, and the names of the benchmarks without statistics.</returns>
let parseReport (path: string) =
    let text = File.ReadAllText path
    use document = JsonDocument.Parse text
    let root = document.RootElement
    match root.TryGetProperty "Benchmarks" with
    | false, _ -> [], []
    | true, benchmarks ->
        let rows = benchmarks.EnumerateArray() |> Seq.toList
        rows |> List.choose measurementOfRow, rows |> List.choose failureOfRow

/// <summary>Every upstream-shape measurement and every failed benchmark in a results directory, one report per `*-report-full.json` file.</summary>
/// <param name="directory">The results directory.</param>
/// <returns>The measurements and the names of the benchmarks that failed to run.</returns>
let measurementsOf (directory: string) =
    match Directory.Exists directory with
    | false -> [], []
    | true ->
        let parsed = Directory.EnumerateFiles(directory, "*-report-full.json") |> Seq.sort |> Seq.map parseReport |> Seq.toList
        parsed |> List.collect fst, parsed |> List.collect snd

/// <summary>Parses one runner JSON file (P9.S05).</summary>
/// <param name="path">The file.</param>
/// <returns>The run.</returns>
let parseRunner (path: string) =
    use document = JsonDocument.Parse (File.ReadAllText path)
    let root = document.RootElement
    let text (name: string) = root.GetProperty(name).GetString()
    let results =
        root.GetProperty("results").EnumerateArray()
        |> Seq.map (fun item ->
            { Pair = normalizePair (item.GetProperty("pair").GetString())
              Glyphs = item.GetProperty("glyphsPerPass").GetInt32()
              MedianMs = item.GetProperty("medianMs").GetDouble()
              MinMs = item.GetProperty("minMs").GetDouble()
              BytesPerPass = item.GetProperty("bytesPerPass").GetDouble()
              Consistent = item.GetProperty("consistent").GetBoolean()
              Hash = item.GetProperty("hash").GetString() })
        |> Seq.toList
    { Impl = text "impl"
      Runtime = text "runtime"
      Architecture = text "architecture"
      Framework = text "framework"
      Vector128 = root.GetProperty("vector128").GetBoolean()
      Processors = root.GetProperty("processors").GetInt32()
      Results = results }

/// <summary>Every runner run in a directory, one per JSON file.</summary>
/// <param name="directory">The directory.</param>
/// <returns>The runs, in file-name order.</returns>
let runnerRunsOf (directory: string) =
    match Directory.Exists directory with
    | false -> []
    | true -> Directory.EnumerateFiles(directory, "*.json") |> Seq.sort |> Seq.map parseRunner |> Seq.toList

/// <summary>Whether a numeric value passes a budget: within the target, within the gate, or over the gate.</summary>
/// <param name="value">The measured value.</param>
/// <param name="target">The target.</param>
/// <param name="gate">The gate.</param>
/// <returns>The verdict.</returns>
let verdict (value: float) (target: float) (gate: float) =
    match value <= target with
    | true -> Pass
    | false ->
        match value <= gate with
        | true -> Gate
        | false -> Miss

/// <summary>Formats a ratio with a times sign and three significant digits.</summary>
/// <param name="ratio">The ratio.</param>
/// <returns>The text.</returns>
let formatRatio (ratio: float) =
    match ratio with
    | value when value >= 100.0 -> $"{value:F0}×"
    | value when value >= 10.0 -> $"{value:F1}×"
    | value -> $"{value:F2}×"

/// <summary>Formats a duration in nanoseconds at a human scale, stating the unit.</summary>
/// <param name="nanoseconds">The duration in nanoseconds.</param>
/// <returns>The text.</returns>
let formatDuration (nanoseconds: float) =
    match nanoseconds with
    | value when value >= 1_000_000_000.0 -> $"{value / 1_000_000_000.0:F2} s"
    | value when value >= 1_000_000.0 -> $"{value / 1_000_000.0:F2} ms"
    | value when value >= 1_000.0 -> $"{value / 1_000.0:F2} µs"
    | value -> $"{value:F0} ns"

/// <summary>Formats a byte count at a human scale, stating the unit.</summary>
/// <param name="bytes">The number of bytes.</param>
/// <returns>The text.</returns>
let formatBytes (bytes: float) =
    match bytes with
    | value when value >= 1_000_000.0 -> $"{value / 1_000_000.0:F2} MB"
    | value when value >= 1_000.0 -> $"{value / 1_000.0:F2} KB"
    | value -> $"{value:F1} B"

/// <summary>The measured mean of one method under one job, from a list of measurements.</summary>
/// <param name="measurements">The measurements of one pair.</param>
/// <param name="method">The method.</param>
/// <param name="job">The job.</param>
/// <returns>The mean in nanoseconds, or none.</returns>
let meanOf (measurements: Measurement list) (method: string) (job: string) =
    match measurements |> List.tryFind (fun measurement -> measurement.Method = method && measurement.Job = job) with
    | Some measurement -> ValueSome measurement.MeanNs
    | None -> ValueNone

/// <summary>The allocated bytes of the Farfbuzz method under the native job.</summary>
/// <param name="measurements">The measurements of one pair.</param>
/// <returns>The bytes, or none.</returns>
let farfbuzzBytes (measurements: Measurement list) =
    match measurements |> List.tryFind (fun measurement -> measurement.Method = "Farfbuzz" && measurement.Job = "HarfBuzz 15.1.0") with
    | Some measurement -> measurement.Bytes
    | None -> ValueNone

/// <summary>Computes one pair's row from its measurements, the budget and the glyph count.</summary>
/// <param name="pair">The pair key.</param>
/// <param name="measurements">The measurements of the pair.</param>
/// <param name="budget">The pair's budget, when present.</param>
/// <param name="glyphs">The glyphs per pass, when known.</param>
/// <returns>The row.</returns>
let computeRow (pair: string) (measurements: Measurement list) (budget: Budget voption) (glyphs: int voption) =
    let farfbuzzNs = meanOf measurements "Farfbuzz" "HarfBuzz 15.1.0" |> ValueOption.defaultValue nan
    let nativeNs = meanOf measurements "Native" "HarfBuzz 15.1.0" |> ValueOption.defaultValue nan
    let hbsNs = meanOf measurements "HarfBuzzSharp" "HarfBuzzSharp 14.4.0"
    let bytesPerGlyph =
        match farfbuzzBytes measurements, glyphs with
        | ValueSome bytes, ValueSome count when count > 0 -> ValueSome (bytes / float count)
        | _ -> ValueNone
    { Pair = pair; Runtime = "CoreCLR x64"; FarfbuzzNs = farfbuzzNs; NativeNs = nativeNs; HbsNs = hbsNs; BytesPerGlyph = bytesPerGlyph }

/// <summary>The host environment header, read from one report file.</summary>
/// <param name="directory">The results directory.</param>
/// <returns>The header lines, or none when no report file exists.</returns>
let environmentHeader (directory: string) =
    let first =
        match Directory.Exists directory with
        | true -> Directory.EnumerateFiles(directory, "*-report-full.json") |> Seq.tryHead
        | false -> None
    match first with
    | None -> ValueNone
    | Some path ->
        use document = JsonDocument.Parse (File.ReadAllText path)
        match document.RootElement.TryGetProperty "HostEnvironmentInfo" with
        | false, _ -> ValueNone
        | true, info ->
            let field (name: string) =
                match info.TryGetProperty name with
                | true, element ->
                    match element.ValueKind with
                    | JsonValueKind.String -> element.GetString()
                    | JsonValueKind.Number -> element.GetRawText()
                    | _ -> element.ToString()
                | false, _ -> "unknown"
            let version = field "BenchmarkDotNetVersion"
            let os = field "OsVersion"
            let processor = field "ProcessorName"
            let cores = field "LogicalCoreCount"
            let runtime = field "RuntimeVersion"
            let architecture = field "Architecture"
            ValueSome [ $"BenchmarkDotNet {version}"
                        $"OS: {os}"
                        $"Processor: {processor} ({cores} logical cores)"
                        $"Runtime: {runtime}"
                        $"Architecture: {architecture}" ]

/// <summary>Whether any measured cell of a row misses its gate.</summary>
/// <param name="row">The row.</param>
/// <param name="measurements">The measurements of the pair.</param>
/// <param name="budget">The pair's budget.</param>
/// <returns>True when a ratio or the bytes per glyph misses a gate.</returns>
let anyGateMiss (row: Row) (measurements: Measurement list) (budget: Budget voption) =
    let farfbuzz15 = row.FarfbuzzNs
    let native15 = row.NativeNs
    let farfbuzz14 = meanOf measurements "Farfbuzz" "HarfBuzzSharp 14.4.0" |> ValueOption.defaultValue nan
    match budget with
    | ValueNone -> false
    | ValueSome budget ->
        let nativeMiss = native15 > 0.0 && farfbuzz15 / native15 > budget.NativeGate
        let hbsMiss =
            match row.HbsNs, farfbuzz14 with
            | ValueSome hbs, farfbuzz when hbs > 0.0 && farfbuzz / hbs > budget.HbsGate -> true
            | _ -> false
        let bytesMiss =
            match row.BytesPerGlyph with
            | ValueSome bytes when bytes > budget.BytesGate -> true
            | _ -> false
        nativeMiss || hbsMiss || bytesMiss

/// <summary>Formats one ratio cell: the ratio, its unit and its verdict against the gate and target.</summary>
/// <param name="ratio">The measured ratio, or NaN when unmeasured.</param>
/// <param name="target">The target.</param>
/// <param name="gate">The gate.</param>
/// <returns>The cell text.</returns>
let ratioCell (ratio: float) (target: float) (gate: float) =
    match Double.IsNaN ratio with
    | true -> "—"
    | false ->
        let label =
            match verdict ratio target gate with
            | Pass -> "PASS"
            | Gate -> "gate"
            | Miss -> "GATE MISS"
            | Unmeasured -> "—"
        $"{formatRatio ratio} {label}"

/// <summary>Formats the bytes-per-glyph cell.</summary>
/// <param name="bytesPerGlyph">The measured bytes per glyph, or none.</param>
/// <param name="target">The target.</param>
/// <param name="gate">The gate.</param>
/// <returns>The cell text.</returns>
let bytesCell (bytesPerGlyph: float voption) (target: float) (gate: float) =
    match bytesPerGlyph with
    | ValueNone -> "—"
    | ValueSome bytes ->
        let label =
            match verdict bytes target gate with
            | Pass -> "PASS"
            | Gate -> "gate"
            | Miss -> "GATE MISS"
            | Unmeasured -> "—"
        $"{formatBytes bytes} {label}"

/// <summary>The pair keys in the order they appear in the budget table, so the report lists all ten in
/// benchmark-shape.cc order even when a pair is unmeasured.</summary>
/// <returns>The pair keys in file order.</returns>
let orderedPairs () =
    match File.Exists budgetsPath with
    | false -> []
    | true ->
        File.ReadAllLines budgetsPath
        |> Array.filter (fun line -> line.Length > 0 && not (line.StartsWith "#"))
        |> Array.skip 1
        |> Array.choose (fun line ->
            let fields = line.Split '\t'
            match fields.Length = 7 with
            | true -> Some fields[0]
            | false -> None)
        |> Array.toList

/// <summary>The runtimes of the platform matrix whose cells are held to the budgets (exit criteria of phase 9); the
/// WebAssembly interpreter is recorded only.</summary>
let gatedRuntimes = [ "NativeAOT"; "WASM-AOT" ]

/// <summary>The runtimes of the platform matrix, in the order of the report.</summary>
let matrixRuntimes = [ "NativeAOT"; "WASM-interp"; "WASM-AOT" ]

/// <summary>The run of an implementation on a runtime.</summary>
/// <param name="runs">The runner runs.</param>
/// <param name="runtime">The runtime label.</param>
/// <param name="impl">The implementation name.</param>
/// <returns>The run, or none.</returns>
let runOf (runs: RunnerRun list) (runtime: string) (impl: string) =
    runs |> List.tryFind (fun run -> run.Runtime = runtime && run.Impl = impl)

/// <summary>The result of one pair in a run.</summary>
/// <param name="run">The run, or none.</param>
/// <param name="pair">The pair key.</param>
/// <returns>The result, or none.</returns>
let resultOf (run: RunnerRun option) (pair: string) =
    run |> Option.bind (fun run -> run.Results |> List.tryFind (fun result -> result.Pair = pair))

/// <summary>Formats a duration in milliseconds, stating the unit.</summary>
/// <param name="milliseconds">The duration.</param>
/// <returns>The text.</returns>
let formatMilliseconds (milliseconds: float) = $"{milliseconds:F1} ms"

/// <summary>The ratio and bytes verdicts of one pair on a runtime, Farfbuzz against the native implementation of the same runtime
/// (HarfBuzzSharp 14.4.0), with the budget of the HarfBuzzSharp column.</summary>
/// <param name="budget">The pair's budget.</param>
/// <param name="farfbuzz">The Farfbuzz result.</param>
/// <param name="native">The native result.</param>
/// <returns>True when the ratio or the bytes per glyph misses a gate.</returns>
let matrixMiss (budget: Budget voption) (farfbuzz: RunnerResult option) (native: RunnerResult option) =
    match budget, farfbuzz, native with
    | ValueSome budget, Some farfbuzz, Some native ->
        let ratioMiss = native.MedianMs > 0.0 && farfbuzz.MedianMs / native.MedianMs > budget.HbsGate
        let bytesMiss = farfbuzz.Glyphs > 0 && farfbuzz.BytesPerPass / float farfbuzz.Glyphs > budget.BytesGate
        ratioMiss || bytesMiss
    | ValueSome budget, Some farfbuzz, None ->
        farfbuzz.Glyphs > 0 && farfbuzz.BytesPerPass / float farfbuzz.Glyphs > budget.BytesGate
    | ValueNone, _, _ | ValueSome _, None, _ -> false

/// <summary>Whether any gated runtime of the matrix misses a gate on any pair.</summary>
/// <param name="budgets">The budgets.</param>
/// <param name="runs">The runner runs.</param>
/// <returns>True when a gate is missed.</returns>
let anyMatrixMiss (budgets: Map<string, Budget>) (runs: RunnerRun list) =
    gatedRuntimes
    |> List.exists (fun runtime ->
        orderedPairs ()
        |> List.exists (fun pair ->
            matrixMiss (budgets |> Map.tryFind pair |> Option.toValueOption) (resultOf (runOf runs runtime "farfbuzz") pair) (resultOf (runOf runs runtime "native") pair)))

/// <summary>The hash cell: equal to the expected hash, different from it, or the hash itself when nothing is expected.</summary>
/// <param name="expected">The expected hash, if one is known.</param>
/// <param name="hash">The measured hash.</param>
/// <param name="reference">What the expectation is, for the cell text.</param>
/// <returns>The cell text.</returns>
let hashCell (expected: string voption) (hash: string) (reference: string) =
    match expected with
    | ValueSome value when value = hash -> $"= {reference}"
    | ValueSome _ -> $"DIFFERS from {reference}"
    | ValueNone -> hash

/// <summary>Whether any runner result has an output hash it must not have: a Farfbuzz hash different from the one of P9.S01, or a native hash
/// different from the NativeAOT native run of the same pair.</summary>
/// <param name="hashes">The Farfbuzz hash of P9.S01 by pair.</param>
/// <param name="runs">The runner runs.</param>
/// <returns>True when a hash differs.</returns>
let anyHashDiffers (hashes: Map<string, string>) (runs: RunnerRun list) =
    let reference = runOf runs "NativeAOT" "native"
    runs
    |> List.exists (fun run ->
        run.Results
        |> List.exists (fun result ->
            match run.Impl, hashes |> Map.tryFind result.Pair, resultOf reference result.Pair with
            | _, _, _ when not result.Consistent -> true
            | "farfbuzz", Some expected, _ -> expected <> result.Hash
            | "native", _, Some expected -> expected.Hash <> result.Hash
            | _, _, _ -> false))

/// <summary>The table of one runtime of the platform matrix.</summary>
/// <param name="budgets">The budgets.</param>
/// <param name="hashes">The Farfbuzz hash of P9.S01 by pair.</param>
/// <param name="runs">The runner runs.</param>
/// <param name="runtime">The runtime label.</param>
/// <returns>The markdown lines.</returns>
let matrixTable (budgets: Map<string, Budget>) (hashes: Map<string, string>) (runs: RunnerRun list) (runtime: string) =
    let farfbuzzRun = runOf runs runtime "farfbuzz"
    let nativeRun = runOf runs runtime "native"
    let gated = List.contains runtime gatedRuntimes
    let reference = runOf runs "NativeAOT" "native"
    let describe =
        match farfbuzzRun with
        | Some run -> $"{run.Framework}, {run.Architecture}, Vector128 accelerated: {run.Vector128}, {run.Processors} logical processors"
        | None -> "not run"
    let rows =
        orderedPairs ()
        |> List.map (fun pair ->
            let farfbuzz = resultOf farfbuzzRun pair
            let native = resultOf nativeRun pair
            let budget = budgets |> Map.tryFind pair |> Option.toValueOption
            let ratio =
                match farfbuzz, native, budget with
                | Some farfbuzz, Some native, ValueSome budget when gated -> ratioCell (farfbuzz.MedianMs / native.MedianMs) budget.HbsTarget budget.HbsGate
                | Some farfbuzz, Some native, _ -> formatRatio (farfbuzz.MedianMs / native.MedianMs)
                | _, _, _ -> "—"
            let bytes =
                match farfbuzz, budget with
                | Some farfbuzz, ValueSome budget when gated && farfbuzz.Glyphs > 0 -> bytesCell (ValueSome (farfbuzz.BytesPerPass / float farfbuzz.Glyphs)) budget.BytesTarget budget.BytesGate
                | Some farfbuzz, _ when farfbuzz.Glyphs > 0 -> formatBytes (farfbuzz.BytesPerPass / float farfbuzz.Glyphs)
                | _, _ -> "—"
            let farfbuzzHash =
                match farfbuzz with
                | Some result ->
                    let cell = hashCell (hashes |> Map.tryFind pair |> Option.toValueOption) result.Hash "P9.S01"
                    match result.Consistent with
                    | true -> cell
                    | false -> $"{cell}; PASSES DIFFER IN GLYPHS"
                | None -> "—"
            let nativeHash =
                match native, resultOf reference pair with
                | Some result, Some expected when runtime <> "NativeAOT" -> hashCell (ValueSome expected.Hash) result.Hash "NativeAOT native"
                | Some result, _ -> result.Hash
                | None, _ -> "—"
            let time (result: RunnerResult option) = result |> Option.map (fun result -> formatMilliseconds result.MedianMs) |> Option.defaultValue "not measured"
            $"| {pair} | {time farfbuzz} | {time native} | {ratio} | {bytes} | {farfbuzzHash} | {nativeHash} |")
    [ $"### {runtime}"
      ""
      $"Runtime: {describe}."
      ""
      "| Pair | Farfbuzz median pass | HarfBuzzSharp 14.4.0 median pass | Farfbuzz / HarfBuzzSharp | Farfbuzz B/glyph | Farfbuzz hash | HarfBuzzSharp hash |"
      "|---|---:|---:|---|---|---|---|" ]
    @ rows
    @ [ "" ]

/// <summary>The platform-matrix section: one table per runtime of the runner, and the ARM64 line or tables.</summary>
/// <param name="budgets">The budgets.</param>
/// <param name="hashes">The Farfbuzz hash of P9.S01 by pair.</param>
/// <param name="runs">The runner runs of this host.</param>
/// <param name="arm64">The runner runs of an ARM64 host, committed under the runner directory's arm64 folder.</param>
/// <returns>The markdown lines.</returns>
let matrixSection (budgets: Map<string, Budget>) (hashes: Map<string, string>) (runs: RunnerRun list) (arm64: RunnerRun list) =
    let intro =
        [ ""
          "## Platform matrix"
          ""
          "Written by the runner of P9.S05 (`bench/Farfbuzz.Bench.Runner`, `bench/Farfbuzz.Bench.Wasm`) through `dotnet fsi build.fsx perf-matrix`."
          "Each time is the median pass over every line of the pair (at least 10 passes and 2 s), in milliseconds; B/glyph is the bytes"
          "allocated per pass over the glyphs per pass. The native column is HarfBuzzSharp 14.4.0, whose bundled HarfBuzz is 14.4.0,"
          "not the oracle's 15.1.0: its hash is compared with the NativeAOT run of the same implementation, never with Farfbuzz's."
          "Verdicts apply to NativeAOT and WASM-AOT; the WebAssembly interpreter is recorded without verdicts."
          "" ]
    let tables = matrixRuntimes |> List.collect (matrixTable budgets hashes runs)
    let arm =
        match arm64 with
        | [] -> [ "ARM64: not run (no host)"; "" ]
        | armRuns ->
            [ "### ARM64"; "" ]
            @ (matrixRuntimes |> List.collect (matrixTable budgets hashes armRuns))
    intro @ tables @ arm

/// <summary>The markdown report body, one row per pair.</summary>
/// <param name="budgets">The budget map.</param>
/// <param name="glyphs">The glyph counts.</param>
/// <param name="measurements">All measurements.</param>
/// <returns>The report text.</returns>
let buildReport (budgets: Map<string, Budget>) (glyphs: Map<string, int>) (hashes: Map<string, string>) (measurements: Measurement list) (resultsDir: string) (runs: RunnerRun list) (arm64: RunnerRun list) (note: string) (failures: string list) =
    let rows =
        orderedPairs ()
        |> List.map (fun pair ->
            let ofPair = measurements |> List.filter (fun measurement -> measurement.Pair = pair)
            let budget = budgets |> Map.tryFind pair |> Option.toValueOption
            let glyphCount = glyphs |> Map.tryFind pair |> Option.toValueOption
            computeRow pair ofPair budget glyphCount)
    let header =
        match environmentHeader resultsDir with
        | ValueNone -> [ "No BenchmarkDotNet results were found: the results directory has no `*-report-full.json`." ]
        | ValueSome lines -> lines
    let today = DateTime.Now.ToString("yyyy-MM-dd", Globalization.CultureInfo.InvariantCulture)
    let lines =
        [ "# Performance report"
          ""
          $"Generated by `dotnet fsi tools/perf/PerfReport.fsx` on {today}. Budgets from"
          "`docs/research/perf-budgets.tsv`; glyphs per pass from the \"Upstream pairs\" table of"
          "`docs/research/perf-baseline.md` (written by `--tally`, P9.S01)."
          ""
          "## Environment"
          "" ]
        @ (header |> List.map (fun line -> $"- {line}"))
        @ (match note.Length with
           | 0 -> []
           | _ -> [ ""; note ])
        @ [ ""
            "## CoreCLR x64 (BenchmarkDotNet)"
            ""
            "Each ratio is Farfbuzz mean / baseline mean for one full pass over the pair. B/glyph is the Farfbuzz"
            "`Memory.BytesAllocatedPerOperation` divided by the glyphs per pass. `PASS` meets the target, `gate` is"
            "within the gate but above the target, `GATE MISS` exceeds the gate."
            ""
            "| Pair | Farfbuzz | Native | vs Native | vs HBS | B/glyph |"
            "|---|---:|---:|---|---|---|" ]
    let body =
        rows
        |> List.map (fun row ->
            match row with
            | row when Double.IsNaN row.FarfbuzzNs ->
                $"| {row.Pair} | not measured | not measured | — | — | — |"
            | row ->
                let farfbuzz = row.FarfbuzzNs
                let native = row.NativeNs
                let hbs = row.HbsNs
                let farfbuzz14 = meanOf (measurements |> List.filter (fun m -> m.Pair = row.Pair)) "Farfbuzz" "HarfBuzzSharp 14.4.0" |> ValueOption.defaultValue nan
                let budget = budgets |> Map.tryFind row.Pair |> Option.toValueOption
                let nativeCell =
                    match budget with
                    | ValueSome budget -> ratioCell (farfbuzz / native) budget.NativeTarget budget.NativeGate
                    | ValueNone -> formatRatio (farfbuzz / native)
                let hbsCell =
                    match budget, hbs with
                    | ValueSome budget, ValueSome hbs -> ratioCell (farfbuzz14 / hbs) budget.HbsTarget budget.HbsGate
                    | _, _ -> "—"
                let bytesCell =
                    match budget with
                    | ValueSome budget -> bytesCell row.BytesPerGlyph budget.BytesTarget budget.BytesGate
                    | ValueNone -> "—"
                $"| {row.Pair} | {formatDuration farfbuzz} | {formatDuration native} | {nativeCell} | {hbsCell} | {bytesCell} |")
    let misses =
        rows
        |> List.filter (fun row -> anyGateMiss row (measurements |> List.filter (fun m -> m.Pair = row.Pair)) (budgets |> Map.tryFind row.Pair |> Option.toValueOption))
        |> List.map _.Pair
    let missSection =
        match misses with
        | [] -> [ ""; "## Gate misses"; ""; "None." ]
        | pairs -> [ ""; "## Gate misses"; "" ] @ (pairs |> List.map (fun pair -> $"- {pair}"))
    let failureSection =
        match failures with
        | [] -> [ ""; "## Benchmarks that failed to run"; ""; "None." ]
        | names ->
            [ ""; "## Benchmarks that failed to run"; ""; $"{names.Length} benchmarks failed to run (no statistics in the BenchmarkDotNet report); `--enforce` fails while any exists." ; "" ]
            @ (names |> List.map (fun name -> $"- {name}: failed to run"))
    let closeOut =
        [ ""; "The close-out state of phase 9 is kept by hand in [perf-close-out.md](perf-close-out.md), which this script never writes." ]
    lines @ body @ missSection @ failureSection @ closeOut @ matrixSection budgets hashes runs arm64

/// <summary>Runs the report: reads budgets, glyphs, measurements and runner runs, writes the report, and enforces when asked.</summary>
/// <param name="resultsDir">The BenchmarkDotNet results directory.</param>
/// <param name="runnerDir">The runner JSON directory; its <c>arm64</c> folder holds the runs of an ARM64 host.</param>
/// <param name="enforce">Exit 1 on any gate miss.</param>
/// <param name="note">A line placed under the environment header, or the empty string.</param>
/// <returns>The exit code.</returns>
let run (resultsDir: string) (runnerDir: string) (enforce: bool) (note: string) =
    let budgets = readBudgets ()
    let glyphs = readGlyphCounts ()
    let hashes = readFarfbuzzHashes ()
    let measurements, failures = measurementsOf resultsDir
    let runs = runnerRunsOf runnerDir
    let arm64 = runnerRunsOf (Path.Combine(runnerDir, "arm64"))
    let report = buildReport budgets glyphs hashes measurements resultsDir runs arm64 note failures
    File.WriteAllLines(reportPath, report)
    Console.WriteLine $"    wrote {Path.GetRelativePath(root, reportPath)} ({measurements.Length} measurements, {failures.Length} benchmarks failed to run, {runs.Length} runner runs, {arm64.Length} ARM64 runner runs)"
    let anyMiss =
        orderedPairs ()
        |> List.exists (fun pair ->
            let ofPair = measurements |> List.filter (fun measurement -> measurement.Pair = pair)
            let budget = budgets |> Map.tryFind pair |> Option.toValueOption
            let row = computeRow pair ofPair budget (glyphs |> Map.tryFind pair |> Option.toValueOption)
            anyGateMiss row ofPair budget)
        || anyMatrixMiss budgets runs
        || anyHashDiffers hashes runs
    failures |> List.iter (fun name -> Console.Error.WriteLine $"    failed to run: {name}")
    match enforce, anyMiss, failures with
    | true, true, _ ->
        Console.Error.WriteLine "    GATE MISS: a pair exceeds its budget (see the report)"
        1
    | true, false, _ :: _ ->
        Console.Error.WriteLine $"    {failures.Length} benchmarks failed to run (see the report)"
        1
    | true, false, [] -> 0
    | false, _, _ -> 0

let arguments = fsi.CommandLineArgs |> Array.toList |> List.tail

/// <summary>The text after <c>--note</c>, or the empty string.</summary>
/// <param name="arguments">The arguments.</param>
/// <returns>The note.</returns>
let rec noteOf (arguments: string list) =
    match arguments with
    | "--note" :: text :: _ -> text
    | _ :: rest -> noteOf rest
    | [] -> String.Empty

/// <summary>The arguments that are not options or the value of <c>--note</c>.</summary>
/// <param name="arguments">The arguments.</param>
/// <returns>The positional arguments.</returns>
let rec positionalOf (arguments: string list) =
    match arguments with
    | "--note" :: _ :: rest -> positionalOf rest
    | argument :: rest when argument.StartsWith "--" -> positionalOf rest
    | argument :: rest -> argument :: positionalOf rest
    | [] -> []

let enforce = arguments |> List.contains "--enforce"
match positionalOf arguments with
| resultsDir :: runnerDir :: _ -> exit (run resultsDir runnerDir enforce (noteOf arguments))
| _ ->
    Console.Error.WriteLine "usage: dotnet fsi tools/perf/PerfReport.fsx [--enforce] [--note <text>] <results-dir> <runner-json-dir>"
    exit 2

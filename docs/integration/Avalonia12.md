# Farfbuzz in Avalonia 12

`Farfbuzz.Avalonia` is Avalonia 12's text shaping subsystem on Farfbuzz. It implements `ITextShaperImpl` and `ITextShaperTypeface` directly on the core: no HarfBuzzSharp, no facade, no native library.

## Use

```csharp
AppBuilder.Configure<App>()
    .UsePlatformDetect()
    .UseFarfbuzz();
```

Call `UseFarfbuzz()` after `UsePlatformDetect()`: `UsePlatformDetect` installs HarfBuzz (`UseHarfBuzz`), and the last initializer of the text shaping subsystem wins (`AppBuilder.cs`). Avalonia.HarfBuzz is still loaded by the platform packages but never shapes. Prune the HarfBuzzSharp packages as for Avalonia 11 (`docs/integration/MyApp.md`) if its native libraries should not ship.

The project pins Avalonia `[12.1.1]` and compiles against the implementation assembly with `AvaloniaAccessUnstablePrivateApis`, because `ITextShaperImpl` and `ITextShaperTypeface` are `[NotClientImplementable]` and `AvaloniaLocator` is `[PrivateApi]`. Warning AVA3001 is expected and suppressed. A different Avalonia version needs the pin changed and the test below run again.

## What it does

`FarfbuzzTextShaper.ShapeText` takes the steps of `HarfBuzzTextShaper`:

1. an empty text gives an empty `ShapedBuffer`;
2. the text is decoded as UTF-16 from the memory that contains it (string or array), the part being the item and the rest its context, clusters counting from the start of the whole text and made relative to the start of the part afterwards;
3. a trailing break is merged into zero-width non-joiners on the run's own glyph array (`AdapterRuns.MergeBreakPair`);
4. segment properties are guessed, the direction is set from the bidi level and the language from `culture.TwoLetterISOLanguageName`;
5. the features of `TextShaperOptions` are passed as they are;
6. the plan cache of the typeface is threaded through `Shape.shapeCached`;
7. the advance and offset of each glyph are scaled by `fontRenderingEmSize / scale`, plus the letter spacing; a tab becomes the space glyph with the incremental tab width, or four space advances when there is none.

`FarfbuzzTypeface` builds the core face from the tables of the platform typeface (`IFontMemory.TryGetTable`), or from its stream when it has no table access.

## Test

```
dotnet run -c Release --project tests/Farfbuzz.Avalonia.Tests -- --impl harfbuzz artifacts/avalonia12/native.tsv
dotnet run -c Release --project tests/Farfbuzz.Avalonia.Tests -- --impl farfbuzz artifacts/avalonia12/farfbuzz.tsv
cmp artifacts/avalonia12/native.tsv artifacts/avalonia12/farfbuzz.tsv
```

The program starts Avalonia 12.1.1 with Skia and a standard runtime platform and no window (`Avalonia.Headless` 12.1.1 is not in the local package cache), shapes the corpus of the Avalonia 11 sample and some more (parts of longer texts, letter spacing, a feature switched off, a CRLF inside a part), and writes `line, index, glyph, cluster, advance, offset x, offset y`. The two dumps are byte-identical.

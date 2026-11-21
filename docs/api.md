# The public API of Farfbuzz

Farfbuzz is a library of F# functions over immutable values. This page lists what a program may call without knowing how the engine is built. Everything else in `Farfbuzz.dll` is `internal`: the table readers, the shapers, the working sets and the generated data change when the engine changes, and no program may depend on them. The list is pinned by `tests/Farfbuzz.PublicApi/public-api.baseline`; a change to it is the owner's decision, recorded in `PLAN.md` (decision 14).

Version rule: 0.x is a preview. The owner decides 1.0; until then a signature may change, with a change of the baseline and of this page.

## The list

| Area | Public names |
|---|---|
| Values | `Tag`, `Tags`, `Feature`, `Variation`, `Direction`, `Script`, `Scripts`, `Language`, `SegmentProperties`, with their parsers and formatters (`Feature.parse`, `Feature.format`, `Variation.parse`, `Variation.format`, `Tag.ofString`, `Tag.toString`, `Language.ofString`) |
| Fonts | `Face` (`create`, `createOrFail`, `empty`, `count`, `upem`, `glyphCount`, `withUpem`, `ofTables`, `tableTags`, `referenceTable`) and `Font` (`create`, `withScale`, `withPpem`, `withPtem`, `withSyntheticBold`, `withSyntheticSlant`, variations and named instances, metrics, advances (`hAdvance`, `vAdvance`, `hAdvances`, `vAdvances`, and `hAdvancesInto`, `vAdvancesInto`, which write into an array you own and allocate nothing: the result is the number of glyphs, and an array shorter than that is left as it was), extents, glyph names, glyph lookup) |
| Text and glyphs | `GlyphRun` (`empty`, `addUtf8`, `addUtf16`, `addUtf32`, `addCodepoints`, `withDirection`, `withScript`, `withLanguage`, `withFlags`, `withClusterLevel`, `guessSegmentProperties`, `infos`, `positions`, `segmentProperties`), `GlyphInfo`, `GlyphPosition`, `GlyphFlags`, `GlyphExtents`, `FontExtents`, `BufferFlags`, `ClusterLevel` |
| Shaping | `Shape` (`shape`, `shapeFull`, `listShapers`, `shapeWithPlan`), `ShapePlan` (`create`), `PlanCache` (`empty`), `LineShape` (`utf8`, `utf16`, `utf32`, `codepoints`: the lines of a document through one font and one plan cache) and `ShapedDocument` (`infosOf`, `positionsOf`, `errorOf`) |
| Errors | `ParseError`, `ShapeError`, `LineOutcome`, `DetachedLine` |
| Text formats | `Serialize` (glyphs to and from the text and JSON forms of `hb-shape`), `SerializeFlags` |

The names are the core's, not HarfBuzz's. The HarfBuzzSharp facade is a different assembly (`HarfBuzzSharp`) with HarfBuzzSharp's own names; see `docs/handoff/phase-10-harfbuzzsharp-facade.md`.

## Examples

Shape one line (`tests/Farfbuzz.PublicApi/Use.fs`):

```fsharp
let font = Font.create (Face.create (ReadOnlyMemory bytes) 0u)
let run =
    GlyphRun.addUtf8 (ReadOnlySpan utf8) 0 utf8.Length GlyphRun.empty
    |> GlyphRun.guessSegmentProperties Language.cLocale
match Shape.shape font ImmutableArray.Empty run with
| Ok shaped -> GlyphRun.infos shaped, GlyphRun.positions shaped
| Error error -> failwithf "%A" error
```

Share one `Font` across the lines of a document: `LineShape.utf8 degree PlanCache.empty font features shapers defaultLanguage settings text lines` shapes every line through one plan cache, and `ShapedDocument.infosOf document index` reads a line, the glyphs of each equal to those of the line shaped alone.

A shaping error is a value: `Shape.shapeFull font features (ValueSome shapers) run` returns `Error` when no requested shaper exists, and nothing is thrown.

Keep a plan and reuse it: `ShapePlan.create font (GlyphRun.segmentProperties run) features shapers` returns the plan, `Shape.shapeWithPlan font plan features run` shapes with it, and the result equals a fresh `Shape.shape`.

## What is not here

Line breaking, bidirectional ordering and font fallback are not HarfBuzz's job, so they are not Farfbuzz's either. A program that needs them uses a library that does them and hands Farfbuzz one run, in one direction, in one font.

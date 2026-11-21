# Divergences from HarfBuzz

Every intentional difference between Farfbuzz and HarfBuzz 15.1.0 is recorded here, in the same commit
that introduces it. A conformance id may only leave `tests/Farfbuzz.Tests/conformance.baseline` together
with an entry here (the guard enforces it).

## Entry format

### D-<number> — <short title>
- **Upstream behaviour:** what HarfBuzz does, with `path:line` at the pinned tag.
- **Farfbuzz behaviour:** what we do instead.
- **Reason:** why (drop-in constraint, platform, upstream bug, harness limitation…).
- **Tests affected:** conformance ids or test names, or "none".
- **Revisit when:** the condition that would remove the divergence.

## Entries

### D-1 — Darwin `%g` trailing zero on an exact tie
- **Upstream behaviour:** on macOS, HarfBuzz's `%g` inherits the Darwin C library's trailing zero on some exact ties, for example `8.59980e+06` (`src/hb-number.cc:95-152` calls the host `snprintf`).
- **Farfbuzz behaviour:** always prints the C-standard form with trailing fraction zeros removed, for example `8.5998e+06`.
- **Reason:** formatting is a pure function and must not depend on the host platform.
- **Tests affected:** `P1.TextFormats` normalises only the oracle's Darwin output; `P1.Number` pins the standard form.
- **Revisit when:** the owner accepts a host-dependent formatter.

### D-5 — Apply-time check of an anchor of format 3 does not charge the operation budget
- **Upstream behaviour:** `GSUBGPOS::sanitize` dispatches every lookup subtable's `sanitize`
  (`src/hb-ot-layout-common.hh:1434`), and the apply-time check of an anchor of format 3 charges the delta words of its
  hinting devices against the apply context's operation budget (`max(64 x table length, 16,384)`, per `apply_string` of one map).
- **Farfbuzz behaviour:** every subtable type of GSUB (types 1-8) and of GPOS (types 1-9) is deep-sanitized in
  `LayoutTable.sanitize` with the operation budget upstream charges, extension subtables included (a zero inner offset
  is the Null object and refused, `src/hb-open-type.hh:424-428`; the context and chained-context subtables of GPOS types 7
  and 8 are the code of GSUB types 5 and 6, P5.S12), so a lookup whose subtable is invalid rejects the whole table exactly when
  HarfBuzz rejects it. What remains is the charge of the devices of an anchor of format 3 at apply time, which Farfbuzz does
  not make: a table of a few hundred bytes whose run applies such an anchor thousands of times would see HarfBuzz stop finding
  anchors where Farfbuzz goes on. Anchors of formats 1 and 2 and null offsets charge nothing (`check_struct` is a point check).
- **Reason:** the budget of the apply context is not modelled for anchors; no corpus face or generated table reaches it.
- **Tests affected:** the `P5.S07` mutated-subtable differential (single, multiple, alternate), the `P5.S08` mutated-ligature
  differential, the `P5.S10` differential for a GPOS single or pair subtable, the `P5.S09` differential for a context, chained-context
  or reverse-chaining subtable, the `P5.S11` differentials for a cursive or mark attachment subtable and the `P5.S12` differential for a
  GPOS context, chained-context or extension subtable (5,000 changed-byte tables of real fonts, one count, offset, format, coverage,
  class definition, rule or lookup-record field at a time) all compare acceptance and shaped output with HarfBuzz.
- **Revisit when:** a font reaches the anchor device budget, or the apply context's budget is ported with the anchors' devices.

### D-7 — Fallback mark positioning on unported outlines
- **Upstream behaviour:** `_hb_ot_shape_fallback_mark_position` reads the base glyph's extents through the font's
  outline source, so on a VARC face it positions the mark over the base exactly as on a glyf face.
- **Farfbuzz behaviour:** `FallbackMark.position` returns `Error (OutlinesNotPorted source)` when the base has a mark
  to position on a VARC face (and `SyntheticOutlines` for a synthetic face), so shaping fails with a typed error
  instead of producing a mark offset. CFF1 faces position the mark from their charstring bounds since P7.S16, CFF2
  faces since P7.S17, sbix and CBDT faces from their bitmap extents since P7.S18, COLR faces from their clip boxes and
  paint graphs since P7.S19.
- **Reason:** glyph extents for VARC outlines are not ported yet. A quietly wrong position would be worse than a typed error.
- **Tests affected:** `P5.S14 marks on every stripped face`, `P4.S04 no-layout shaping` and `P4.S07 no-layout vertical
  shaping` count these answers as "outlines not ported" per source; the fixed case `a mark on a stripped CFF face is
  positioned as HarfBuzz positions it` compares a CFF1 face with the oracle.
- **Revisit when:** the extra slices port glyph extents for VARC outlines.

### D-8 — `collectLookups` does not collect FeatureVariations lookups or the DoS limits
- **Upstream behaviour:** `hb_ot_layout_collect_lookups` (src/hb-ot-layout.cc:1367-1383) appends the lookups of every
  feature whose index `hb_ot_layout_collect_features` collects, then the FeatureVariations lookups
  (`feature_variation_collect_lookups`: the substituted features and the version-1.1 lookup-variation records), and the
  feature walk is bounded by `HB_MAX_SCRIPTS` (500), `HB_MAX_LANGSYS` (2000) and `HB_MAX_FEATURE_INDICES` (8000,
  src/hb-limits.hh:75-88).
- **Farfbuzz behaviour:** `LayoutApi.collectLookups` collects the feature lookups and the required features but not the
  FeatureVariations lookups, and does not enforce the three DoS bounds.
- **Reason:** FeatureVariations lookups belong to the variation machinery (phase 7) and the bounds are DoS protections;
  `collectLookups` is exposed for API completeness and is not shaped against the oracle yet.
- **Tests affected:** none.
- **Revisit when:** a FeatureVariations face queries `collectLookups`, or the function gains a differential.

### D-9 — The `would_substitute` tests of the Indic reordering do not charge the operation budget
- **Upstream behaviour:** `hb_indic_would_substitute_feature_t::would_substitute` (src/hb-ot-shaper-indic.cc:98-110) calls
  `hb_ot_layout_lookup_would_substitute_with_buffer`, whose `hb_would_apply_context_t` charges the walk over subtables, rules
  and ligatures to the buffer's work budget (`charge_ops`, src/hb-ot-layout-gsubgpos.hh:298-309; src/hb-ot-layout.cc:1595-1611):
  a font whose offsets alias each other can make the shaper stop, marking the buffer unsuccessful.
- **Farfbuzz behaviour:** `LayoutApi.wouldSubstitute` walks the same subtables without a budget, so the consonant-position and
  reph tests of `IndicInitial` never run the buffer out of operations.
- **Reason:** the budget of the apply context is not modelled for `would_apply`; the subtables are sanitized with their own
  budget when the table is loaded, and no corpus face or generated syllable reaches the difference.
- **Tests affected:** none.
- **Revisit when:** a mutated GSUB table makes a would-apply walk exceed the budget in a differential, or the operation budget
  is passed to `LayoutApi.wouldSubstitute`.

### D-10 — The budget spent on a glyph that fails to draw under a COLR clip is not carried, and CFF2 blending is charged beside the sink
- **Upstream behaviour:** `hb_paint_extents_push_clip_glyph` (src/hb-paint-extents.cc:131-145) draws the clip glyph through
  `hb_font_draw_glyph` into a sink that spends one segment from the session's live budget at each emitted segment
  (`consume_segment`, src/hb-paint-extents.cc:79-85), the same counter the glyf point assembly and the CFF charstring
  interpreter spend from (`draw_session.get_budget ()`, src/hb-ot-font.cc:923-930). A glyf glyph whose assembly fails (a
  composite cycle, too many points, the budget) leaves the budget it spent before failing spent, and CFF2 `blend` charges its
  proportional work to the same counter while the charstring runs (src/hb-cff2-interp-cs.hh:332).
- **Farfbuzz behaviour:** a glyf glyph that fails to assemble returns no box and gives the budget back, so the next clip is
  drawn from a budget up to a few thousand units larger than upstream's; a CFF2 charstring's blend charges are taken from the
  budget when the charstring ends, so the sink counts its segments against a budget that does not yet hold them. Outlines that
  draw are exact: glyf points are paid first and then every segment in upstream's order, CFF charstrings keep the points
  emitted before a failure (`CffCharString.interpretKeeping`) and are charged their operators whether or not they succeed.
- **Reason:** `GlyphOutline.pointsWithin` returns the budget only for a glyph that assembles, and the CFF2 interpreter keeps
  the budget in its own state; sharing one counter would change both for every other user.
- **Tests affected:** `P7.COLR extents at default coordinates`, `P7.COLR extents at varied coordinates` and
  `P7.COLR extents of altered tables` compare every COLR face of the corpus and 14 955 altered tables with the oracle and
  agree; no corpus face exhausts the 16 777 216-segment budget after a failed glyph or inside CFF2 blending.
- **Revisit when:** a COLR font whose clip glyphs fail to assemble or blend under CFF2 near the budget shows a difference.

### D-11 — A buffer that has failed on its length limit or its operation budget is left differently in the stages before the lookups
- **Upstream behaviour:** `hb_shape` runs every stage whatever the buffer's state: `hb_ot_shape_internal` has no early exit
  (src/hb-ot-shape.cc:1171-1203) and `_hb_ot_shape` answers true (:1205-1214), so a buffer that failed in one stage goes through the
  rest, each stage doing what its loops do with `successful` false. The normaliser, entered with a failed buffer or failing inside its
  first round, still gives the glyphs of the first run of simple characters their glyph index in the glyphs of the input
  (`get_nominal_glyphs` writes `normalizer_glyph_index ()` before `next_glyphs`, which moves in place and cannot fail,
  src/hb-ot-shape-normalize.cc:343-346), `hb_ot_map_glyphs_fast` copies those indices to the glyphs, `all_simple` stays true until
  the loop has passed the first run (:322-367), the reorder round runs on the failed buffer whenever it is false (:372-401), and the
  glyphs a failed buffer holds are those of the input with the indices the first round wrote. The Indic, USE and Myanmar passes, the
  Arabic joining and the positioning stages likewise read and write the failed buffer.
- **Farfbuzz behaviour:** a run that has failed maps no glyph in the normaliser (`Normalize.decomposeSimple`, the decompose pass),
  computes `all_simple` from the text and not from how far the first round got, and skips the reorder round when the run has failed
  (`Normalize.finishRounds`); the passes of the other stages are written as the out-buffer machine and leave a failed run as the
  machine's `sync` does. The glyphs, clusters, advances and offsets a failed run holds differ from HarfBuzz's in the requests counted
  below, and the operation budget is spent differently by some stages before the lookups (Indic, Myanmar, USE).
- **Reason:** found by the length-limit and budget differentials against HarfBuzz itself (`BudgetDiff`), which lower the limits
  through the oracle's shim `farfbuzz_oracle_buffer_set_limits`; through the public API the normaliser, the preprocess hooks and
  the reordering cannot reach a failure (their growth is at most a factor of eighteen of the text, against the limit of 256 times its
  length and 4,096 operations a glyph, both at least 65,536), so the differences are reached only by a caller that sets the limits of a
  `GlyphRun` itself or by a font that makes the lookups fail first: the lookups, which are where a font can reach a failure, are
  equal to HarfBuzz's (below). Making the normaliser leave the failed buffer as HarfBuzz does needs the index the failing step had
  reached, which the run's result does not carry.
- **Tests affected:** `Budget differentials`, one test for each subject and limit (24), each failing when more requests differ than
  the count recorded in `BudgetDiff.knownDifferences` (of 400 requests with limits 0 to 89 glyphs or budgets 0 to 6,000 operations):
  Arabic (Amiri) 2 and 17, Arabic (Nastaliq) 23 and 17, Balinese 6 and 34, Devanagari 1 and 34, Duployan 65 and 32, Hangul on
  AdobeBlank 1 and 10, Kannada 3 and 172, Khmer 0 and 13, Latin 3 and 15, Myanmar 0 and 52, Thai 2 and 15 (length limit, then
  operation budget); Hangul on the in-house font, Khmer and Myanmar on the length limit and Hangul on its budget agree. The public-API
  tests (a multiple substitution that expands a run past the limit, context recursion past the budget and past `HB_MAX_NESTING_LEVEL`,
  for GSUB and GPOS, 105 and 252 and 252 and 22 and 22 cases, glyphs, clusters, advances, offsets, flags and the success flag) and
  the skipped-glyph test (832 cases) agree with HarfBuzz in every case.
- **Revisit when:** the run result carries how far a pass got before it failed, or a font reaches one of these differences through
  the public limits.

### D-P10-1 — `HarfBuzzSharp.Language.Default` is the C locale and reads no locale
- **Upstream behaviour:** `hb_language_get_default` reads `setlocale (LC_CTYPE, NULL)` once per process (`src/hb-common.cc:384-396`); builds without `newlocale`/`uselocale` always answer `"C"` (`src/hb.hh:505-528`).
- **Farfbuzz behaviour:** `Language.Default` is `Language.cLocale`, `"c"`, and the facade passes the same value as the default language of segment-property guessing; the process locale and `CultureInfo.CurrentCulture` are never read. `new Language (CultureInfo)` uses `culture.TwoLetterISOLanguageName` exactly as HarfBuzzSharp does (`Language.cs:29-30` at SkiaSharp `v4.155.0`), so the invariant culture gives `"iv"`, as natively.
- **Reason:** Golden points 1 to 3: a pure function reads no process state. Measured by `P10.S03` in the test process: the 15.1.0 oracle's `hb_language_get_default ()` is `c`, equal to the facade's value.
- **Tests affected:** `P10.S03` (`Language.Default is the C locale and reads no culture`, `the oracle's default language is recorded`).
- **Revisit when:** a process that calls `setlocale` with a language must shape with that language by default.

### D-P10-2 — `HarfBuzzSharp.Language` equality is ordinal equality of the canonical name
- **Upstream behaviour:** `Language.Equals` compares the native handles, interned `hb_language_t` pointers (`Language.cs` at `v4.155.0`); the handle of the invalid language is NULL and `Name` is null.
- **Farfbuzz behaviour:** two languages are equal when their canonical names are ordinally equal; `Name` is null for the invalid language, as natively; `GetHashCode` is the hash of the name, as upstream.
- **Reason:** there is no interning table, and none is wanted (golden point 5).
- **Tests affected:** `P10.S03` (`equality is by canonical name and the invalid language has no name`).
- **Revisit when:** never expected.

### D-P10-3 — `HarfBuzzSharp.Blob` copies its bytes once; `AsSpan` and `AsStream` expose the copy
- **Upstream behaviour:** `Blob (IntPtr, int, MemoryMode, ReleaseDelegate)` wraps the caller's memory (`hb_blob_create`); `AsSpan` and `AsStream` read and write that memory; the release delegate runs when HarfBuzz drops its last reference.
- **Farfbuzz behaviour:** the constructor copies `length` bytes into a managed array and never reads the pointer again; `MemoryMode` is recorded and never acted on; `AsSpan` is a writable span over the copy, so a write no longer reaches the caller's memory; `AsStream` is read-only over the copy; the release delegate runs exactly once, at `Dispose` (at once for an empty blob, as `hb_blob_create` does), never from the finaliser.
- **Reason:** golden points 3 and 5: no native pointer is kept; a blob that outlives its memory cannot be read after release. A consumer that writes through `AsSpan` into memory it owns is the only case that changes.
- **Tests affected:** `P10.S04`.
- **Revisit when:** a consumer is found that depends on writing through a blob.

### D-P10-4 — `HarfBuzzSharp.NativeObject.Handle` is an opaque token, never a native pointer
- **Upstream behaviour:** `Handle` is the `hb_*_t*` pointer of the native object, zero after `Dispose`.
- **Farfbuzz behaviour:** `Handle` is `GCHandle.ToIntPtr` of a weak handle to the object, non-zero while the object lives, freed in `Dispose` and set to `IntPtr.Zero` there as `NativeObject` does; passing it to native code is meaningless.
- **Reason:** there is no native object.
- **Tests affected:** `tests/Farfbuzz.Diff/TestFonts.fs` (`coverage`, used by the shaping differentials of the Diff project, which run against the real package) reads `face.Handle` for the oracle's externs; the facade test project links `TestFonts.fs` for its font list only and never calls `coverage`.
- **Revisit when:** never expected.

### D-P10-5 — `HarfBuzzSharp.GlyphInfo` var fields are zero after shaping
- **Upstream behaviour:** `var1` and `var2` of `hb_glyph_info_t` carry shaper scratch state after `hb_shape_full` (`hb-ot-layout.hh` glyph properties, syllables).
- **Farfbuzz behaviour:** shaper properties live in `GlyphRun.Props`; the infos the facade copies back into the buffer have `Var1 = Var2 = 0`.
- **Reason:** golden points 1 and 2 (phase 3 keeps the properties out of the infos); asked of this phase by `phase-3-buffer-unicode.md`.
- **Tests affected:** `GlyphInfo.Equals` and `GetHashCode` (which compare `var1` and `var2`) can differ from native after shaping; no test compares them.
- **Revisit when:** a consumer is found that reads `var1` or `var2`.

### D-P10-6 — `Font.Shape` ignores the Verify buffer flag and reports unported outlines as an exception
- **Upstream behaviour:** `hb_shape_full` with `HB_BUFFER_FLAG_VERIFY` shapes a second time and returns false when the results disagree (`hb-shape.cc:136-142, 150-160`); HarfBuzzSharp ignores the result. Shaping needs no outline.
- **Farfbuzz behaviour:** the facade shapes with `Shape.shapeWithPlan`, which does not verify, so the flag has no effect on the glyphs; when the core answers `OutlinesNotPorted` (a glyph outline it cannot read yet) or no shaper of the list runs, the shell throws `NotSupportedException` with the reason instead of returning a result that does not exist. A shaper list in which no name is supported leaves the buffer unchanged, as `hb_shape_full` does.
- **Reason:** the verification is a diagnostic of HarfBuzz's own tests, and the typed error of the core has no native counterpart.
- **Tests affected:** none (`P10.S08` compares the facade with the core on 5 584 runs of the three suites, Verify included in their flags).
- **Revisit when:** the P10.S17 equivalence run shows a case that needs either.

### D-P10-7 — A table-callback face asks for its outline tables at the first shape or extents query
- **Upstream behaviour:** `hb_face_create_for_tables` loads each table through the callback the first time the shaper or a query needs it (`hb-face.cc:130`, `hb_face_reference_table`); a Latin line that needs no glyph outline never asks for `glyf`.
- **Farfbuzz behaviour:** the shell fetches `Faces.ShapingTags` once when the face is frozen and `Faces.OutlineTags` (`glyf`, `loca`, `gvar`, `CFF `, `CFF2`, `sbix`, `CBDT`, `CBLC`, `COLR`, `VARC`) once, when the first `Shape` or glyph query is made on a font of the face; the core reads a table only from the face it was given and has no lazy table loading, and fallback mark positioning, space fallback, vertical origins and colour fonts read glyph extents during shaping (39 of the 5 584 suite runs differed without the outline tables).
- **Reason:** shaping must equal the blob-face result; making a `Font` still asks the callback for nothing, and a callback is called at most once per tag.
- **Tests affected:** `P10.S08` (`a face made from a table delegate shapes every suite run as the face made from the file does`).
- **Revisit when:** the core gets lazy table loading, so that only a line that needs extents fetches them.

### D-P10-8 — Replaceable font functions: reference route, no fast kernels, and limits
- **Upstream behaviour:** every shaper reaches the font only through `hb_font_funcs_t` callbacks; a sub-font delegates unset callbacks to its parent with scale conversion (`hb-font.cc:2195`).
- **Farfbuzz behaviour:** `Font.Funcs` (`FontFuncs voption`); `ValueNone` is the OpenType default and every default fast path (SimpleRun, GeneralLine, the lean normaliser, the font memo and advance plans) is unchanged. A font with functions answers `Font.nominalGlyph`, `variationGlyph`, `hAdvance`, `vAdvance`, `hAdvances`, `vAdvances`, `hAdvancePositions`, `hExtents`, `vExtents`, `glyphName`, `glyphFromName`, `Extents.glyph`, `Origins.vOrigins` and the Hangul presence test through the record, and `Execute` sends it down the general reference route (`runGeneral`) so that no kernel reads a table behind the record's back. `FontFuncsOf.subFont` delegates to the parent with scale conversion. The record's `HOrigin`, `HKerning` and `ContourPoint` fields exist but no core code asks them (the default route never did); the defaults give `0,0`, `0` and none.
- **Reason:** golden points 2 and 4; the speed of the default route is kept.
- **Tests affected:** `P10.S12` (4 tests; 5 850 suite runs identical with a record that wraps the defaults).
- **Revisit when:** a kernel must serve fonts with functions; the Hangul owned routes and the document line shaper (`Document.shapeLines`) still read tables directly and are not routed.

### D-P10-9 — Replaceable Unicode functions: reference route, plan overlay, and limits
- **Upstream behaviour:** `hb_buffer_t::unicode` is read wherever a shaper asks a code point's general category, combining class, mirror, script, composition or decomposition (`hb-ot-shape.cc`, `hb-ot-shape-normalize.cc`, the shapers' compose and decompose hooks, `hb_buffer_guess_segment_properties`); `hb_buffer_get_unicode_funcs` returns the object the buffer holds.
- **Farfbuzz behaviour:** `OtShapePlan.Unicode` (`UnicodeFuncs voption`) carries the functions of the buffer, put there at execution by `Execute.withUnicode` (the cached plan is never changed). `ValueNone` is the default `hb-ucd` set and every default fast path is unchanged; a plan with functions takes the general route only, as a font with functions does. The packed properties of every glyph (`OtProps.setUnicodePropsWith`, the dotted circle, the normaliser's parts and composites), the mirror of `rotateChars`, the hooks of the Hebrew, Khmer, Indic and USE shapers, the Arabic context categories and the guess of the script read the record. `Buffer.UnicodeFunctions` returns the managed object that was set, not a new wrapper over a handle each time (upstream's getter makes one that releases the native object it does not own when disposed); a value whose handle is zero sets the default, as `hb_buffer_set_unicode_funcs` does with a null pointer.
- **Limits, stated:** the fallback shaper does not read the functions; `decompose_compatibility` is not replaceable (HarfBuzzSharp does not expose it); a combining class above 255 is cut to its low byte and a general category past the last value to the last, as the packed properties are.
- **Reason:** golden points 2 and 4; the default route keeps its speed.
- **Tests affected:** `P10.S15` (8 tests; the default functions equal the oracle on every code point of U+0000 to U+10FFFF; delegates that ask the defaults reproduce default shaping on 200 suite runs).
- **Revisit when:** a kernel must serve buffers with Unicode functions.

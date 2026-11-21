namespace Farfbuzz

open System.Collections.Immutable

/// <summary>The shapers the ot pipeline recognizes, the <c>hb_ot_shaper_t</c> instances.</summary>
[<Struct>]
type internal ShaperKind =
    | DefaultShaper
    | DumberShaper
    | ArabicShaper
    | ThaiShaper
    | HangulShaper
    | HebrewShaper
    | IndicShaper
    | KhmerShaper
    | MyanmarShaper
    | MyanmarZawgyiShaper
    | UseShaper

/// <summary>When a shaper zeroes the advances of zero-width marks, <c>hb_ot_shape_zero_width_marks_type_t</c>.</summary>
[<Struct>]
type internal ZeroWidthMarks =
    | ZeroWidthMarksNone
    | ZeroWidthMarksByGdefEarly
    | ZeroWidthMarksByGdefLate

/// <summary>The normalization mode of a shaper, <c>hb_ot_shape_normalization_mode_t</c>.</summary>
[<Struct>]
type internal NormalizationMode =
    | NormalizationNone
    | NormalizationDecomposed
    | NormalizationComposedDiacritics
    | NormalizationComposedDiacriticsNoShortCircuit
    | NormalizationAuto

/// <summary>The fixed data part of a shaper vtable, the tail members of <c>hb_ot_shaper_t</c>.</summary>
[<Struct>]
type internal ShaperProfile =
    { Kind: ShaperKind
      GposTag: Tag
      Normalization: NormalizationMode
      ZeroWidthMarks: ZeroWidthMarks
      FallbackPosition: bool }

/// <summary>Which OpenType and AAT machinery a face provides, gathered once per plan.</summary>
[<Struct>]
type internal PlanCapabilities =
    { HasGlyphClasses: bool
      HasGsub: bool
      HasGpos: bool
      HasKern: bool
      HasMachineKern: bool
      HasCrossKern: bool
      HasMorx: bool
      HasKerx: bool
      HasTrak: bool
      HasStat: bool }

/// <summary>A pause between two stages of a table, the <c>pause_func_t</c> of <c>hb_ot_map_t::stage_map_t</c>: one case per
/// function a shaper registers with <c>add_gsub_pause</c>, dispatched by <c>ShaperHooks.applyPause</c>.</summary>
[<Struct>]
type internal PauseKind =
    | NoPause
    | ClearSyllables
    | KhmerSetupSyllables
    | KhmerReorder
    | IndicSetupSyllables
    | IndicInitialReordering
    | IndicFinalReordering
    | ClearSubstitutionFlags
    | UseSetupSyllables
    | UseRecordRphf
    | UseRecordPref
    | UseReorder
    | MyanmarSetupSyllables
    | MyanmarReorder
    | ArabicRecordStch
    | ArabicFallbackShape
    | ArabicReleaseAction

/// <summary>What the pipeline needs to know of a pause.</summary>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal PauseKind =
    /// <summary>Whether the pause runs a function over the run, which splits the lookups of its stage from the next:
    /// a pause with no function, or one that changes nothing here (<c>hb_syllabic_clear_var</c> only releases a buffer
    /// variable), leaves the lookups on both sides to one pass of the kernel.</summary>
    /// <param name="pause">The pause.</param>
    /// <returns>True when the pause runs a function.</returns>
    let runsFunction pause =
        match pause with
        | NoPause -> false
        | ClearSyllables -> false
        | KhmerSetupSyllables -> true
        | KhmerReorder -> true
        | IndicSetupSyllables -> true
        | IndicInitialReordering -> true
        | IndicFinalReordering -> true
        | ClearSubstitutionFlags -> true
        | UseSetupSyllables -> true
        | UseRecordRphf -> true
        | UseRecordPref -> true
        | UseReorder -> true
        | MyanmarSetupSyllables -> true
        | MyanmarReorder -> true
        | ArabicRecordStch -> true
        | ArabicFallbackShape -> true
        | ArabicReleaseAction -> false

/// <summary>One feature of the compiled OpenType map, <c>hb_ot_map_t::feature_map_t</c>.</summary>
[<Struct>]
type internal FeatureMap =
    { Tag: Tag
      GsubIndex: uint32
      GposIndex: uint32
      GsubStage: int
      GposStage: int
      Shift: int
      Mask: uint32
      Mask1: uint32
      NeedsFallback: bool
      AutoZwnj: bool
      AutoZwj: bool
      Random: bool
      PerSyllable: bool }

/// <summary>One lookup of the compiled OpenType map, <c>hb_ot_map_t::lookup_map_t</c>.</summary>
[<Struct>]
type internal LookupMap =
    { LookupIndex: int
      AutoZwnj: bool
      AutoZwj: bool
      Random: bool
      PerSyllable: bool
      Mask: uint32
      FeatureTag: Tag }

/// <summary>One stage of the compiled OpenType map, <c>hb_ot_map_t::stage_map_t</c>.</summary>
[<Struct>]
type internal StageMap = { LastLookup: int; Pause: PauseKind }

/// <summary>The compiled OpenType feature map, <c>hb_ot_map_t</c>.</summary>
type internal OtMap =
    { GlobalMask: uint32
      Features: ImmutableArray<FeatureMap>
      GsubLookups: ImmutableArray<LookupMap>
      GposLookups: ImmutableArray<LookupMap>
      GsubStages: ImmutableArray<StageMap>
      GposStages: ImmutableArray<StageMap>
      HasPauses: bool  // some GSUB stage ends in a pause that runs a function, fixed when the map is compiled
      GsubViews: Memo<obj>  // the GSUB lookups of the map kept ready for the kernel, built the first time a line applies them
      ChosenGsubScript: Tag
      ChosenGposScript: Tag
      FoundGsubScript: bool
      FoundGposScript: bool }

/// <summary>The masks of the Hangul shaper's plan data, <c>hangul_shape_plan_t</c>: the <c>ljmo</c>, <c>vjmo</c> and
/// <c>tjmo</c> feature masks and the mask of <c>calt</c>.</summary>
type internal HangulPlan =
    { Ljmo: uint32
      Vjmo: uint32
      Tjmo: uint32
      Calt: uint32 }

/// <summary>The masks of the Khmer shaper's plan data, <c>khmer_shape_plan_t</c>: the basic features applied per
/// syllable; the features applied globally have no mask of their own.</summary>
type internal KhmerPlan =
    { Pref: uint32
      Blwf: uint32
      Abvf: uint32
      Pstf: uint32
      Cfar: uint32 }

/// <summary>The Arabic shaper's plan data, <c>arabic_shape_plan_t</c>: the mask of the feature each joining action selects (the
/// last, for no action, is zero), whether the map has <c>stch</c>, and whether the fallback shaping applies; the fallback plan is
/// built from the font when a pause needs it.</summary>
type internal ArabicPlan =
    { MaskArray: ImmutableArray<uint32>
      HasStch: bool
      DoFallback: bool }

/// <summary>Where the reph goes in a syllable, <c>reph_position_t</c> (src/hb-ot-shaper-indic.cc:121-127).</summary>
[<Struct>]
type internal IndicRephPosition =
    | RephAfterMain
    | RephBeforeSub
    | RephAfterSub
    | RephBeforePost
    | RephAfterPost

/// <summary>How the reph is encoded, <c>reph_mode_t</c> (src/hb-ot-shaper-indic.cc:128-132).</summary>
[<Struct>]
type internal IndicRephMode =
    | RephImplicit  // formed out of an initial Ra, H sequence
    | RephExplicit  // formed out of an initial Ra, H, ZWJ sequence
    | RephLogRepha  // an encoded repha character, which needs reordering

/// <summary>Which consonants the below-forms feature applies to, <c>blwf_mode_t</c> (src/hb-ot-shaper-indic.cc:133-136).</summary>
[<Struct>]
type internal IndicBlwfMode =
    | BlwfPreAndPost
    | BlwfPostOnly

/// <summary>The per-script configuration of the Indic shaper, <c>indic_config_t</c> (src/hb-ot-shaper-indic.cc:137-146).</summary>
[<Struct>]
type internal IndicConfig =
    { HasOldSpec: bool
      Virama: uint32
      RephPosition: IndicRephPosition
      RephMode: IndicRephMode
      BlwfMode: IndicBlwfMode }

/// <summary>The lookups of the stage of a feature and whether they match with no context,
/// <c>hb_indic_would_substitute_feature_t</c> (src/hb-ot-shaper-indic.cc:90-116). <c>Prepared</c> is the cell of the tester
/// built from them for a face the first time the feature is asked about it, and never changed afterwards.</summary>
type internal IndicWouldSubstitute =
    { Lookups: ImmutableArray<int>
      ZeroContext: bool
      Prepared: Memo<obj> }

/// <summary>The Indic shaper's plan data, <c>indic_shape_plan_t</c> (src/hb-ot-shaper-indic.cc:260-282): the configuration of the
/// script, whether the font is old-spec, the lookups that decide where a consonant goes, and the masks of the features
/// applied by the reordering; the features applied globally have no mask. The virama glyph, which upstream loads lazily
/// from the font, is read from the cmap when it is used. <c>Positions</c> is the cell of the consonant positions the
/// font gave, kept for a face and a virama from the first time they are asked for.</summary>
type internal IndicPlan =
    { Config: IndicConfig
      IsOldSpec: bool
      RphfFeature: IndicWouldSubstitute
      PrefFeature: IndicWouldSubstitute
      BlwfFeature: IndicWouldSubstitute
      PstfFeature: IndicWouldSubstitute
      VatuFeature: IndicWouldSubstitute
      Positions: Memo<obj>
      RphfMask: uint32
      PrefMask: uint32
      BlwfMask: uint32
      AbvfMask: uint32
      HalfMask: uint32
      PstfMask: uint32
      InitMask: uint32 }

/// <summary>The universal shaping engine's plan data, <c>use_shape_plan_t</c> (src/hb-ot-shaper-use.cc:148-175): the mask of
/// <c>rphf</c>, the masks of the topographical features in the order <c>isol</c>, <c>init</c>, <c>medi</c>, <c>fina</c> (zero for
/// one that is global), and, for a script that joins, its <c>arabic_plan</c>.</summary>
type internal UsePlan =
    { RphfMask: uint32
      TopographicalMasks: ImmutableArray<uint32>
      Arabic: ArabicPlan voption }

/// <summary>What a shaper's <c>data_create</c> builds from the compiled plan, the <c>data</c> of <c>hb_ot_shape_plan_t</c>:
/// an immutable value made when the plan is, with nothing to destroy. The Hangul, Khmer, Indic, USE and Arabic shapers
/// have plan data; the others have none.</summary>
type internal ShaperData =
    | NoShaperData
    | HangulData of hangul: HangulPlan
    | KhmerData of khmer: KhmerPlan
    | IndicData of indic: IndicPlan
    | UseData of universal: UsePlan
    | ArabicData of arabic: ArabicPlan

/// <summary>The plan of one ot shaping run, <c>hb_ot_shape_plan_t</c>.</summary>
type internal OtShapePlan =
    { Props: SegmentProperties
      Shaper: ShaperProfile
      Map: OtMap
      Data: ShaperData
      UserFeatures: ImmutableArray<Feature>
      FracMask: uint32
      NumrMask: uint32
      DnomMask: uint32
      RtlmMask: uint32
      KernMask: uint32
      RequestedKerning: bool
      HasFrac: bool
      HasVert: bool
      HasGposMark: bool
      ZeroMarks: bool
      FallbackGlyphClasses: bool
      FallbackMarkPositioning: bool
      AdjustMarkPositioningWhenZeroing: bool
      ApplyGpos: bool
      ApplyKern: bool
      ApplyFallbackKern: bool
      ApplyKerx: bool
      ApplyMorx: bool
      ApplyTrak: bool
      Unicode: UnicodeFuncs voption }

/// <summary>A glyph-outline source only the outlines phase can read; this phase fails on it.</summary>
[<Struct>]
type internal OutlineSource =
    | ColrOutlines
    | VarcOutlines
    | SyntheticOutlines

/// <summary>A verify check that failed, one of the failure kinds of <c>hb_buffer_diff_flags_t</c>.</summary>
[<Struct>]
type internal VerifyCheck =
    | NotMonotone
    | UnsafeToBreakRangeInvalid
    | UnsafeToBreakFailed
    | UnsafeToConcatFailed

/// <summary>Why shaping did not produce glyphs.</summary>
type ShapeError =
    internal
        | OutlinesNotPorted of source: OutlineSource
        | NoShaperSucceeded of requested: ImmutableArray<string>
        | VerifyFailed of check: VerifyCheck * reconstruction: GlyphRun

/// <summary>The GSUB lookups between two pauses of a plan, cut out of its map once with the table that says which of them cover each
/// glyph, so that a line applies a window without building either.</summary>
type internal GsubWindow =
    { WindowLookups: ImmutableArray<LookupMap>
      WindowStages: ImmutableArray<StageMap>  // one stage holding all the lookups of the window
      WindowGlyphLookups: ImmutableArray<uint64>  // for each glyph, a bit per position of the first 64 lookups of the window whose subtables cover it
      WindowViews: Memo<obj> }  // the lookups of the window kept ready for the kernel, built the first time a line applies them

/// <summary>The plan of one shaping run, shared by the ot and fallback shapers.</summary>
type ShapePlan =
    internal
        { ShaperName: string
          Ot: OtShapePlan voption
          GsubCovered: ImmutableArray<uint64>  // the glyph set some GSUB subtable that can apply covers, built with the plan
          GposCovered: ImmutableArray<uint64>  // the same for the GPOS subtables
          GsubLookupCovered: ImmutableArray<ImmutableArray<uint64>>  // the same for each GSUB lookup, by index in the table
          GsubGlyphLookups: ImmutableArray<uint64>  // for each glyph, a bit per position of the first 64 lookups of the map whose subtables cover it
          GsubWindows: ImmutableArray<GsubWindow>  // the lookups between the pauses of the map that run a function, then those after the last
          GposLookupCovered: ImmutableArray<ImmutableArray<uint64>>  // the same for each GPOS lookup, by index in the table
          GposGlyphLookups: ImmutableArray<uint64>  // the same for the first 64 GPOS lookups of the map
          GposPairs: PairProgram }  // the GPOS lookups made of pair adjustments, compiled for the pair loops

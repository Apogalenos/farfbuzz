namespace Farfbuzz

open System
open System.Collections.Immutable
open Farfbuzz.OpenType
open Farfbuzz.Var

/// <summary>Which outline tables a face has, read once, so a glyph's extents need not look for bitmap and colour tables.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal OutlinePlan =
    { Plain: bool  // no bitmap strikes and no COLR or VARC table: the outlines alone answer
      HasCff1: bool
      HasCff2: bool }

/// <summary>The lazily built OpenType accelerators of one face, the <c>hb_ot_face_t</c> of upstream.</summary>
[<NoEquality; NoComparison>]
type internal OtFace =
    { Face: Face
      Hhea: Lazy<Hea>
      Vhea: Lazy<Hea>
      Os2: Lazy<Os2>
      Cmap: Lazy<CmapAccel>
      Post: Lazy<PostAccel>
      Cff1: Lazy<Cff1Accel>
      Cff2: Lazy<Cff2Accel>
      Hmtx: Lazy<Mtx>
      Vmtx: Lazy<Mtx>
      Vorg: Lazy<Vorg>
      Glyf: Lazy<GlyfHeaders>
      GlyfPoints: Lazy<GlyfFace>
      Gvar: Lazy<Gvar>
      Sbix: Lazy<Sbix>
      Cbdt: Lazy<Cbdt>
      Colr: Lazy<Colr>
      Outlines: Lazy<OutlinePlan>
      Gdef: Lazy<Gdef>
      Gsub: Lazy<LayoutTable<SubstSubtable>>
      Gpos: Lazy<LayoutTable<PosSubtable>>
      GsubLookupSets: Lazy<ImmutableArray<Lazy<ImmutableArray<uint64>>>>  // the glyph set of each GSUB lookup, built on first use
      GposLookupSets: Lazy<ImmutableArray<Lazy<ImmutableArray<uint64>>>>  // the same for GPOS
      GposPairTables: Lazy<ImmutableArray<Lazy<ImmutableArray<PairTable>>>>  // the compiled pair subtables of each GPOS lookup, built on first use
      GposPairTablesInert: Lazy<ImmutableArray<Lazy<ImmutableArray<PairTable>>>>  // the same for a font whose devices do nothing; the same arrays for a lookup whose records carry none
      GposPairTablesResolved: KeyedSlot<ResolveKey, Lazy<ImmutableArray<Lazy<ImmutableArray<PairTable>>>>>  // the same for a font that reads devices, for the newest coordinates and scales it was asked for
      HvarAdvances: KeyedSlot<ResolveKey, ImmutableArray<int>>  // the unscaled horizontal advance of every glyph for the newest coordinates asked for, HVAR deltas included
      GposTargetSets: Lazy<ImmutableArray<Lazy<ImmutableArray<uint64> voption>>>  // the glyphs each GPOS lookup of mark attachments attaches to, built on first use
      GposMarkTables: Lazy<ImmutableArray<Lazy<ImmutableArray<MarkTable voption>>>>  // the compiled mark subtables of each GPOS lookup, built on first use
      GsubChainSets: Lazy<ImmutableArray<Lazy<ImmutableArray<ChainSets voption>>>>  // the dense coverages of the chained-context subtables of each GSUB lookup, built on first use
      Kern: Lazy<KernTable>
      Variations: Lazy<FaceVariations>
      Feat: Lazy<Feat>
      Ltag: Lazy<Ltag>
      Morx: Lazy<Morx>
      Mort: Lazy<Morx>
      Trak: Lazy<Trak>
      Stat: Lazy<Stat>
      Kerx: Lazy<Kerx>
      Ankr: Lazy<Ankr> }

/// <summary>Building the OpenType accelerators of a face.</summary>
[<RequireQualifiedAccess>]
module internal OtFace =
    /// <summary>Creates the accelerators; each table is sanitized and read at its first use.</summary>
    /// <param name="face">The face.</param>
    /// <returns>The accelerators.</returns>
    let create (face: Face) : OtFace =
        let os2 = lazy (Os2.load face)
        let cff2 = lazy (Cff2.load face)
        let sbix = lazy (Sbix.load face)
        let cbdt = lazy (Cbdt.load face)
        let gsub = lazy (LayoutTable.load face Tags.Gsub (SubtableParser(GsubTables.parse)) GsubTables.subtableDigest GsubTables.secondDigest)
        let gpos = lazy (LayoutTable.load face Tags.Gpos (SubtableParser(GposTables.parse)) GposTables.subtableDigest GposTables.secondDigest)
        let pairTables = lazy (Sealed.init gpos.Value.Lookups.Length (fun lookupIndex -> lazy (PairTables.ofLookup gpos.Value.Lookups[lookupIndex].Value.Subtables)))
        { Face = face
          Hhea = lazy (Hhea.load face Tags.Hhea Tags.UpperHhea)
          Vhea = lazy (Hhea.load face Tags.Vhea Tags.UpperVhea)
          Os2 = os2
          Cmap = lazy (Cmap.load face os2.Value)
          Post = lazy (Post.load face)
          Cff1 = lazy (Cff1.load face)
          Cff2 = cff2
          Hmtx = lazy (Hmtx.load face true)
          Vmtx = lazy (Hmtx.load face false)
          Vorg = lazy (Vorg.load face)
          Glyf = lazy (GlyfHeader.load face)
          GlyfPoints = lazy (GlyfFace.load face)
          Gvar = lazy (Gvar.load face)
          Sbix = sbix
          Cbdt = cbdt
          Colr = lazy (Colr.load face)
          Outlines =
            lazy
                { Plain =
                    sbix.Value.Strikes.Length = 0
                    && cbdt.Value.Strikes.Length = 0
                    && ValueOption.isNone (Face.tryFindTable face Tags.Colr)
                    && ValueOption.isNone (Face.tryFindTable face Tags.Varc)
                  HasCff1 = ValueOption.isSome (Face.tryFindTable face Tags.Cff)
                  HasCff2 = ValueOption.isSome (Face.tryFindTable face Tags.Cff2) }
          Gdef = lazy (Gdef.load face)
          Gsub = gsub
          Gpos = gpos
          GsubLookupSets = lazy (LookupGlyphs.setsOf LookupGlyphs.subst gsub.Value)
          GposLookupSets = lazy (LookupGlyphs.setsOf LookupGlyphs.pos gpos.Value)
          GposPairTables = pairTables
          GposPairTablesInert = lazy (Sealed.init gpos.Value.Lookups.Length (fun lookupIndex -> lazy (match PairTables.anyDevices gpos.Value.Lookups[lookupIndex].Value.Subtables 0 with | true -> PairTables.ofLookupInert gpos.Value.Lookups[lookupIndex].Value.Subtables | false -> pairTables.Value[lookupIndex].Value)))
          GposPairTablesResolved = KeyedSlot<ResolveKey, Lazy<ImmutableArray<Lazy<ImmutableArray<PairTable>>>>>(ResolvedPairs.Capacity)
          HvarAdvances = KeyedSlot<ResolveKey, ImmutableArray<int>>(ResolvedPairs.Capacity)
          GposTargetSets = lazy (LookupGlyphs.targetSetsOf gpos.Value)
          GposMarkTables = lazy (Sealed.init gpos.Value.Lookups.Length (fun lookupIndex -> lazy (MarkTables.ofLookup gpos.Value.Lookups[lookupIndex].Value.Subtables)))
          GsubChainSets = lazy (Sealed.init gsub.Value.Lookups.Length (fun lookupIndex -> lazy (ChainSets.ofLookup gsub.Value.Lookups[lookupIndex].Value.Subtables)))
          Kern = lazy (KernTable.load face)
          Variations = lazy (FaceVariations.loadWith face cff2)
          Feat = lazy (Feat.load face)
          Ltag = lazy (Ltag.load face)
          Morx = lazy (Morx.load face false)
          Mort = lazy (Morx.load face true)
          Trak = lazy (Trak.load face)
          Stat = lazy (Stat.load face)
          Kerx = lazy (Kerx.load face)
          Ankr = lazy (Ankr.load face) }

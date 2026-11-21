namespace Farfbuzz.HarfBuzzSharp.Bridge

open System
open System.Collections.Immutable
open Farfbuzz

/// <summary>Faces for the HarfBuzzSharp shell (hb-face.cc).</summary>
[<AbstractClass; Sealed>]
type Faces =
    /// <summary>The number of faces in a file, <c>hb_face_count</c> (hb-face.cc:949-962).</summary>
    /// <param name="blob">The font file.</param>
    /// <returns>The face count, 0 when the file does not sanitize.</returns>
    static member Count (blob: ReadOnlyMemory<byte>) =
        Face.count blob

    /// <summary>The empty face, <c>hb_face_get_empty</c>: 1000 units per em and no glyphs.</summary>
    static member Empty =
        Face.empty

    /// <summary>The tables a face reads while it shapes: the metrics, the character map, the layout and variation tables and the AAT
    /// tables; the shell fetches exactly these from a table callback when it freezes a face.</summary>
    static member ShapingTags =
        ImmutableArray.Create<uint32>(
            uint32 Tags.Head, uint32 Tags.Maxp, uint32 Tags.UpperMaxp, uint32 Tags.Hhea, uint32 Tags.UpperHhea, uint32 Tags.Vhea,
            uint32 Tags.UpperVhea, uint32 Tags.Hmtx, uint32 Tags.UpperHmtx, uint32 Tags.Vmtx, uint32 Tags.UpperVmtx, uint32 Tags.Os2,
            uint32 Tags.Cmap, uint32 Tags.Post, uint32 Tags.Vorg, uint32 Tags.Fvar, uint32 Tags.Avar, uint32 Tags.Gdef, uint32 Tags.Gsub,
            uint32 Tags.Gpos, uint32 Tags.Hvar, uint32 Tags.Vvar, uint32 Tags.Mvar, uint32 Tags.Kern, uint32 Tags.Feat, uint32 Tags.Ltag,
            uint32 Tags.Morx, uint32 Tags.Mort, uint32 Tags.Trak, uint32 Tags.Stat, uint32 Tags.Kerx, uint32 Tags.Ankr)

    /// <summary>The tables only outlines, bitmaps and colour glyphs need; the shell fetches them on the first query that asks for extents,
    /// contour points or paint, never when it freezes a face.</summary>
    static member OutlineTags =
        ImmutableArray.Create<uint32>(
            uint32 Tags.Glyf, uint32 Tags.Loca, uint32 Tags.Gvar, uint32 Tags.UpperGvar, uint32 Tags.Cff, uint32 Tags.Cff2, uint32 Tags.Sbix,
            uint32 Tags.Cbdt, uint32 Tags.Cblc, uint32 Tags.Colr, uint32 Tags.Varc)

    /// <summary>The table tags of a face in directory order, <c>hb_face_get_table_tags</c>.</summary>
    /// <param name="face">The core face.</param>
    /// <returns>The tags.</returns>
    static member TableTags (face: Face) =
        ImmutableArray.CreateRange(Face.tableTags face, fun tag -> uint32 tag)

    /// <summary>A table's bytes, empty when absent, <c>hb_face_reference_table</c>.</summary>
    /// <param name="face">The core face.</param>
    /// <param name="tag">The table tag.</param>
    /// <returns>The slice of the face's file.</returns>
    static member Table (face: Face, tag: uint32) =
        Face.referenceTable face (LanguagePrimitives.UInt32WithMeasure tag)

    /// <summary>A face from a font file with the overrides of <c>hb_face_set_upem</c> and <c>hb_face_set_glyph_count</c> applied.</summary>
    /// <param name="blob">The font file.</param>
    /// <param name="index">The face index, the high 16 bits kept.</param>
    /// <param name="upem">The units per em; 0 reads them from the font.</param>
    /// <param name="glyphCount">The glyph count; -1 reads it from the font.</param>
    /// <returns>The face.</returns>
    static member OfBlob (blob: ReadOnlyMemory<byte>, index: int, upem: int, glyphCount: int) =
        Faces.WithOverrides (Face.create blob (uint32 index), upem, glyphCount)

    /// <summary>A face from table bytes, <c>hb_face_create_for_tables</c> (hb-face.cc:130), with the same overrides.</summary>
    /// <param name="tables">The tables by tag; an absent table is a table of no bytes.</param>
    /// <param name="index">The face index to report.</param>
    /// <param name="upem">The units per em; 0 reads them from <c>head</c>.</param>
    /// <param name="glyphCount">The glyph count; -1 reads it from <c>maxp</c>.</param>
    /// <returns>The face.</returns>
    static member OfTables (tables: ImmutableDictionary<uint32, ReadOnlyMemory<byte>>, index: int, upem: int, glyphCount: int) =
        let pairs = tables |> Seq.map (fun pair -> struct (LanguagePrimitives.UInt32WithMeasure<tag> pair.Key, pair.Value)) |> ImmutableArray.CreateRange
        Faces.WithOverrides (Face.ofTables (uint32 index) pairs, upem, glyphCount)

    /// <summary>A face made from the tables of another and more tables, for a table-callback face that fetches its outline tables once a query needs
    /// them; the units per em and glyph count of the first face are kept (hb-face.cc:130).</summary>
    /// <param name="face">The core face.</param>
    /// <param name="extra">The tables to add by tag; an empty table adds nothing.</param>
    /// <returns>The face with the tables added.</returns>
    static member WithTables (face: Face, extra: ImmutableDictionary<uint32, ReadOnlyMemory<byte>>) =
        let own = Face.tableTags face |> Seq.map (fun tag -> struct (tag, Face.referenceTable face tag))
        let added = extra |> Seq.map (fun pair -> struct (LanguagePrimitives.UInt32WithMeasure<tag> pair.Key, pair.Value))
        Faces.WithOverrides (Face.ofTables face.Index (Seq.append own added |> ImmutableArray.CreateRange), face.Upem, face.GlyphCount)

    /// <summary>The units per em of a face, <c>hb_face_get_upem</c>.</summary>
    /// <param name="face">The core face.</param>
    /// <returns>The units per em.</returns>
    static member Upem (face: Face) =
        face.Upem

    /// <summary>The glyph count of a face, <c>hb_face_get_glyph_count</c>.</summary>
    /// <param name="face">The core face.</param>
    /// <returns>The glyph count.</returns>
    static member GlyphCount (face: Face) =
        face.GlyphCount

    /// <summary>Applies the overrides a shell recorded before it froze a face: a units per em above 0 and a glyph count of 0 or more replace
    /// the font's own values, <c>hb_face_set_upem</c> and <c>hb_face_set_glyph_count</c> (hb-face.cc:836-870).</summary>
    /// <param name="face">The face.</param>
    /// <param name="upem">The units per em override, 0 for none.</param>
    /// <param name="glyphCount">The glyph count override, negative for none.</param>
    /// <returns>The face.</returns>
    static member WithOverrides (face: Face, upem: int, glyphCount: int) =
        let sized = if upem > 0 then Face.withUpem upem face else face
        if glyphCount >= 0 then { sized with GlyphCount = glyphCount; Accelerators = AccelSlot() } else sized

namespace Farfbuzz

open System.Collections.Immutable

/// <summary>The compiled mark attachment subtables of a lookup, built from the parsed subtables.</summary>
[<RequireQualifiedAccess>]
module internal MarkTables =
    /// <summary>Whether an anchor is a plain one or a null one, which a compiled table holds as two numbers.</summary>
    /// <param name="anchor">The anchor.</param>
    /// <returns>False for an anchor with devices.</returns>
    let private isPlain anchor =
        match anchor with
        | NoAnchor -> true
        | PlainAnchor _ -> true
        | DeviceAnchor _ -> false

    /// <summary>The x of an anchor, zero for a null one.</summary>
    /// <param name="anchor">The anchor.</param>
    /// <returns>The coordinate.</returns>
    let private anchorX anchor =
        match anchor with
        | PlainAnchor (x, _) -> x
        | NoAnchor -> 0
        | DeviceAnchor (x, _, _, _) -> x

    /// <summary>The y of an anchor, zero for a null one.</summary>
    /// <param name="anchor">The anchor.</param>
    /// <returns>The coordinate.</returns>
    let private anchorY anchor =
        match anchor with
        | PlainAnchor (_, y) -> y
        | NoAnchor -> 0
        | DeviceAnchor (_, y, _, _) -> y

    /// <summary>Whether an anchor is one a matrix entry may have, that is any but the null anchor.</summary>
    /// <param name="anchor">The anchor.</param>
    /// <returns>True for an anchor that is not null.</returns>
    let private isKnown anchor =
        match anchor with
        | NoAnchor -> false
        | PlainAnchor _ -> true
        | DeviceAnchor _ -> true

    /// <summary>Compiles a mark-to-base or mark-to-mark subtable.</summary>
    /// <param name="attachment">The subtable.</param>
    /// <returns>The compiled table, or none when an anchor has a device.</returns>
    let private compile (attachment: MarkAttachment) =
        match Seq.forall isPlain attachment.MarkAnchors && Seq.forall isPlain attachment.Anchors with
        | false -> ValueNone
        | true ->
            ValueSome
                { Source = attachment
                  MarkX = Sealed.map anchorX (attachment.MarkAnchors.AsSpan())
                  MarkY = Sealed.map anchorY (attachment.MarkAnchors.AsSpan())
                  AnchorX = Sealed.map anchorX (attachment.Anchors.AsSpan())
                  AnchorY = Sealed.map anchorY (attachment.Anchors.AsSpan())
                  AnchorKnown = Sealed.map isKnown (attachment.Anchors.AsSpan()) }

    /// <summary>Compiles one subtable of a lookup: a mark-to-base or mark-to-mark subtable, none for any other.</summary>
    /// <param name="subtable">The subtable.</param>
    /// <returns>The compiled table, or none.</returns>
    let ofSubtable (subtable: SubtableAccel<PosSubtable>) =
        match subtable.Subtable with
        | MarkBasePos attachment -> compile attachment
        | MarkMarkPos attachment -> compile attachment
        | SinglePos _ -> ValueNone
        | PairPosGlyphs _ -> ValueNone
        | PairPosClasses _ -> ValueNone
        | CursivePos _ -> ValueNone
        | MarkLigPos _ -> ValueNone
        | PosContext _ -> ValueNone
        | PosChainContext _ -> ValueNone
        | PosPending _ -> ValueNone

    /// <summary>Compiles the subtables of a lookup, each its own entry in the order of the lookup.</summary>
    /// <param name="subtables">The lookup's subtables.</param>
    /// <returns>The compiled table of each subtable, none for one that is not a mark subtable or has a device anchor.</returns>
    let ofLookup (subtables: ImmutableArray<SubtableAccel<PosSubtable>>) =
        Sealed.map ofSubtable (subtables.AsSpan())

namespace Farfbuzz

open System.Collections.Immutable

/// <summary>One statement of a generated action body (e.g. src/hb-ot-shaper-indic-machine.hh:1176-1271).</summary>
[<Struct>]
type internal RagelOp =
    | TsFromP // ts = p
    | TsReset // ts = 0
    | TeFromNextP // te = p+1
    | TeFromP // te = p
    | PBack // p--
    | PFromTe // p = ((te))-1
    | ActSet of act: byte // act = N
    | FoundSyllable of syllableType: byte * broken: bool // found_syllable, with the HAS_BROKEN_SYLLABLE flag

/// <summary>A numbered action list; ActSwitch is <c>switch (act)</c> indexed by act (empty array = no case).</summary>
type internal RagelAction =
    | NoAction
    | Ops of ops: ImmutableArray<RagelOp>
    | ActSwitch of byAct: ImmutableArray<ImmutableArray<RagelOp>>

/// <summary>A Ragel 6.10 <c>-e -F1</c> scanner (src/gen-ragel-artifacts.py:22) as emitted for one machine.</summary>
type internal RagelMachine =
    { TransKeys: ImmutableArray<byte>
      KeySpans: ImmutableArray<byte>
      IndexOffsets: ImmutableArray<uint16>
      Indicies: ImmutableArray<byte>
      TransTargs: ImmutableArray<byte>
      TransActions: ImmutableArray<byte>
      SetsTsOnEntry: ImmutableArray<bool>
      ResetsTsOnExit: ImmutableArray<bool>
      EofTrans: ImmutableArray<uint16>
      Actions: ImmutableArray<RagelAction>
      Start: int }

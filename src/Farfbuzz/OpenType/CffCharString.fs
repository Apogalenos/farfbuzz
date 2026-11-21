namespace Farfbuzz

open System

/// <summary>What a charstring does to a sink; implemented by a struct, never by a closure.</summary>
type internal ICharStringSink<'S> =
    /// <summary>A move-to: the path closes and the current point becomes <paramref name="x"/>.<paramref name="y"/>.</summary>
    /// <param name="state">The sink state.</param>
    /// <param name="x">The new point's x.</param>
    /// <param name="y">The new point's y.</param>
    /// <returns>The next sink state.</returns>
    abstract MoveTo : state: 'S * x: float * y: float -> 'S

    /// <summary>A line from the current point to a new point.</summary>
    /// <param name="state">The sink state.</param>
    /// <param name="fromX">The current point's x.</param>
    /// <param name="fromY">The current point's y.</param>
    /// <param name="x">The new point's x.</param>
    /// <param name="y">The new point's y.</param>
    /// <returns>The next sink state.</returns>
    abstract LineTo : state: 'S * fromX: float * fromY: float * x: float * y: float -> 'S

    /// <summary>A cubic curve from the current point through two control points to a new point.</summary>
    /// <param name="state">The sink state.</param>
    /// <param name="fromX">The current point's x.</param>
    /// <param name="fromY">The current point's y.</param>
    /// <param name="x1">The first control point's x.</param>
    /// <param name="y1">The first control point's y.</param>
    /// <param name="x2">The second control point's x.</param>
    /// <param name="y2">The second control point's y.</param>
    /// <param name="x3">The new point's x.</param>
    /// <param name="y3">The new point's y.</param>
    /// <returns>The next sink state.</returns>
    abstract CurveTo :
        state: 'S * fromX: float * fromY: float * x1: float * y1: float * x2: float * y2: float * x3: float * y3: float -> 'S

/// <summary>What CFF2 blending needs from a blend context; implemented by a struct so the interpreter stays generic and
/// allocation-free.</summary>
type internal ICharStringBlend<'B> =
    /// <summary>Whether blending is on, <c>do_blend</c>.</summary>
    /// <param name="blend">The blend context.</param>
    /// <returns>True when the deltas are added to their defaults.</returns>
    abstract Blending : blend: 'B -> bool

    /// <summary>The region count of a data set, <c>get_region_index_count</c>; 0 past the data sets.</summary>
    /// <param name="blend">The blend context.</param>
    /// <param name="ivs">The data set number.</param>
    /// <returns>The region count.</returns>
    abstract RegionCount : blend: 'B * ivs: int -> int

    /// <summary>One region scalar of a data set, <c>get_region_scalars</c>'s one value; 0 past the regions.</summary>
    /// <param name="blend">The blend context.</param>
    /// <param name="ivs">The data set number.</param>
    /// <param name="index">The column index.</param>
    /// <returns>The scalar.</returns>
    abstract Scalar : blend: 'B * ivs: int * index: int -> float32

    /// <summary>The font dict's private-dict data set number, <c>orig_ivs</c>.</summary>
    /// <param name="blend">The blend context.</param>
    /// <returns>The initial data set number.</returns>
    abstract Ivs : blend: 'B -> int

/// <summary>The charstring flavour: CFF1, or CFF2 with its blend context.</summary>
[<Struct>]
type internal CffFlavour<'B> =
    | Cff1
    | Cff2 of blend: 'B

/// <summary>The no-blend context of a CFF1 charstring: every blend operation is inert.</summary>
[<Struct>]
type internal Cff1Blend =
    | Cff1Blend
    interface ICharStringBlend<Cff1Blend> with
        member _.Blending _ = false
        member _.RegionCount (_, _) = 0
        member _.Scalar (_, _, _) = 0.0f
        member _.Ivs _ = 0
    interface ICffBlendSource<Cff1Blend> with
        member _.Blending _ = false
        member _.RegionCount (_, _) = 0
        member _.Scalar (_, _, _) = 0.0f

/// <summary>How a charstring ended: normally, or as a seac endchar whose base and accent are resolved by the caller.</summary>
[<Struct>]
type internal CharStringEnd =
    | Ended
    | EndedWithSeac of adx: float * ady: float * baseCode: int * accentCode: int

/// <summary>The CFF charstring interpreter, one tail-recursive loop over a struct state, generic over a struct sink.</summary>
[<RequireQualifiedAccess>]
module internal CffCharString =
    /// <summary>The operator-count limit, <c>HB_CFF_MAX_OPS</c>.</summary>
    [<Literal>]
    let MaxOps = 200000

    /// <summary>The call-stack limit, <c>kMaxCallLimit</c>.</summary>
    [<Literal>]
    let MaxCalls = 10

    /// <summary>The argument-stack limit, <c>cff_stack_t&lt;number_t, 513&gt;</c>.</summary>
    [<Literal>]
    let MaxArguments = 513

    /// <summary>The hstem operator, <c>OpCode_hstem</c>.</summary>
    [<Literal>]
    let private OpHstem = 1

    /// <summary>The vstem operator, <c>OpCode_vstem</c>.</summary>
    [<Literal>]
    let private OpVstem = 3

    /// <summary>The vmoveto operator, <c>OpCode_vmoveto</c>.</summary>
    [<Literal>]
    let private OpVmoveto = 4

    /// <summary>The rlineto operator, <c>OpCode_rlineto</c>.</summary>
    [<Literal>]
    let private OpRlineto = 5

    /// <summary>The hlineto operator, <c>OpCode_hlineto</c>.</summary>
    [<Literal>]
    let private OpHlineto = 6

    /// <summary>The vlineto operator, <c>OpCode_vlineto</c>.</summary>
    [<Literal>]
    let private OpVlineto = 7

    /// <summary>The rrcurveto operator, <c>OpCode_rrcurveto</c>.</summary>
    [<Literal>]
    let private OpRrcurveto = 8

    /// <summary>The callsubr operator, <c>OpCode_callsubr</c>.</summary>
    [<Literal>]
    let private OpCallsubr = 10

    /// <summary>The return operator, <c>OpCode_return</c>.</summary>
    [<Literal>]
    let private OpReturn = 11

    /// <summary>The escape operator, <c>OpCode_escape</c>.</summary>
    [<Literal>]
    let private OpEscape = 12

    /// <summary>The endchar operator, <c>OpCode_endchar</c>.</summary>
    [<Literal>]
    let private OpEndchar = 14

    /// <summary>The vsindex operator, <c>OpCode_vsindexcs</c>.</summary>
    [<Literal>]
    let private OpVsindex = 15

    /// <summary>The blend operator, <c>OpCode_blendcs</c>.</summary>
    [<Literal>]
    let private OpBlend = 16

    /// <summary>The hstemhm operator, <c>OpCode_hstemhm</c>.</summary>
    [<Literal>]
    let private OpHstemhm = 18

    /// <summary>The hintmask operator, <c>OpCode_hintmask</c>.</summary>
    [<Literal>]
    let private OpHintmask = 19

    /// <summary>The cntrmask operator, <c>OpCode_cntrmask</c>.</summary>
    [<Literal>]
    let private OpCntrmask = 20

    /// <summary>The rmoveto operator, <c>OpCode_rmoveto</c>.</summary>
    [<Literal>]
    let private OpRmoveto = 21

    /// <summary>The hmoveto operator, <c>OpCode_hmoveto</c>.</summary>
    [<Literal>]
    let private OpHmoveto = 22

    /// <summary>The vstemhm operator, <c>OpCode_vstemhm</c>.</summary>
    [<Literal>]
    let private OpVstemhm = 23

    /// <summary>The rcurveline operator, <c>OpCode_rcurveline</c>.</summary>
    [<Literal>]
    let private OpRcurveline = 24

    /// <summary>The rlinecurve operator, <c>OpCode_rlinecurve</c>.</summary>
    [<Literal>]
    let private OpRlinecurve = 25

    /// <summary>The vvcurveto operator, <c>OpCode_vvcurveto</c>.</summary>
    [<Literal>]
    let private OpVvcurveto = 26

    /// <summary>The hhcurveto operator, <c>OpCode_hhcurveto</c>.</summary>
    [<Literal>]
    let private OpHhcurveto = 27

    /// <summary>The shortint operator, <c>OpCode_shortint</c>.</summary>
    [<Literal>]
    let private OpShortint = 28

    /// <summary>The callgsubr operator, <c>OpCode_callgsubr</c>.</summary>
    [<Literal>]
    let private OpCallgsubr = 29

    /// <summary>The vhcurveto operator, <c>OpCode_vhcurveto</c>.</summary>
    [<Literal>]
    let private OpVhcurveto = 30

    /// <summary>The hvcurveto operator, <c>OpCode_hvcurveto</c>.</summary>
    [<Literal>]
    let private OpHvcurveto = 31

    /// <summary>The fixedcs operator, <c>OpCode_fixedcs</c>.</summary>
    [<Literal>]
    let private OpFixedcs = 255

    /// <summary>The dotsection operator, <c>OpCode_dotsection</c>.</summary>
    [<Literal>]
    let private OpDotsection = 256

    /// <summary>The hflex operator, <c>OpCode_hflex</c>.</summary>
    [<Literal>]
    let private OpHflex = 290

    /// <summary>The flex operator, <c>OpCode_flex</c>.</summary>
    [<Literal>]
    let private OpFlex = 291

    /// <summary>The hflex1 operator, <c>OpCode_hflex1</c>.</summary>
    [<Literal>]
    let private OpHflex1 = 292

    /// <summary>The flex1 operator, <c>OpCode_flex1</c>.</summary>
    [<Literal>]
    let private OpFlex1 = 293

    /// <summary>The invalid operator, <c>OpCode_invalid</c>.</summary>
    [<Literal>]
    let private OpInvalid = 0xFFFF

    /// <summary>The interpreter state, copied by value through the loop.</summary>
    [<Struct>]
    type private State =
        { StrStart: int
          StrLength: int
          StrOffset: int
          StrError: bool
          Args: CharArgStack
          Calls: CharCallStack
          PtX: float
          PtY: float
          Endchar: bool
          Ending: CharStringEnd
          SeenHintmask: bool
          HstemCount: int
          VstemCount: int
          HintmaskSize: int
          ProcessedWidth: bool
          MaxOps: int
          Budget: int64
          Ivs: int
          SeenVsindex: bool
          SeenBlend: bool
          RegionCount: int }

    /// <summary>Whether the interpreter is in error, <c>cs_interp_env_t::in_error</c>.</summary>
    /// <param name="state">The state.</param>
    /// <returns>True in error.</returns>
    let private inError (state: State) : bool =
        state.StrError || CharArgStack.inError state.Args || CharCallStack.inError state.Calls

    /// <summary>Reads one byte of the current string, <c>byte_str_ref_t::operator[]</c>.</summary>
    /// <param name="table">The CFF table bytes.</param>
    /// <param name="state">The state.</param>
    /// <param name="index">The byte index after the cursor.</param>
    /// <returns>The byte and whether the read ran past the string.</returns>
    let private readByte (table: ReadOnlySpan<byte>) (state: State) (index: int) : struct (byte * bool) =
        match state.StrOffset + index < state.StrLength with
        | true -> struct (Binary.u8 table (state.StrStart + state.StrOffset + index), false)
        | false -> struct (0uy, true)

    /// <summary>Advances the cursor, <c>byte_str_ref_t::inc</c>.</summary>
    /// <param name="state">The state.</param>
    /// <param name="count">The number of bytes.</param>
    /// <returns>The advanced state.</returns>
    let private inc (state: State) (count: int) : State =
        let offset = state.StrOffset + count
        { state with StrOffset = offset; StrError = state.StrError || offset > state.StrLength }

    /// <summary>Fetches the next operator, <c>interp_env_t::fetch_op</c>; CFF2 makes up an endchar or a return when the
    /// bytes run out.</summary>
    /// <param name="flavour">The flavour.</param>
    /// <param name="table">The CFF table bytes.</param>
    /// <param name="state">The state.</param>
    /// <returns>The operator and the advanced state.</returns>
    let private fetchOp (flavour: CffFlavour<'B>) (table: ReadOnlySpan<byte>) (state: State) : struct (int * State) =
        match state.StrOffset + 1 > state.StrLength with
        | false ->
            let opcode = int (Binary.u8 table (state.StrStart + state.StrOffset))
            let advanced = { state with StrOffset = state.StrOffset + 1 }
            match opcode with
            | OpEscape ->
                match advanced.StrOffset + 1 > advanced.StrLength with
                | true -> struct (OpInvalid, advanced)
                | false -> struct (256 + int (Binary.u8 table (advanced.StrStart + advanced.StrOffset)), { advanced with StrOffset = advanced.StrOffset + 1 })
            | _ -> struct (opcode, advanced)
        | true ->
            match flavour with
            | Cff1 -> struct (OpInvalid, state)
            | Cff2 _ ->
                match CharCallStack.getCount state.Calls with
                | 0 -> struct (OpEndchar, state)
                | _ -> struct (OpReturn, state)

    /// <summary>Converts a number to an int, saturating, <c>number_t::to_int</c>.</summary>
    /// <param name="value">The number.</param>
    /// <returns>The int.</returns>
    let private toIntClamped (value: float) : int =
        match value >= float Int32.MinValue && value <= float Int32.MaxValue with
        | true -> int value
        | false ->
            match value < 0.0 with
            | true -> Int32.MinValue
            | false -> match value > 0.0 with | true -> Int32.MaxValue | false -> 0

    /// <summary>Clamps the remaining budget to the operator limit, <c>hb_clamp (remaining, 0, HB_CFF_MAX_OPS)</c>.</summary>
    /// <param name="budget">The remaining budget.</param>
    /// <returns>The operator count.</returns>
    let private clampMaxOps (budget: int64) : int =
        match budget < 0L with
        | true -> 0
        | false -> match budget > int64 MaxOps with | true -> MaxOps | false -> int budget

    /// <summary>The subroutine bias, <c>biased_subrs_t</c>.</summary>
    /// <param name="count">The subroutine count.</param>
    /// <returns>The bias.</returns>
    let private subrBias (count: uint32) : int =
        match count < 1240u with
        | true -> 107
        | false -> match count < 33900u with | true -> 1131 | false -> 32768

    /// <summary>Records that the width operator was seen, <c>set_width</c>; CFF2 has no width so it does nothing.</summary>
    /// <param name="flavour">The flavour.</param>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private checkWidth (flavour: CffFlavour<'B>) (state: State) : State =
        match flavour with
        | Cff1 -> { state with ProcessedWidth = true }
        | Cff2 _ -> state

    /// <summary>Computes the hintmask byte count, <c>determine_hintmask_size</c>.</summary>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private determineHintmaskSize (state: State) : State =
        match state.SeenHintmask with
        | true -> state
        | false ->
            let vstemCount = state.VstemCount + CharArgStack.getCount state.Args / 2
            let hintmaskSize = (state.HstemCount + vstemCount + 7) >>> 3
            { state with VstemCount = vstemCount; HintmaskSize = hintmaskSize; SeenHintmask = true }

    /// <summary>Pushes a number operand, <c>opset_t::process_op</c>.</summary>
    /// <param name="table">The CFF table bytes.</param>
    /// <param name="opcode">The operator.</param>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private pushNumber (table: ReadOnlySpan<byte>) (opcode: int) (state: State) : State =
        match opcode with
        | OpShortint ->
            let struct (b0, e0) = readByte table state 0
            let struct (b1, e1) = readByte table state 1
            let value = int (int16 ((uint16 b0 <<< 8) ||| uint16 b1))
            inc { state with Args = CharArgStack.pushInt state.Args value; StrError = state.StrError || e0 || e1 } 2
        | 247 | 248 | 249 | 250 ->
            let struct (b0, e0) = readByte table state 0
            let value = (opcode - 247) * 256 + int b0 + 108
            inc { state with Args = CharArgStack.pushInt state.Args value; StrError = state.StrError || e0 } 1
        | 251 | 252 | 253 | 254 ->
            let struct (b0, e0) = readByte table state 0
            let value = -(opcode - 251) * 256 - int b0 - 108
            inc { state with Args = CharArgStack.pushInt state.Args value; StrError = state.StrError || e0 } 1
        | opcode when opcode >= 32 && opcode <= 246 ->
            { state with Args = CharArgStack.pushInt state.Args (opcode - 139) }
        | _ -> { state with Args = CharArgStack.clear state.Args }

    /// <summary>Applies one move-to through the sink.</summary>
    /// <param name="sink">The sink.</param>
    /// <param name="state">The state.</param>
    /// <param name="sinkState">The sink state.</param>
    /// <param name="dx">The x offset.</param>
    /// <param name="dy">The y offset.</param>
    /// <returns>The state and sink state.</returns>
    let private moveTo<'K, 'S when 'K : struct and 'K :> ICharStringSink<'S>> (sink: 'K) (state: State) (sinkState: 'S) (dx: float) (dy: float) : struct (State * 'S) =
        let x = state.PtX + dx
        let y = state.PtY + dy
        struct ({ state with PtX = x; PtY = y }, sink.MoveTo(sinkState, x, y))

    /// <summary>Applies one line through the sink, then flushes the arguments.</summary>
    /// <param name="sink">The sink.</param>
    /// <param name="state">The state.</param>
    /// <param name="sinkState">The sink state.</param>
    /// <param name="dx">The x offset.</param>
    /// <param name="dy">The y offset.</param>
    /// <returns>The state and sink state.</returns>
    let private lineTo<'K, 'S when 'K : struct and 'K :> ICharStringSink<'S>> (sink: 'K) (state: State) (sinkState: 'S) (dx: float) (dy: float) : struct (State * 'S) =
        let x = state.PtX + dx
        let y = state.PtY + dy
        struct ({ state with PtX = x; PtY = y }, sink.LineTo(sinkState, state.PtX, state.PtY, x, y))

    /// <summary>The rmoveto operator, <c>path_procs_t::rmoveto</c>.</summary>
    let private rMoveTo (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        let struct (args, dy) = CharArgStack.pop state.Args
        let struct (args, dx) = CharArgStack.pop args
        moveTo sink { state with Args = args } sinkState dx dy

    /// <summary>The hmoveto operator, <c>path_procs_t::hmoveto</c>.</summary>
    let private hMoveTo (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        let struct (args, dx) = CharArgStack.pop state.Args
        moveTo sink { state with Args = args } sinkState dx 0.0

    /// <summary>The vmoveto operator, <c>path_procs_t::vmoveto</c>.</summary>
    let private vMoveTo (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        let struct (args, dy) = CharArgStack.pop state.Args
        moveTo sink { state with Args = args } sinkState 0.0 dy

    /// <summary>Walks the rlineto argument pairs, <c>path_procs_t::rlineto</c>.</summary>
    [<TailCall>]
    let rec private rLineToLoop (sink: 'K) (state: State) (sinkState: 'S) (index: int) : struct (State * 'S) =
        match index + 2 <= CharArgStack.getCount state.Args with
        | false -> struct (state, sinkState)
        | true ->
            let struct (args, dx) = CharArgStack.evalArg state.Args index
            let struct (args, dy) = CharArgStack.evalArg args (index + 1)
            let struct (next, nextSink) = lineTo sink { state with Args = args } sinkState dx dy
            rLineToLoop sink next nextSink (index + 2)

    /// <summary>The rlineto operator.</summary>
    let private rLineTo (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        rLineToLoop sink state sinkState 0

    /// <summary>Walks the hlineto arguments, alternating horizontal then vertical lines.</summary>
    [<TailCall>]
    let rec private hLineToLoop (sink: 'K) (state: State) (sinkState: 'S) (index: int) : struct (State * 'S) =
        let count = CharArgStack.getCount state.Args
        match index + 2 <= count with
        | false ->
            match index < count with
            | false -> struct (state, sinkState)
            | true ->
                let struct (args, dx) = CharArgStack.evalArg state.Args index
                lineTo sink { state with Args = args } sinkState dx 0.0
        | true ->
            let struct (args, dx) = CharArgStack.evalArg state.Args index
            let struct (args, dy) = CharArgStack.evalArg args (index + 1)
            let struct (afterX, afterXSink) = lineTo sink { state with Args = args } sinkState dx 0.0
            let struct (afterY, afterYSink) = lineTo sink afterX afterXSink 0.0 dy
            hLineToLoop sink afterY afterYSink (index + 2)

    /// <summary>The hlineto operator.</summary>
    let private hLineTo (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        hLineToLoop sink state sinkState 0

    /// <summary>Walks the vlineto arguments, alternating vertical then horizontal lines.</summary>
    [<TailCall>]
    let rec private vLineToLoop (sink: 'K) (state: State) (sinkState: 'S) (index: int) : struct (State * 'S) =
        let count = CharArgStack.getCount state.Args
        match index + 2 <= count with
        | false ->
            match index < count with
            | false -> struct (state, sinkState)
            | true ->
                let struct (args, dy) = CharArgStack.evalArg state.Args index
                lineTo sink { state with Args = args } sinkState 0.0 dy
        | true ->
            let struct (args, dy) = CharArgStack.evalArg state.Args index
            let struct (args, dx) = CharArgStack.evalArg args (index + 1)
            let struct (afterY, afterYSink) = lineTo sink { state with Args = args } sinkState 0.0 dy
            let struct (afterX, afterXSink) = lineTo sink afterY afterYSink dx 0.0
            vLineToLoop sink afterX afterXSink (index + 2)

    /// <summary>The vlineto operator.</summary>
    let private vLineTo (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        vLineToLoop sink state sinkState 0

    /// <summary>Applies one relative curve through the sink.</summary>
    /// <param name="sink">The sink.</param>
    /// <param name="state">The state.</param>
    /// <param name="sinkState">The sink state.</param>
    /// <param name="dx1">The first control offset's x.</param>
    /// <param name="dy1">The first control offset's y.</param>
    /// <param name="dx2">The second control offset's x.</param>
    /// <param name="dy2">The second control offset's y.</param>
    /// <param name="dx3">The new point offset's x.</param>
    /// <param name="dy3">The new point offset's y.</param>
    /// <returns>The state and sink state.</returns>
    let private curveTo<'K, 'S when 'K : struct and 'K :> ICharStringSink<'S>> (sink: 'K) (state: State) (sinkState: 'S) (dx1: float) (dy1: float) (dx2: float) (dy2: float) (dx3: float) (dy3: float) : struct (State * 'S) =
        let x1 = state.PtX + dx1
        let y1 = state.PtY + dy1
        let x2 = x1 + dx2
        let y2 = y1 + dy2
        let x3 = x2 + dx3
        let y3 = y2 + dy3
        struct ({ state with PtX = x3; PtY = y3 }, sink.CurveTo(sinkState, state.PtX, state.PtY, x1, y1, x2, y2, x3, y3))

    /// <summary>Walks the rrcurveto argument sextuples.</summary>
    [<TailCall>]
    let rec private rrCurveToLoop (sink: 'K) (state: State) (sinkState: 'S) (index: int) : struct (State * 'S) =
        match index + 6 <= CharArgStack.getCount state.Args with
        | false -> struct (state, sinkState)
        | true ->
            let struct (args, dx1) = CharArgStack.evalArg state.Args index
            let struct (args, dy1) = CharArgStack.evalArg args (index + 1)
            let struct (args, dx2) = CharArgStack.evalArg args (index + 2)
            let struct (args, dy2) = CharArgStack.evalArg args (index + 3)
            let struct (args, dx3) = CharArgStack.evalArg args (index + 4)
            let struct (args, dy3) = CharArgStack.evalArg args (index + 5)
            let struct (next, nextSink) = curveTo sink { state with Args = args } sinkState dx1 dy1 dx2 dy2 dx3 dy3
            rrCurveToLoop sink next nextSink (index + 6)

    /// <summary>The rrcurveto operator.</summary>
    let private rrCurveTo (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        rrCurveToLoop sink state sinkState 0

    /// <summary>Walks the rcurveline curves, then one line.</summary>
    [<TailCall>]
    let rec private rCurveLineLoop (sink: 'K) (state: State) (sinkState: 'S) (index: int) (limit: int) : struct (State * 'S) =
        match index + 6 <= limit with
        | false ->
            let struct (args, dx) = CharArgStack.evalArg state.Args index
            let struct (args, dy) = CharArgStack.evalArg args (index + 1)
            lineTo sink { state with Args = args } sinkState dx dy
        | true ->
            let struct (args, dx1) = CharArgStack.evalArg state.Args index
            let struct (args, dy1) = CharArgStack.evalArg args (index + 1)
            let struct (args, dx2) = CharArgStack.evalArg args (index + 2)
            let struct (args, dy2) = CharArgStack.evalArg args (index + 3)
            let struct (args, dx3) = CharArgStack.evalArg args (index + 4)
            let struct (args, dy3) = CharArgStack.evalArg args (index + 5)
            let struct (next, nextSink) = curveTo sink { state with Args = args } sinkState dx1 dy1 dx2 dy2 dx3 dy3
            rCurveLineLoop sink next nextSink (index + 6) limit

    /// <summary>The rcurveline operator.</summary>
    let private rCurveLine (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        match CharArgStack.getCount state.Args < 8 with
        | true -> struct (state, sinkState)
        | false -> rCurveLineLoop sink state sinkState 0 (CharArgStack.getCount state.Args - 2)

    /// <summary>Walks the rlinecurve lines, then one curve.</summary>
    [<TailCall>]
    let rec private rLineCurveLoop (sink: 'K) (state: State) (sinkState: 'S) (index: int) (limit: int) : struct (State * 'S) =
        match index + 2 <= limit with
        | false ->
            let struct (args, dx1) = CharArgStack.evalArg state.Args index
            let struct (args, dy1) = CharArgStack.evalArg args (index + 1)
            let struct (args, dx2) = CharArgStack.evalArg args (index + 2)
            let struct (args, dy2) = CharArgStack.evalArg args (index + 3)
            let struct (args, dx3) = CharArgStack.evalArg args (index + 4)
            let struct (args, dy3) = CharArgStack.evalArg args (index + 5)
            curveTo sink { state with Args = args } sinkState dx1 dy1 dx2 dy2 dx3 dy3
        | true ->
            let struct (args, dx) = CharArgStack.evalArg state.Args index
            let struct (args, dy) = CharArgStack.evalArg args (index + 1)
            let struct (next, nextSink) = lineTo sink { state with Args = args } sinkState dx dy
            rLineCurveLoop sink next nextSink (index + 2) limit

    /// <summary>The rlinecurve operator.</summary>
    let private rLineCurve (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        match CharArgStack.getCount state.Args < 8 with
        | true -> struct (state, sinkState)
        | false -> rLineCurveLoop sink state sinkState 0 (CharArgStack.getCount state.Args - 6)

    /// <summary>Walks the vvcurveto curves; an odd argument count makes the first control point horizontal.</summary>
    [<TailCall>]
    let rec private vvCurveToLoop (sink: 'K) (state: State) (sinkState: 'S) (index: int) (dx1: float) : struct (State * 'S) =
        match index + 4 <= CharArgStack.getCount state.Args with
        | false -> struct (state, sinkState)
        | true ->
            let struct (args, dy1) = CharArgStack.evalArg state.Args index
            let struct (args, dx2) = CharArgStack.evalArg args (index + 1)
            let struct (args, dy2) = CharArgStack.evalArg args (index + 2)
            let struct (args, dy3) = CharArgStack.evalArg args (index + 3)
            let struct (next, nextSink) = curveTo sink { state with Args = args } sinkState dx1 dy1 dx2 dy2 0.0 dy3
            vvCurveToLoop sink next nextSink (index + 4) 0.0

    /// <summary>The vvcurveto operator.</summary>
    let private vvCurveTo (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        match CharArgStack.getCount state.Args &&& 1 with
        | 0 -> vvCurveToLoop sink state sinkState 0 0.0
        | _ ->
            let struct (args, dx) = CharArgStack.evalArg state.Args 0
            vvCurveToLoop sink { state with Args = args } sinkState 1 dx

    /// <summary>Walks the hhcurveto curves; an odd argument count makes the first control point vertical.</summary>
    [<TailCall>]
    let rec private hhCurveToLoop (sink: 'K) (state: State) (sinkState: 'S) (index: int) (dy1: float) : struct (State * 'S) =
        match index + 4 <= CharArgStack.getCount state.Args with
        | false -> struct (state, sinkState)
        | true ->
            let struct (args, dx1) = CharArgStack.evalArg state.Args index
            let struct (args, dx2) = CharArgStack.evalArg args (index + 1)
            let struct (args, dy2) = CharArgStack.evalArg args (index + 2)
            let struct (args, dx3) = CharArgStack.evalArg args (index + 3)
            let struct (next, nextSink) = curveTo sink { state with Args = args } sinkState dx1 dy1 dx2 dy2 dx3 0.0
            hhCurveToLoop sink next nextSink (index + 4) 0.0

    /// <summary>The hhcurveto operator.</summary>
    let private hhCurveTo (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        match CharArgStack.getCount state.Args &&& 1 with
        | 0 -> hhCurveToLoop sink state sinkState 0 0.0
        | _ ->
            let struct (args, dy) = CharArgStack.evalArg state.Args 0
            hhCurveToLoop sink { state with Args = args } sinkState 1 dy

    /// <summary>Draws one curve from absolute coordinates, updating the current point.</summary>
    let private drawCurve<'K, 'S when 'K : struct and 'K :> ICharStringSink<'S>> (sink: 'K) (state: State) (sinkState: 'S) (x1: float) (y1: float) (x2: float) (y2: float) (x3: float) (y3: float) : struct (State * 'S) =
        let sinkState = sink.CurveTo(sinkState, state.PtX, state.PtY, x1, y1, x2, y2, x3, y3)
        struct ({ state with PtX = x3; PtY = y3 }, sinkState)

    /// <summary>Walks the vhcurveto operator's even-run curves.</summary>
    [<TailCall>]
    let rec private vhCurveToEvenLoop (sink: 'K) (state: State) (sinkState: 'S) (index: int) : struct (State * 'S) =
        let count = CharArgStack.getCount state.Args
        match index + 8 <= count with
        | false -> struct (state, sinkState)
        | true ->
            let struct (args, e0) = CharArgStack.evalArg state.Args index
            let struct (args, e1) = CharArgStack.evalArg args (index + 1)
            let struct (args, e2) = CharArgStack.evalArg args (index + 2)
            let struct (args, e3) = CharArgStack.evalArg args (index + 3)
            let struct (args, e4) = CharArgStack.evalArg args (index + 4)
            let struct (args, e5) = CharArgStack.evalArg args (index + 5)
            let struct (args, e6) = CharArgStack.evalArg args (index + 6)
            let struct (args, e7) = CharArgStack.evalArg args (index + 7)
            let y1 = state.PtY + e0
            let x2 = state.PtX + e1
            let y2 = y1 + e2
            let x3 = x2 + e3
            let y3 = y2
            let struct (after, afterSink) = drawCurve sink { state with Args = args } sinkState state.PtX y1 x2 y2 x3 y3
            let x1 = x3 + e4
            let y1 = y3
            let x2 = x1 + e5
            let y2 = y1 + e6
            let x3 = x2
            let y3 = y2 + e7
            let x3 =
                match count - index < 16 && (count &&& 1) <> 0 with
                | true -> let struct (_, extra) = CharArgStack.evalArg args (index + 8) in x3 + extra
                | false -> x3
            let struct (after, afterSink) = drawCurve sink after afterSink x1 y1 x2 y2 x3 y3
            vhCurveToEvenLoop sink after afterSink (index + 8)

    /// <summary>Walks the vhcurveto operator's odd-run curves, then draws the final curve.</summary>
    [<TailCall>]
    let rec private vhCurveToOddLoop (sink: 'K) (state: State) (sinkState: 'S) (index: int) (x1: float) (y1: float) (x2: float) (y2: float) (x3: float) (y3: float) : struct (State * 'S) =
        let count = CharArgStack.getCount state.Args
        match index + 8 <= count with
        | false ->
            let y3 =
                match index < count with
                | true -> let struct (_, extra) = CharArgStack.evalArg state.Args index in y3 + extra
                | false -> y3
            drawCurve sink state sinkState x1 y1 x2 y2 x3 y3
        | true ->
            let struct (after, afterSink) = drawCurve sink state sinkState x1 y1 x2 y2 x3 y3
            let struct (args, e0) = CharArgStack.evalArg after.Args index
            let struct (args, e1) = CharArgStack.evalArg args (index + 1)
            let struct (args, e2) = CharArgStack.evalArg args (index + 2)
            let struct (args, e3) = CharArgStack.evalArg args (index + 3)
            let struct (args, e4) = CharArgStack.evalArg args (index + 4)
            let struct (args, e5) = CharArgStack.evalArg args (index + 5)
            let struct (args, e6) = CharArgStack.evalArg args (index + 6)
            let struct (args, e7) = CharArgStack.evalArg args (index + 7)
            let x1 = after.PtX + e0
            let y1 = after.PtY
            let x2 = x1 + e1
            let y2 = y1 + e2
            let x3 = x2
            let y3 = y2 + e3
            let struct (after, afterSink) = drawCurve sink { after with Args = args } afterSink x1 y1 x2 y2 x3 y3
            let x1 = x3
            let y1 = y3 + e4
            let x2 = x1 + e5
            let y2 = y1 + e6
            let x3 = x2 + e7
            let y3 = y2
            vhCurveToOddLoop sink after afterSink (index + 8) x1 y1 x2 y2 x3 y3

    /// <summary>The vhcurveto operator.</summary>
    let private vhCurveTo (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        let count = CharArgStack.getCount state.Args
        match count % 8 >= 4 with
        | false -> vhCurveToEvenLoop sink state sinkState 0
        | true ->
            let struct (args, e0) = CharArgStack.evalArg state.Args 0
            let struct (args, e1) = CharArgStack.evalArg args 1
            let struct (args, e2) = CharArgStack.evalArg args 2
            let struct (args, e3) = CharArgStack.evalArg args 3
            let x1 = state.PtX
            let y1 = state.PtY + e0
            let x2 = x1 + e1
            let y2 = y1 + e2
            let x3 = x2 + e3
            let y3 = y2
            vhCurveToOddLoop sink { state with Args = args } sinkState 4 x1 y1 x2 y2 x3 y3

    /// <summary>Walks the hvcurveto operator's even-run curves.</summary>
    [<TailCall>]
    let rec private hvCurveToEvenLoop (sink: 'K) (state: State) (sinkState: 'S) (index: int) : struct (State * 'S) =
        let count = CharArgStack.getCount state.Args
        match index + 8 <= count with
        | false -> struct (state, sinkState)
        | true ->
            let struct (args, e0) = CharArgStack.evalArg state.Args index
            let struct (args, e1) = CharArgStack.evalArg args (index + 1)
            let struct (args, e2) = CharArgStack.evalArg args (index + 2)
            let struct (args, e3) = CharArgStack.evalArg args (index + 3)
            let struct (args, e4) = CharArgStack.evalArg args (index + 4)
            let struct (args, e5) = CharArgStack.evalArg args (index + 5)
            let struct (args, e6) = CharArgStack.evalArg args (index + 6)
            let struct (args, e7) = CharArgStack.evalArg args (index + 7)
            let x1 = state.PtX + e0
            let y1 = state.PtY
            let x2 = x1 + e1
            let y2 = y1 + e2
            let x3 = x2
            let y3 = y2 + e3
            let struct (after, afterSink) = drawCurve sink { state with Args = args } sinkState x1 y1 x2 y2 x3 y3
            let y1 = y3 + e4
            let x1 = x3
            let x2 = x1 + e5
            let y2 = y1 + e6
            let x3 = x2 + e7
            let y3 =
                match count - index < 16 && (count &&& 1) <> 0 with
                | true -> let struct (_, extra) = CharArgStack.evalArg args (index + 8) in y2 + extra
                | false -> y2
            let struct (after, afterSink) = drawCurve sink after afterSink x1 y1 x2 y2 x3 y3
            hvCurveToEvenLoop sink after afterSink (index + 8)

    /// <summary>Walks the hvcurveto operator's odd-run curves, then draws the final curve.</summary>
    [<TailCall>]
    let rec private hvCurveToOddLoop (sink: 'K) (state: State) (sinkState: 'S) (index: int) (x1: float) (y1: float) (x2: float) (y2: float) (x3: float) (y3: float) : struct (State * 'S) =
        let count = CharArgStack.getCount state.Args
        match index + 8 <= count with
        | false ->
            let x3 =
                match index < count with
                | true -> let struct (_, extra) = CharArgStack.evalArg state.Args index in x3 + extra
                | false -> x3
            drawCurve sink state sinkState x1 y1 x2 y2 x3 y3
        | true ->
            let struct (after, afterSink) = drawCurve sink state sinkState x1 y1 x2 y2 x3 y3
            let struct (args, e0) = CharArgStack.evalArg after.Args index
            let struct (args, e1) = CharArgStack.evalArg args (index + 1)
            let struct (args, e2) = CharArgStack.evalArg args (index + 2)
            let struct (args, e3) = CharArgStack.evalArg args (index + 3)
            let struct (args, e4) = CharArgStack.evalArg args (index + 4)
            let struct (args, e5) = CharArgStack.evalArg args (index + 5)
            let struct (args, e6) = CharArgStack.evalArg args (index + 6)
            let struct (args, e7) = CharArgStack.evalArg args (index + 7)
            let x1 = after.PtX
            let y1 = after.PtY + e0
            let x2 = x1 + e1
            let y2 = y1 + e2
            let x3 = x2 + e3
            let y3 = y2
            let struct (after, afterSink) = drawCurve sink { after with Args = args } afterSink x1 y1 x2 y2 x3 y3
            let x1 = x3 + e4
            let y1 = y3
            let x2 = x1 + e5
            let y2 = y1 + e6
            let x3 = x2
            let y3 = y2 + e7
            hvCurveToOddLoop sink after afterSink (index + 8) x1 y1 x2 y2 x3 y3

    /// <summary>The hvcurveto operator.</summary>
    let private hvCurveTo (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        let count = CharArgStack.getCount state.Args
        match count % 8 >= 4 with
        | false -> hvCurveToEvenLoop sink state sinkState 0
        | true ->
            let struct (args, e0) = CharArgStack.evalArg state.Args 0
            let struct (args, e1) = CharArgStack.evalArg args 1
            let struct (args, e2) = CharArgStack.evalArg args 2
            let struct (args, e3) = CharArgStack.evalArg args 3
            let x1 = state.PtX + e0
            let y1 = state.PtY
            let x2 = x1 + e1
            let y2 = y1 + e2
            let x3 = x2
            let y3 = y2 + e3
            hvCurveToOddLoop sink { state with Args = args } sinkState 4 x1 y1 x2 y2 x3 y3

    /// <summary>Applies two curves, <c>path_procs_t::curve2</c>.</summary>
    let private curve2<'K, 'S when 'K : struct and 'K :> ICharStringSink<'S>> (sink: 'K) (state: State) (sinkState: 'S) (x1: float) (y1: float) (x2: float) (y2: float) (x3: float) (y3: float) (x4: float) (y4: float) (x5: float) (y5: float) (x6: float) (y6: float) : struct (State * 'S) =
        let sinkState = sink.CurveTo(sinkState, state.PtX, state.PtY, x1, y1, x2, y2, x3, y3)
        struct ({ state with PtX = x6; PtY = y6 }, sink.CurveTo(sinkState, x3, y3, x4, y4, x5, y5, x6, y6))

    /// <summary>The hflex operator.</summary>
    let private hFlex (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        match CharArgStack.getCount state.Args = 7 with
        | false -> struct ({ state with StrError = true }, sinkState)
        | true ->
            let struct (args, d1) = CharArgStack.evalArg state.Args 0
            let struct (args, d2) = CharArgStack.evalArg args 1
            let struct (args, d3) = CharArgStack.evalArg args 2
            let struct (args, d4) = CharArgStack.evalArg args 3
            let struct (args, d5) = CharArgStack.evalArg args 4
            let struct (args, d6) = CharArgStack.evalArg args 5
            let struct (args, d7) = CharArgStack.evalArg args 6
            let x1 = state.PtX + d1
            let y1 = state.PtY
            let x2 = x1 + d2
            let y2 = y1 + d3
            let x3 = x2 + d4
            let y3 = y2
            let x4 = x3 + d5
            let y4 = y3
            let x5 = x4 + d6
            let y5 = y1
            let x6 = x5 + d7
            curve2 sink { state with Args = args } sinkState x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y5

    /// <summary>The flex operator.</summary>
    let private flex (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        match CharArgStack.getCount state.Args = 13 with
        | false -> struct ({ state with StrError = true }, sinkState)
        | true ->
            let struct (args, a0) = CharArgStack.evalArg state.Args 0
            let struct (args, a1) = CharArgStack.evalArg args 1
            let struct (args, a2) = CharArgStack.evalArg args 2
            let struct (args, a3) = CharArgStack.evalArg args 3
            let struct (args, a4) = CharArgStack.evalArg args 4
            let struct (args, a5) = CharArgStack.evalArg args 5
            let struct (args, a6) = CharArgStack.evalArg args 6
            let struct (args, a7) = CharArgStack.evalArg args 7
            let struct (args, a8) = CharArgStack.evalArg args 8
            let struct (args, a9) = CharArgStack.evalArg args 9
            let struct (args, a10) = CharArgStack.evalArg args 10
            let struct (args, a11) = CharArgStack.evalArg args 11
            let x1 = state.PtX + a0
            let y1 = state.PtY + a1
            let x2 = x1 + a2
            let y2 = y1 + a3
            let x3 = x2 + a4
            let y3 = y2 + a5
            let x4 = x3 + a6
            let y4 = y3 + a7
            let x5 = x4 + a8
            let y5 = y4 + a9
            let x6 = x5 + a10
            let y6 = y5 + a11
            curve2 sink { state with Args = args } sinkState x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6

    /// <summary>The hflex1 operator.</summary>
    let private hFlex1 (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        match CharArgStack.getCount state.Args = 9 with
        | false -> struct ({ state with StrError = true }, sinkState)
        | true ->
            let struct (args, a0) = CharArgStack.evalArg state.Args 0
            let struct (args, a1) = CharArgStack.evalArg args 1
            let struct (args, a2) = CharArgStack.evalArg args 2
            let struct (args, a3) = CharArgStack.evalArg args 3
            let struct (args, a4) = CharArgStack.evalArg args 4
            let struct (args, a5) = CharArgStack.evalArg args 5
            let struct (args, a6) = CharArgStack.evalArg args 6
            let struct (args, a7) = CharArgStack.evalArg args 7
            let struct (args, a8) = CharArgStack.evalArg args 8
            let x1 = state.PtX + a0
            let y1 = state.PtY + a1
            let x2 = x1 + a2
            let y2 = y1 + a3
            let x3 = x2 + a4
            let y3 = y2
            let x4 = x3 + a5
            let y4 = y3
            let x5 = x4 + a6
            let y5 = y4 + a7
            let x6 = x5 + a8
            let y6 = state.PtY
            curve2 sink { state with Args = args } sinkState x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6

    /// <summary>The flex1 operator.</summary>
    let private flex1 (sink: 'K) (state: State) (sinkState: 'S) : struct (State * 'S) =
        match CharArgStack.getCount state.Args = 11 with
        | false -> struct ({ state with StrError = true }, sinkState)
        | true ->
            let struct (args, a0) = CharArgStack.evalArg state.Args 0
            let struct (args, a1) = CharArgStack.evalArg args 1
            let struct (args, a2) = CharArgStack.evalArg args 2
            let struct (args, a3) = CharArgStack.evalArg args 3
            let struct (args, a4) = CharArgStack.evalArg args 4
            let struct (args, a5) = CharArgStack.evalArg args 5
            let struct (args, a6) = CharArgStack.evalArg args 6
            let struct (args, a7) = CharArgStack.evalArg args 7
            let struct (args, a8) = CharArgStack.evalArg args 8
            let struct (args, a9) = CharArgStack.evalArg args 9
            let struct (args, a10) = CharArgStack.evalArg args 10
            let sumX = a0 + a2 + a4 + a6 + a8
            let sumY = a1 + a3 + a5 + a7 + a9
            let x1 = state.PtX + a0
            let y1 = state.PtY + a1
            let x2 = x1 + a2
            let y2 = y1 + a3
            let x3 = x2 + a4
            let y3 = y2 + a5
            let x4 = x3 + a6
            let y4 = y3 + a7
            let x5 = x4 + a8
            let y5 = y4 + a9
            match abs sumX > abs sumY with
            | true ->
                let x6 = x5 + a10
                curve2 sink { state with Args = args } sinkState x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 state.PtY
            | false ->
                let y6 = y5 + a10
                curve2 sink { state with Args = args } sinkState x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 state.PtX y6

    /// <summary>Calls a subroutine, <c>cs_interp_env_t::call_subr</c>.</summary>
    /// <param name="table">The CFF table bytes.</param>
    /// <param name="subrs">The subroutine INDEX.</param>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private callSubr (table: ReadOnlySpan<byte>) (subrs: CffIndex) (state: State) : State =
        let struct (args, n) = CharArgStack.popInt state.Args
        let biased = n + subrBias subrs.Count
        match biased < 0 || uint32 biased >= subrs.Count || CharCallStack.getCount state.Calls >= MaxCalls with
        | true -> { state with Args = args; StrError = true }
        | false ->
            let struct (subrOffset, subrLength) = CffIndex.item table subrs (uint32 biased)
            let calls = CharCallStack.push state.Calls state.StrStart state.StrLength state.StrOffset
            { state with Args = args; Calls = calls; StrStart = subrOffset; StrLength = subrLength; StrOffset = 0 }

    /// <summary>Returns from a subroutine, <c>cs_interp_env_t::return_from_subr</c>.</summary>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private returnFromSubr (state: State) : State =
        let struct (calls, struct (strStart, strLength, strOffset)) = CharCallStack.pop state.Calls
        { state with Calls = calls; StrStart = strStart; StrLength = strLength; StrOffset = strOffset }

    /// <summary>Pops an unsigned number, <c>arg_stack_t::pop_uint</c>: a negative operand is an error and reads as 0.</summary>
    /// <param name="state">The state.</param>
    /// <returns>The state and the unsigned number.</returns>
    let private popUint (state: State) : struct (State * int) =
        let struct (args, n) = CharArgStack.popInt state.Args
        match n < 0 with
        | true -> struct ({ state with Args = args; StrError = true }, 0)
        | false -> struct ({ state with Args = args }, n)

    /// <summary>Applies the vsindex operator, <c>cff2_cs_interp_env_t::process_vsindex</c>.</summary>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private processVsindex (state: State) : State =
        let struct (popped, index) = popUint state
        let next =
            match popped.SeenVsindex || popped.SeenBlend with
            | true -> { popped with StrError = true }
            | false -> { popped with Ivs = index }
        { next with Args = CharArgStack.clear next.Args; SeenVsindex = true }

    /// <summary>Walks the blend delta columns, adding scalar times delta, in column order.</summary>
    /// <param name="blend">The blend context.</param>
    /// <param name="ivs">The data set number.</param>
    /// <param name="args">The argument stack.</param>
    /// <param name="deltasAt">The first delta's index.</param>
    /// <param name="index">The next column.</param>
    /// <param name="k">The region count.</param>
    /// <param name="acc">The running sum.</param>
    /// <returns>The delta sum.</returns>
    [<TailCall>]
    let rec private blendDeltasSum<'B when 'B : struct and 'B :> ICharStringBlend<'B>> (blend: 'B) (ivs: int) (args: CharArgStack) (deltasAt: int) (index: int) (k: int) (acc: float) : float =
        match index < k with
        | false -> acc
        | true ->
            let struct (_, delta) = CharArgStack.evalArg args (deltasAt + index)
            let scalar = blend.Scalar(blend, ivs, index)
            blendDeltasSum blend ivs args deltasAt (index + 1) k (acc + float scalar * delta)

    /// <summary>Sums the blend deltas of one default argument, <c>cff2_cs_interp_env_t::blend_deltas</c>, in double.</summary>
    /// <param name="blend">The blend context.</param>
    /// <param name="ivs">The data set number.</param>
    /// <param name="args">The argument stack.</param>
    /// <param name="deltasAt">The first delta's index.</param>
    /// <param name="k">The region count.</param>
    /// <returns>The delta sum.</returns>
    let private blendDeltas<'B when 'B : struct and 'B :> ICharStringBlend<'B>> (blend: 'B) (ivs: int) (args: CharArgStack) (deltasAt: int) (k: int) : float =
        match blend.Blending(blend) with
        | false -> 0.0
        | true -> blendDeltasSum blend ivs args deltasAt 0 k 0.0

    /// <summary>Adds the deltas to their defaults, one default at a time.</summary>
    /// <param name="blend">The blend context.</param>
    /// <param name="ivs">The data set number.</param>
    /// <param name="args">The argument stack.</param>
    /// <param name="start">The first default's index.</param>
    /// <param name="n">The number of defaults.</param>
    /// <param name="k">The region count.</param>
    /// <param name="index">The next default.</param>
    /// <returns>The argument stack.</returns>
    [<TailCall>]
    let rec private blendLoop<'B when 'B : struct and 'B :> ICharStringBlend<'B>> (blend: 'B) (ivs: int) (args: CharArgStack) (start: int) (n: int) (k: int) (index: int) : CharArgStack =
        match index < n with
        | false -> args
        | true ->
            let struct (_, defaultValue) = CharArgStack.evalArg args (start + index)
            let delta = blendDeltas blend ivs args (start + n + index * k) k
            let args = CharArgStack.setAt args (start + index) (defaultValue + delta)
            blendLoop blend ivs args start n k (index + 1)

    /// <summary>Applies the blend operator, <c>cff2_cs_opset_t::process_blend</c>.</summary>
    /// <param name="blend">The blend context.</param>
    /// <param name="state">The state.</param>
    /// <returns>The state.</returns>
    let private processBlend<'B when 'B : struct and 'B :> ICharStringBlend<'B>> (blend: 'B) (state: State) : State =
        let seen =
            match state.SeenBlend with
            | true -> state
            | false -> { state with SeenBlend = true; RegionCount = blend.RegionCount(blend, state.Ivs) }
        let k = seen.RegionCount
        let struct (popped, n) = popUint seen
        let count = CharArgStack.getCount popped.Args
        let total64 = int64 (k + 1) * int64 n
        match struct (total64 > int64 UInt32.MaxValue, total64 > int64 count) with
        | struct (true, _) -> { popped with StrError = true }
        | struct (_, true) -> { popped with StrError = true }
        | struct (false, false) ->
            let total = int total64
            let budget = seen.Budget - int64 (total - n)
            match budget < 0L with
            | true -> { popped with StrError = true }
            | false ->
                let start = count - total
                let blended = blendLoop blend popped.Ivs popped.Args start n k 0
                { popped with Args = { blended with Count = blended.Count - k * n }; Budget = budget }

    /// <summary>Processes one operator, <c>cs_opset_t::process_op</c>.</summary>
    /// <param name="sink">The sink.</param>
    /// <param name="table">The CFF table bytes.</param>
    /// <param name="globalSubrs">The global subroutine INDEX.</param>
    /// <param name="localSubrs">The local subroutine INDEX.</param>
    /// <param name="flavour">The flavour.</param>
    /// <param name="opcode">The operator.</param>
    /// <param name="state">The state.</param>
    /// <param name="sinkState">The sink state.</param>
    /// <returns>The state and sink state.</returns>
    let private processOp<'K, 'S, 'B when 'K : struct and 'K :> ICharStringSink<'S> and 'B : struct and 'B :> ICharStringBlend<'B>> (sink: 'K) (table: ReadOnlySpan<byte>) (globalSubrs: CffIndex) (localSubrs: CffIndex) (flavour: CffFlavour<'B>) (opcode: int) (state: State) (sinkState: 'S) : struct (State * 'S) =
        match opcode with
        | OpDotsection ->
            match flavour with
            | Cff1 -> struct (state, sinkState)
            | Cff2 _ -> struct ({ state with Args = CharArgStack.clear state.Args }, sinkState)
        | OpEndchar ->
            let widthState = checkWidth flavour state
            let ending =
                match flavour, CharArgStack.getCount widthState.Args >= 4 with
                | Cff1, true ->
                    let struct (args, adx) = CharArgStack.evalArg widthState.Args (CharArgStack.getCount widthState.Args - 4)
                    let struct (args, ady) = CharArgStack.evalArg args (CharArgStack.getCount widthState.Args - 3)
                    let struct (args, baseNum) = CharArgStack.evalArg args (CharArgStack.getCount widthState.Args - 2)
                    let struct (_, accentNum) = CharArgStack.evalArg args (CharArgStack.getCount widthState.Args - 1)
                    EndedWithSeac (adx, ady, toIntClamped baseNum, toIntClamped accentNum)
                | _ -> Ended
            struct ({ widthState with Args = CharArgStack.clear widthState.Args; Endchar = true; Ending = ending }, sinkState)
        | OpReturn -> struct (returnFromSubr state, sinkState)
        | OpVsindex ->
            match flavour with
            | Cff1 -> struct ({ state with Args = CharArgStack.clear state.Args }, sinkState)
            | Cff2 _ -> struct (processVsindex state, sinkState)
        | OpBlend ->
            match flavour with
            | Cff1 -> struct ({ state with Args = CharArgStack.clear state.Args }, sinkState)
            | Cff2 blend -> struct (processBlend blend state, sinkState)
        | OpFixedcs ->
            match state.StrOffset + 4 <= state.StrLength with
            | false -> struct (state, sinkState)
            | true ->
                let raw = int (int32 (Binary.u32 table (state.StrStart + state.StrOffset)))
                struct (inc { state with Args = CharArgStack.pushFixed state.Args raw } 4, sinkState)
        | OpCallsubr -> struct (callSubr table localSubrs state, sinkState)
        | OpCallgsubr -> struct (callSubr table globalSubrs state, sinkState)
        | OpHstem | OpHstemhm ->
            let widthState = checkWidth flavour state
            let hstemCount = widthState.HstemCount + CharArgStack.getCount widthState.Args / 2
            struct ({ widthState with HstemCount = hstemCount; Args = CharArgStack.clear widthState.Args }, sinkState)
        | OpVstem | OpVstemhm ->
            let widthState = checkWidth flavour state
            let vstemCount = widthState.VstemCount + CharArgStack.getCount widthState.Args / 2
            struct ({ widthState with VstemCount = vstemCount; Args = CharArgStack.clear widthState.Args }, sinkState)
        | OpHintmask | OpCntrmask ->
            let widthState = checkWidth flavour state
            let determined = determineHintmaskSize widthState
            match determined.StrOffset + determined.HintmaskSize <= determined.StrLength with
            | false -> struct (determined, sinkState)
            | true -> struct (inc { determined with Args = CharArgStack.clear determined.Args } determined.HintmaskSize, sinkState)
        | OpRmoveto ->
            let widthState = checkWidth flavour state
            let struct (moved, movedSink) = rMoveTo sink widthState sinkState
            struct ({ moved with Args = CharArgStack.clear moved.Args }, movedSink)
        | OpHmoveto ->
            let widthState = checkWidth flavour state
            let struct (moved, movedSink) = hMoveTo sink widthState sinkState
            struct ({ moved with Args = CharArgStack.clear moved.Args }, movedSink)
        | OpVmoveto ->
            let widthState = checkWidth flavour state
            let struct (moved, movedSink) = vMoveTo sink widthState sinkState
            struct ({ moved with Args = CharArgStack.clear moved.Args }, movedSink)
        | OpRlineto ->
            let struct (pathed, pathedSink) = rLineTo sink state sinkState
            struct ({ pathed with Args = CharArgStack.clear pathed.Args }, pathedSink)
        | OpHlineto ->
            let struct (pathed, pathedSink) = hLineTo sink state sinkState
            struct ({ pathed with Args = CharArgStack.clear pathed.Args }, pathedSink)
        | OpVlineto ->
            let struct (pathed, pathedSink) = vLineTo sink state sinkState
            struct ({ pathed with Args = CharArgStack.clear pathed.Args }, pathedSink)
        | OpRrcurveto ->
            let struct (pathed, pathedSink) = rrCurveTo sink state sinkState
            struct ({ pathed with Args = CharArgStack.clear pathed.Args }, pathedSink)
        | OpRcurveline ->
            let struct (pathed, pathedSink) = rCurveLine sink state sinkState
            struct ({ pathed with Args = CharArgStack.clear pathed.Args }, pathedSink)
        | OpRlinecurve ->
            let struct (pathed, pathedSink) = rLineCurve sink state sinkState
            struct ({ pathed with Args = CharArgStack.clear pathed.Args }, pathedSink)
        | OpVvcurveto ->
            let struct (pathed, pathedSink) = vvCurveTo sink state sinkState
            struct ({ pathed with Args = CharArgStack.clear pathed.Args }, pathedSink)
        | OpHhcurveto ->
            let struct (pathed, pathedSink) = hhCurveTo sink state sinkState
            struct ({ pathed with Args = CharArgStack.clear pathed.Args }, pathedSink)
        | OpVhcurveto ->
            let struct (pathed, pathedSink) = vhCurveTo sink state sinkState
            struct ({ pathed with Args = CharArgStack.clear pathed.Args }, pathedSink)
        | OpHvcurveto ->
            let struct (pathed, pathedSink) = hvCurveTo sink state sinkState
            struct ({ pathed with Args = CharArgStack.clear pathed.Args }, pathedSink)
        | OpHflex ->
            let struct (flexed, flexedSink) = hFlex sink state sinkState
            struct ({ flexed with Args = CharArgStack.clear flexed.Args }, flexedSink)
        | OpFlex ->
            let struct (flexed, flexedSink) = flex sink state sinkState
            struct ({ flexed with Args = CharArgStack.clear flexed.Args }, flexedSink)
        | OpHflex1 ->
            let struct (flexed, flexedSink) = hFlex1 sink state sinkState
            struct ({ flexed with Args = CharArgStack.clear flexed.Args }, flexedSink)
        | OpFlex1 ->
            let struct (flexed, flexedSink) = flex1 sink state sinkState
            struct ({ flexed with Args = CharArgStack.clear flexed.Args }, flexedSink)
        | _ -> struct (pushNumber table opcode state, sinkState)

    /// <summary>Runs one charstring, <c>cs_interpreter_t::interpret</c>.</summary>
    /// <param name="sink">The sink.</param>
    /// <param name="table">The CFF table bytes.</param>
    /// <param name="globalSubrs">The global subroutine INDEX.</param>
    /// <param name="localSubrs">The local subroutine INDEX.</param>
    /// <param name="flavour">The flavour.</param>
    /// <param name="state">The state.</param>
    /// <param name="sinkState">The sink state.</param>
    /// <returns>The sink state, the ending, the remaining operator count, the remaining budget and whether the charstring ran to its end.</returns>
    [<TailCall>]
    let rec private run<'K, 'S, 'B when 'K : struct and 'K :> ICharStringSink<'S> and 'B : struct and 'B :> ICharStringBlend<'B>>
            (sink: 'K) (table: ReadOnlySpan<byte>) (globalSubrs: CffIndex) (localSubrs: CffIndex)
            (flavour: CffFlavour<'B>) (state: State) (sinkState: 'S) =
        let struct (opcode, fetched) = fetchOp flavour table state
        let struct (processed, processedSink) = processOp sink table globalSubrs localSubrs flavour opcode fetched sinkState
        let remaining = processed.MaxOps - 1
        let next = { processed with MaxOps = remaining }
        match inError next, remaining = 0 with
        | true, _ -> struct (processedSink, next.Ending, processed.MaxOps, next.Budget, false)
        | false, true -> struct (processedSink, next.Ending, remaining, next.Budget, false)
        | false, false ->
            match next.Endchar with
            | true -> struct (processedSink, next.Ending, remaining, next.Budget, true)
            | false -> run sink table globalSubrs localSubrs flavour next processedSink

    /// <summary>Runs one charstring and keeps what the sink received even when it fails, as upstream's sink keeps it: <c>interpret</c>
    /// charges the operators it ran to the budget whether or not it succeeded.</summary>
    /// <typeparam name="'K">The struct sink.</typeparam>
    /// <typeparam name="'S">The sink state.</typeparam>
    /// <typeparam name="'B">The blend context.</typeparam>
    /// <param name="sink">The sink.</param>
    /// <param name="seed">The initial sink state.</param>
    /// <param name="flavour">The flavour.</param>
    /// <param name="charStringStart">The charstring's offset in the table.</param>
    /// <param name="charStringLength">The charstring's length.</param>
    /// <param name="globalSubrs">The global subroutine INDEX.</param>
    /// <param name="localSubrs">The local subroutine INDEX.</param>
    /// <param name="table">The CFF table bytes.</param>
    /// <param name="budget">The remaining glyph budget.</param>
    /// <returns>Whether it ran to its end, the sink state, the ending and the remaining budget.</returns>
    let interpretKeeping<'K, 'S, 'B when 'K : struct and 'K :> ICharStringSink<'S> and 'B : struct and 'B :> ICharStringBlend<'B>>
            (sink: 'K) (seed: 'S) (flavour: CffFlavour<'B>) (charStringStart: int) (charStringLength: int)
            (globalSubrs: CffIndex) (localSubrs: CffIndex) (table: ReadOnlySpan<byte>) (budget: int64) =
        let startOps = clampMaxOps budget
        match startOps with
        | 0 -> struct (false, seed, Ended, budget)
        | _ ->
            let ivs =
                match flavour with
                | Cff1 -> 0
                | Cff2 blend -> blend.Ivs(blend)
            let state =
                { StrStart = charStringStart
                  StrLength = charStringLength
                  StrOffset = 0
                  StrError = false
                  Args = CharArgStack.create ()
                  Calls = CharCallStack.create ()
                  PtX = 0.0
                  PtY = 0.0
                  Endchar = false
                  Ending = Ended
                  SeenHintmask = false
                  HstemCount = 0
                  VstemCount = 0
                  HintmaskSize = 0
                  ProcessedWidth = false
                  MaxOps = startOps
                  Budget = budget
                  Ivs = ivs
                  SeenVsindex = false
                  SeenBlend = false
                  RegionCount = 0 }
            let struct (sinkState, ending, remainingOps, finalBudget, ran) = run sink table globalSubrs localSubrs flavour state seed
            struct (ran, sinkState, ending, finalBudget - int64 (startOps - remainingOps))

    /// <summary>Runs one charstring; none exactly where upstream's interpret returns false.</summary>
    /// <typeparam name="'K">The struct sink.</typeparam>
    /// <typeparam name="'S">The sink state.</typeparam>
    /// <typeparam name="'B">The blend context.</typeparam>
    /// <param name="sink">The sink.</param>
    /// <param name="seed">The initial sink state.</param>
    /// <param name="flavour">The flavour.</param>
    /// <param name="charStringStart">The charstring's offset in the table.</param>
    /// <param name="charStringLength">The charstring's length.</param>
    /// <param name="globalSubrs">The global subroutine INDEX.</param>
    /// <param name="localSubrs">The local subroutine INDEX.</param>
    /// <param name="table">The CFF table bytes.</param>
    /// <param name="budget">The remaining glyph budget.</param>
    /// <returns>The sink state, the ending and the remaining budget, or none on error.</returns>
    let interpret<'K, 'S, 'B when 'K : struct and 'K :> ICharStringSink<'S> and 'B : struct and 'B :> ICharStringBlend<'B>>
            (sink: 'K) (seed: 'S) (flavour: CffFlavour<'B>) (charStringStart: int) (charStringLength: int)
            (globalSubrs: CffIndex) (localSubrs: CffIndex) (table: ReadOnlySpan<byte>) (budget: int64)
            : struct ('S * CharStringEnd * int64) voption =
        let struct (ran, sinkState, ending, finalBudget) = interpretKeeping sink seed flavour charStringStart charStringLength globalSubrs localSubrs table budget
        match ran with
        | true -> ValueSome (struct (sinkState, ending, finalBudget))
        | false -> ValueNone

    /// <summary>The CFF1 flavour: no blending.</summary>
    let cff1Flavour : CffFlavour<Cff1Blend> = Cff1

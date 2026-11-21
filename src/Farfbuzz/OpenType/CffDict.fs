namespace Farfbuzz

open System
open System.Collections.Immutable
open System.Text

/// <summary>One CFF DICT operator and the operands that preceded it.</summary>
[<Struct>]
[<NoEquality; NoComparison>]
type internal DictOp = { Op: int; Operands: ImmutableArray<double> }

/// <summary>The CFF DICT interpreter, <c>dict_interpreter_t</c>.</summary>
[<RequireQualifiedAccess>]
module internal CffDict =
    /// <summary>The largest argument stack.</summary>
    [<Literal>]
    let private StackLimit = 513

    /// <summary>The largest BCD character count.</summary>
    [<Literal>]
    let private MaxBcdChars = 32

    /// <summary>The BCD digits followed by the special nibble characters.</summary>
    [<Literal>]
    let private BcdAlphabet = "0123456789.EE?-?"

    /// <summary>The interpreter state.</summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type private State =
        { Position: int
          Stack: double list
          Count: int
          Ops: DictOp list }

    /// <summary>Parses the collected BCD text.</summary>
    /// <param name="text">The BCD text.</param>
    /// <param name="count">The nibble count.</param>
    /// <returns>The value, or none when it is not a complete number.</returns>
    let private bcdValue (text: string) (count: int) : double voption =
        match text.Length > 0 && text.Length = count with
        | false -> ValueNone
        | true ->
            match Number.parseDouble (ReadOnlySpan(Encoding.ASCII.GetBytes text)) 0 true with
            | ValueSome scanned -> ValueSome scanned.Value
            | ValueNone -> ValueNone

    /// <summary>Appends one BCD nibble and returns the next character count.</summary>
    /// <param name="chars">The characters so far.</param>
    /// <param name="nibble">The nibble.</param>
    /// <param name="count">The character count.</param>
    /// <returns>The next count, or none when the maximum is exceeded.</returns>
    let private bcdAppend (chars: StringBuilder) (nibble: int) (count: int) : int voption =
        chars.Append(BcdAlphabet[nibble]) |> ignore
        match nibble with
        | 12 ->
            chars.Append('-') |> ignore
            let next = count + 2
            match next <= MaxBcdChars with
            | true -> ValueSome next
            | false -> ValueNone
        | _ -> ValueSome (count + 1)

    /// <summary>Computes the next BCD position, nibble and byte.</summary>
    /// <param name="dict">The DICT bytes.</param>
    /// <param name="index">The nibble index.</param>
    /// <param name="position">The byte position.</param>
    /// <param name="byte">The current byte.</param>
    /// <returns>The next position, nibble and byte.</returns>
    let private bcdStep (dict: ReadOnlySpan<byte>) (index: int) (position: int) (byte: int) =
        match index &&& 1 with
        | 0 ->
            match position < dict.Length with
            | false -> struct (-1, 0, byte)
            | true -> struct (position + 1, int dict[position] >>> 4, int dict[position])
        | _ -> struct (position, byte &&& 0x0F, byte)

    /// <summary>Runs the BCD nibble loop.</summary>
    /// <param name="dict">The DICT bytes.</param>
    /// <param name="chars">The characters accumulated so far.</param>
    /// <param name="position">The next byte.</param>
    /// <param name="count">The character count.</param>
    /// <param name="index">The nibble index.</param>
    /// <param name="byte">The current byte.</param>
    /// <returns>The offset after the number and its value, or none on error.</returns>
    [<TailCall>]
    let rec private bcdLoop (dict: ReadOnlySpan<byte>) (chars: StringBuilder) (position: int) (count: int) (index: int) (byte: int) =
        let struct (position, nibble, byte) = bcdStep dict index position byte
        match count >= MaxBcdChars, position, nibble with
        | true, _, _ -> ValueNone
        | _, -1, _ -> ValueNone
        | _, _, 13 -> ValueNone
        | _, _, 15 ->
            match bcdValue (chars.ToString()) count with
            | ValueSome value -> ValueSome (struct (position, value))
            | ValueNone -> ValueNone
        | _, _, _ ->
            match bcdAppend chars nibble count with
            | ValueNone -> ValueNone
            | ValueSome next -> bcdLoop dict chars position next (index + 1) byte

    /// <summary>Parses one CFF BCD real number, <c>parse_bcd</c>.</summary>
    /// <param name="dict">The DICT bytes.</param>
    /// <param name="start">The first BCD byte.</param>
    /// <returns>The offset after the number and its value, or none on error.</returns>
    let private parseBcd (dict: ReadOnlySpan<byte>) (start: int) : struct (int * double) voption =
        bcdLoop dict (StringBuilder()) start 0 0 0

    /// <summary>Pushes one operand, rejecting a full stack.</summary>
    /// <param name="value">The value.</param>
    /// <param name="count">The current stack size.</param>
    /// <param name="stack">The stack in push order, latest first.</param>
    /// <returns>The new stack, or none when full.</returns>
    let private push (value: double) (count: int) (stack: double list) : double list voption =
        match count < StackLimit with
        | true -> ValueSome (value :: stack)
        | false -> ValueNone

    /// <summary>Emits one operator with the current operands.</summary>
    /// <param name="op">The operator code.</param>
    /// <param name="stack">The stack in push order, latest first.</param>
    /// <param name="ops">The operators so far, latest first.</param>
    /// <returns>The new state.</returns>
    let private emit (op: int) (stack: double list) (ops: DictOp list) : State =
        { Position = 0
          Stack = []
          Count = 0
          Ops = { Op = op; Operands = Sealed.ofList (List.rev stack) } :: ops }

    /// <summary>Reads one signed 16-bit operand.</summary>
    /// <param name="dict">The DICT bytes.</param>
    /// <param name="position">The operand offset.</param>
    /// <returns>The value and the next offset, or none.</returns>
    let private readShort (dict: ReadOnlySpan<byte>) (position: int) : struct (int * double) voption =
        match position + 2 <= dict.Length with
        | false -> ValueNone
        | true ->
            let value = int (int16 ((uint16 (Binary.u8 dict position) <<< 8) ||| uint16 (Binary.u8 dict (position + 1))))
            ValueSome (struct (position + 2, double value))

    /// <summary>Reads one signed 32-bit operand.</summary>
    /// <param name="dict">The DICT bytes.</param>
    /// <param name="position">The operand offset.</param>
    /// <returns>The value and the next offset, or none.</returns>
    let private readLong (dict: ReadOnlySpan<byte>) (position: int) : struct (int * double) voption =
        match position + 4 <= dict.Length with
        | false -> ValueNone
        | true ->
            let value =
                (uint32 (Binary.u8 dict position) <<< 24)
                ||| (uint32 (Binary.u8 dict (position + 1)) <<< 16)
                ||| (uint32 (Binary.u8 dict (position + 2)) <<< 8)
                ||| uint32 (Binary.u8 dict (position + 3))
            ValueSome (struct (position + 4, double (int32 value)))

    /// <summary>Pushes one operand and returns the next interpreter state.</summary>
    /// <param name="state">The current state.</param>
    /// <param name="position">The next byte offset.</param>
    /// <param name="value">The operand.</param>
    /// <returns>The next state, or none when the stack is full.</returns>
    let private pushAt (state: State) (position: int) (value: double) : State voption =
        match push value state.Count state.Stack with
        | ValueNone -> ValueNone
        | ValueSome stack -> ValueSome { state with Position = position; Stack = stack; Count = state.Count + 1 }

    /// <summary>Pushes a parsed operand and returns the next interpreter state.</summary>
    /// <param name="state">The current state.</param>
    /// <param name="parsed">The operand and next offset, or none.</param>
    /// <returns>The next state, or none.</returns>
    let private pushParsed (state: State) (parsed: struct (int * double) voption) : State voption =
        match parsed with
        | ValueNone -> ValueNone
        | ValueSome (struct (position, value)) -> pushAt state position value

    /// <summary>Interprets a DICT as operators with their operands, <c>dict_interpreter_t::interpret</c>.</summary>
    /// <param name="dict">The DICT bytes.</param>
    /// <param name="state">The interpreter state.</param>
    /// <returns>The operators, or none on error.</returns>
    [<TailCall>]
    let rec private run (dict: ReadOnlySpan<byte>) (state: State) : ImmutableArray<DictOp> voption =
        match state.Position >= dict.Length with
        | true -> ValueSome (Sealed.ofList (List.rev state.Ops))
        | false ->
            let opcode = int (Binary.u8 dict state.Position)
            let next = state.Position + 1
            let fail () = ValueNone
            match opcode with
            | 28 | 29 | 30 ->
                let parsed =
                    match opcode with
                    | 28 -> readShort dict next
                    | 29 -> readLong dict next
                    | _ -> parseBcd dict next
                match pushParsed state parsed with
                | ValueNone -> fail ()
                | ValueSome nextState -> run dict nextState
            | value when value >= 32 && value <= 246 ->
                match pushAt state next (double (value - 139)) with
                | ValueNone -> fail ()
                | ValueSome nextState -> run dict nextState
            | value when value >= 247 && value <= 250 ->
                let parsed =
                    match next < dict.Length with
                    | false -> ValueNone
                    | true -> ValueSome (struct (next + 1, double ((value - 247) * 256 + int (Binary.u8 dict next) + 108)))
                match pushParsed state parsed with
                | ValueNone -> fail ()
                | ValueSome nextState -> run dict nextState
            | value when value >= 251 && value <= 254 ->
                let parsed =
                    match next < dict.Length with
                    | false -> ValueNone
                    | true -> ValueSome (struct (next + 1, double (-((value - 251) * 256) - int (Binary.u8 dict next) - 108)))
                match pushParsed state parsed with
                | ValueNone -> fail ()
                | ValueSome nextState -> run dict nextState
            | 12 ->
                match next < dict.Length with
                | false -> run dict { state with Position = next; Stack = []; Count = 0 }
                | true ->
                    let emitted = emit (1200 + int (Binary.u8 dict next)) state.Stack state.Ops
                    run dict { emitted with Position = next + 1 }
            | 22 | 24 ->
                let emitted = emit opcode state.Stack state.Ops
                run dict { emitted with Position = next }
            | value when value < 22 ->
                let emitted = emit value state.Stack state.Ops
                run dict { emitted with Position = next }
            | _ -> run dict { state with Position = next; Stack = []; Count = 0 }
    /// <summary>Interprets a DICT as operators with their operands, <c>dict_interpreter_t::interpret</c>.</summary>
    /// <param name="dict">The DICT bytes.</param>
    /// <returns>The operators, or none on error.</returns>
    let interpret (dict: ReadOnlySpan<byte>) : ImmutableArray<DictOp> voption =
        run dict { Position = 0; Stack = []; Count = 0; Ops = [] }

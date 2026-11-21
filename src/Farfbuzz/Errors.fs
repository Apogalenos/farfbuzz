namespace Farfbuzz

/// <summary>Why a textual input was rejected. Positions are byte offsets into the UTF-8 encoding of the input.</summary>
type ParseError =
    | UnexpectedEnd of position: int * expected: string  // the input stopped where more was required
    | UnexpectedByte of position: int * found: byte * expected: string
    | InvalidToken of position: int * token: string * reason: string
    | TrailingInput of position: int  // input left over after a complete value

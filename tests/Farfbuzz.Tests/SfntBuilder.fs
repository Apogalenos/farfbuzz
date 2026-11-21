module Farfbuzz.Tests.SfntBuilder

open System
open Farfbuzz

/// <summary>Writes a big-endian uint16 as two bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let be16 (value: uint32) : byte array =
    [| byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian 24-bit value as three bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let be24 (value: uint32) : byte array =
    [| byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Writes a big-endian uint32 as four bytes.</summary>
/// <param name="value">The value.</param>
/// <returns>The bytes.</returns>
let be32 (value: uint32) : byte array =
    [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// <summary>Builds a bare sfnt with a table directory and the tables in order.</summary>
/// <param name="version">The sfnt version tag.</param>
/// <param name="tables">The tables.</param>
/// <returns>The file bytes.</returns>
let sfnt (version: uint32) (tables: (Tag * byte array) list) : byte array =
    let headerSize = 12 + 16 * tables.Length
    let header =
        Array.concat
            [ be32 version
              be16 (uint32 tables.Length)
              be16 0u
              be16 0u
              be16 0u ]
    let records, _ =
        tables
        |> List.fold (fun (records, offset) (tag, bytes) ->
            let record =
                Array.concat
                    [ be32 (uint32 tag)
                      be32 0u
                      be32 (uint32 offset)
                      be32 (uint32 bytes.Length) ]
            record :: records, offset + bytes.Length) ([], headerSize)
    Array.concat (header :: List.rev records @ (tables |> List.map snd))

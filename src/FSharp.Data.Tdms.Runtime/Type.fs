namespace FSharp.Data.Tdms

type Type =
  | Void
  | I8
  | I16
  | I32
  | I64
  | U8
  | U16
  | U32
  | U64
  | SingleFloat
  | DoubleFloat
  | ExtendedFloat
  | SingleFloatWithUnit
  | DoubleFloatWithUnit
  | ExtendedFloatWithUnit
  | String
  | Boolean
  | Timestamp
  | FixedPoint
  | ComplexSingleFloat
  | ComplexDoubleFloat
  | DaqMxRawData

module Type =
  
  open System
  open System.Numerics

  let id ``type`` =
    match ``type`` with
    | Type.Void -> 0u
    | Type.I8 -> 1u
    | Type.I16 -> 2u
    | Type.I32 -> 3u
    | Type.I64 -> 4u
    | Type.U8 -> 5u
    | Type.U16 -> 6u
    | Type.U32 -> 7u
    | Type.U64 -> 8u
    | Type.SingleFloat -> 9u
    | Type.DoubleFloat -> 10u
    | Type.ExtendedFloat -> 11u
    | Type.SingleFloatWithUnit -> 0x19u
    | Type.DoubleFloatWithUnit -> 0x1Au
    | Type.ExtendedFloatWithUnit -> 0x1Bu
    | Type.String -> 0x20u
    | Type.Boolean -> 0x21u
    | Type.Timestamp -> 0x44u
    | Type.FixedPoint -> 0x4Fu
    | Type.ComplexSingleFloat -> 0x08000Cu
    | Type.ComplexDoubleFloat -> 0x10000Du
    | Type.DaqMxRawData -> 0xFFFFFFFFu

  let ofId id =
      match id with
        | 0u -> Some Type.Void
        | 1u -> Some Type.I8
        | 2u -> Some Type.I16
        | 3u -> Some Type.I32
        | 4u -> Some Type.I64
        | 5u -> Some Type.U8
        | 6u -> Some Type.U16
        | 7u -> Some Type.U32
        | 8u -> Some Type.U64
        | 9u -> Some Type.SingleFloat
        | 10u -> Some Type.DoubleFloat
        | 11u -> Some Type.ExtendedFloat
        | 0x19u -> Some Type.SingleFloatWithUnit
        | 0x1Au -> Some Type.DoubleFloatWithUnit
        | 0x1Bu -> Some Type.ExtendedFloatWithUnit
        | 0x20u -> Some Type.String
        | 0x21u -> Some Type.Boolean
        | 0x44u -> Some Type.Timestamp
        | 0x4Fu -> Some Type.FixedPoint
        | 0x08000Cu -> Some Type.ComplexSingleFloat
        | 0x10000Du -> Some Type.ComplexDoubleFloat
        | 0xFFFFFFFFu -> Some Type.DaqMxRawData
        | _ -> None

  let size ``type`` =
    match ``type`` with
    | Type.Void | Type.Boolean | Type.I8 | Type.U8 -> Some 1u
    | Type.I16 | Type.U16 -> Some 2u
    | Type.I32 | Type.U32 | Type.SingleFloat | Type.SingleFloatWithUnit -> Some 4u
    | Type.I64 | Type.U64 | Type.DoubleFloat | Type.DoubleFloatWithUnit | Type.ComplexSingleFloat | Type.FixedPoint -> Some 8u
    | Type.ComplexDoubleFloat | Type.Timestamp | Type.ExtendedFloat | Type.ExtendedFloatWithUnit -> Some 16u
    | Type.String | Type.DaqMxRawData -> None
  
  let system ``type`` =
    match ``type`` with
    | Type.Void -> Some typeof<unit>
    | Type.Boolean -> Some typeof<bool>
    | Type.I8 -> Some typeof<int8>
    | Type.I16 -> Some typeof<int16>
    | Type.I32 -> Some typeof<int>
    | Type.I64 -> Some typeof<int64>
    | Type.U8 -> Some typeof<uint8>
    | Type.U16 -> Some typeof<uint16>
    | Type.U32 -> Some typeof<uint32>
    | Type.U64 -> Some typeof<uint64>
    | Type.SingleFloat | Type.SingleFloatWithUnit -> Some typeof<float32>
    | Type.DoubleFloat | Type.DoubleFloatWithUnit -> Some typeof<float>
    | Type.ComplexSingleFloat | Type.ComplexDoubleFloat -> Some typeof<Complex>
    | Type.String -> Some typeof<string>
    | Type.Timestamp -> Some typeof<DateTime>
    | Type.DaqMxRawData | Type.ExtendedFloat | Type.ExtendedFloatWithUnit | Type.FixedPoint -> None

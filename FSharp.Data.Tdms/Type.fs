namespace FSharp.Data.Tdms

type Type =
  | Void = 0u
  | I8 = 1u
  | I16 = 2u
  | I32 = 3u
  | I64 = 4u
  | U8 = 5u
  | U16 = 6u
  | U32 = 7u
  | U64 = 8u
  | SingleFloat = 9u
  | DoubleFloat = 10u
  | ExtendedFloat = 11u
  | SingleFloatWithUnit = 0x19u
  | DoubleFloatWithUnit = 0x1Au
  | ExtendedFloatWithUnit = 0x1Bu
  | String = 0x20u
  | Boolean = 0x21u
  | Timestamp = 0x44u
  | FixedPoint = 0x4Fu
  | ComplexSingleFloat = 0x08000Cu
  | ComplexDoubleFloat = 0x10000Du
  | DaqMxRawData = 0xFFFFFFFFu

module Type =
  
  open System
  open System.Numerics

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

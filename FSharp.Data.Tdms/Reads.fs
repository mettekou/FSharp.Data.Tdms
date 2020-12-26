namespace FSharp.Data.Tdms

open System
open System.IO
open System.Numerics

type Reads = {
  Void : BinaryReader -> unit
  Bool : BinaryReader -> bool
  Int8 : BinaryReader -> int8
  UInt8 : BinaryReader -> uint8
  Int16 : BinaryReader -> int16
  UInt16 : BinaryReader -> uint16
  Int32 : BinaryReader -> int32
  UInt32 : BinaryReader -> uint32
  Int64 : BinaryReader -> int64
  UInt64 : BinaryReader -> uint64
  Single : BinaryReader -> float32
  Double : BinaryReader -> float
  DoubleComplex : BinaryReader -> Complex
  String : BinaryReader -> string
  Timestamp : BinaryReader -> DateTime
  Type : BinaryReader -> FSharp.Data.Tdms.Type
  PropertyValue : FSharp.Data.Tdms.Type -> BinaryReader -> obj
}

module Reads =
  open System.Text

  let readVoid (reader : BinaryReader) = reader.ReadByte() |> ignore

  let readBoolean (reader : BinaryReader) = reader.ReadBoolean()

  let readInt8 (reader : BinaryReader) = reader.ReadSByte()

  let readUInt8 (reader : BinaryReader) = reader.ReadByte()

  let readInt16 (reader : BinaryReader) = reader.ReadInt16()

  let readUInt16 (reader : BinaryReader) = reader.ReadUInt16()

  let readInt32 (reader : BinaryReader) = reader.ReadInt32()

  let readInt64 (reader : BinaryReader) = reader.ReadInt64()

  let readUInt32 (reader : BinaryReader) = reader.ReadUInt32()

  let readUInt64 (reader : BinaryReader) = reader.ReadUInt64()
  
  let readSingleFloat (reader : BinaryReader) = reader.ReadSingle()

  let readDoubleFloat (reader : BinaryReader) = reader.ReadDouble()

  let readDoubleComplex (reader : BinaryReader) = Complex(reader.ReadDouble(), reader.ReadDouble())

  let readType (reader : BinaryReader) = readUInt32 reader |> Type.ofId |> Option.defaultValue Type.Void

  let readString (reader : BinaryReader) =
    #if NETCORE2_0 || NETSTANDARD2_0
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
    #endif
    readUInt32 reader |> int |> reader.ReadBytes |> Encoding.GetEncoding(1252).GetString

  let readTimestamp (reader : BinaryReader) =
    DateTime(1904, 1, 1, 0, 0, 0, DateTimeKind.Utc)
      .AddSeconds(float (readUInt64 reader) / float(UInt64.MaxValue))
      .AddSeconds(float (readInt64 reader))
      .ToLocalTime()

  let readPropertyValue tdsType (reader : BinaryReader) =
    match tdsType with
      | Type.Void -> readVoid |> box
      | Type.Boolean -> readBoolean |> box
      | Type.I8 -> readInt8 reader |> box
      | Type.U8 -> readUInt8 reader |> box
      | Type.I16 -> readInt16 reader |> box
      | Type.U16 -> readUInt16 reader |> box
      | Type.I32 -> readInt32 reader |> box
      | Type.U32 -> readUInt32 reader |> box
      | Type.I64 -> readInt64 reader |> box
      | Type.U64 -> readUInt64 reader |> box
      | Type.SingleFloat -> readSingleFloat reader |> box
      | Type.DoubleFloat -> readDoubleFloat reader |> box
      | Type.ComplexDoubleFloat -> readDoubleComplex reader |> box
      | Type.String -> readString reader |> box
      | Type.Timestamp -> readTimestamp reader |> box

  let readInt16Big (reader : BinaryReader) = BitConverter.ToInt16(reader.ReadBytes 2 |> Array.rev, 0)

  let readUInt16Big (reader : BinaryReader) = BitConverter.ToUInt16(reader.ReadBytes 2 |> Array.rev, 0)

  let readInt32Big (reader : BinaryReader) = BitConverter.ToInt32(reader.ReadBytes 4 |> Array.rev, 0)

  let readInt64Big (reader : BinaryReader) = BitConverter.ToInt64(reader.ReadBytes 8 |> Array.rev, 0)

  let readUInt32Big (reader : BinaryReader) = BitConverter.ToUInt32(reader.ReadBytes 4 |> Array.rev, 0)

  let readUInt64Big (reader : BinaryReader) = BitConverter.ToUInt64(reader.ReadBytes 8 |> Array.rev, 0)
  
  let readSingleFloatBig (reader : BinaryReader) = BitConverter.ToSingle(reader.ReadBytes 4 |> Array.rev, 0)

  let readDoubleFloatBig (reader : BinaryReader) = BitConverter.ToDouble(reader.ReadBytes 8 |> Array.rev, 0)

  let readDoubleComplexBig (reader : BinaryReader) = Complex(BitConverter.ToDouble(reader.ReadBytes 8 |> Array.rev, 0), BitConverter.ToDouble(reader.ReadBytes 8 |> Array.rev, 0))

  let readTypeBig (reader : BinaryReader) = readUInt32Big reader |> Type.ofId |> Option.defaultValue Type.Void

  let readStringBig (reader : BinaryReader) = readUInt32Big reader |> int |> reader.ReadBytes |> Encoding.UTF8.GetString

  let readTimestampBig (reader : BinaryReader) =
    DateTime(1904, 1, 1, 0, 0, 0, DateTimeKind.Utc)
      .AddSeconds(float (readUInt64Big reader) / float(UInt64.MaxValue))
      .AddSeconds(float (readInt64Big reader))
      .ToLocalTime()

  let readPropertyValueBig tdsType (reader : BinaryReader) =
    match tdsType with
      | Type.Void -> readVoid |> box
      | Type.Boolean -> readBoolean |> box
      | Type.I8 -> readInt8 reader |> box
      | Type.U8 -> readUInt8 reader |> box
      | Type.I16 -> readInt16Big reader |> box
      | Type.U16 -> readUInt16Big reader |> box
      | Type.I32 -> readInt32Big reader |> box
      | Type.U32 -> readUInt32Big reader |> box
      | Type.I64 -> readInt64Big reader |> box
      | Type.U64 -> readUInt64Big reader |> box
      | Type.SingleFloat -> readSingleFloatBig reader |> box
      | Type.DoubleFloat -> readDoubleFloatBig reader |> box
      | Type.ComplexDoubleFloat -> readDoubleComplexBig reader |> box
      | Type.String -> readStringBig reader |> box
      | Type.Timestamp -> readTimestampBig reader |> box

  let littleEndianMappings = { Void = readVoid; Bool = readBoolean; Int8 = readInt8; UInt8 = readUInt8; Int16 = readInt16; UInt16 = readUInt16; Int32 = readInt32; UInt32 = readUInt32; Int64 = readInt64; UInt64 = readUInt64; Single = readSingleFloat; Double = readDoubleFloat; DoubleComplex = readDoubleComplex; String = readString; Timestamp = readTimestamp; Type = readType; PropertyValue = readPropertyValue }

  let bigEndianMappings = { Void = readVoid; Bool = readBoolean; Int8 = readInt8; UInt8 = readUInt8; Int16 = readInt16Big; UInt16 = readUInt16Big; Int32 = readInt32Big; UInt32 = readUInt32Big; Int64 = readInt64Big; UInt64 = readUInt64Big; Single = readSingleFloatBig; Double = readDoubleFloatBig; String = readStringBig; DoubleComplex = readDoubleComplexBig; Timestamp = readTimestampBig; Type = readTypeBig; PropertyValue = readPropertyValueBig }
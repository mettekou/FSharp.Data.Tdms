namespace FSharp.Data.Tdms

open System
open System.IO

type Reads = {
  Int32 : BinaryReader -> int32
  UInt32 : BinaryReader -> uint32
  UInt64 : BinaryReader -> uint64
  Single : BinaryReader -> float32
  Double : BinaryReader -> float
  String : BinaryReader -> string
  Timestamp : BinaryReader -> DateTime
  Type : BinaryReader -> FSharp.Data.Tdms.Type
  PropertyValue : FSharp.Data.Tdms.Type -> BinaryReader -> obj
}

module Reads =
  open System.Text

  let readInt32 (reader : BinaryReader) = reader.ReadInt32()

  let readInt64 (reader : BinaryReader) = reader.ReadInt64()

  let readUInt32 (reader : BinaryReader) = reader.ReadUInt32()

  let readUInt64 (reader : BinaryReader) = reader.ReadUInt64()
  
  let readSingleFloat (reader : BinaryReader) = reader.ReadSingle()

  let readDoubleFloat (reader : BinaryReader) = reader.ReadDouble()

  let readType (reader : BinaryReader) = readUInt32 reader |> LanguagePrimitives.EnumOfValue

  let readString (reader : BinaryReader) = readUInt32 reader |> int |> reader.ReadBytes |> Encoding.UTF8.GetString

  let readTimestamp (reader : BinaryReader) =
    DateTime(1904, 1, 1, 0, 0, 0, DateTimeKind.Utc)
      .AddSeconds(float (readUInt64 reader) / float(UInt64.MaxValue))
      .AddSeconds(float (readInt64 reader))
      .ToLocalTime()

  let readPropertyValue tdsType (reader : BinaryReader) =
    match tdsType with
      | Type.I32 -> readInt32 reader |> box
      | Type.DoubleFloat -> readDoubleFloat reader |> box
      | Type.String -> readString reader |> box
      | Type.Timestamp -> readTimestamp reader |> box
      | x -> printfn "%A" x |> box

  let readInt32Big (reader : BinaryReader) = BitConverter.ToInt32(reader.ReadBytes 4 |> Array.rev, 0)

  let readInt64Big (reader : BinaryReader) = BitConverter.ToInt64(reader.ReadBytes 8 |> Array.rev, 0)

  let readUInt32Big (reader : BinaryReader) = BitConverter.ToUInt32(reader.ReadBytes 4 |> Array.rev, 0)

  let readUInt64Big (reader : BinaryReader) = BitConverter.ToUInt64(reader.ReadBytes 8 |> Array.rev, 0)
  
  let readSingleFloatBig (reader : BinaryReader) = BitConverter.ToSingle(reader.ReadBytes 4 |> Array.rev, 0)

  let readDoubleFloatBig (reader : BinaryReader) = BitConverter.ToDouble(reader.ReadBytes 8 |> Array.rev, 0)

  let readTypeBig (reader : BinaryReader) = readUInt32Big reader |> LanguagePrimitives.EnumOfValue

  let readStringBig (reader : BinaryReader) = readUInt32Big reader |> int |> reader.ReadBytes |> Encoding.UTF8.GetString

  let readTimestampBig (reader : BinaryReader) =
    DateTime(1904, 1, 1, 0, 0, 0, DateTimeKind.Utc)
      .AddSeconds(float (readUInt64Big reader) / float(UInt64.MaxValue))
      .AddSeconds(float (readInt64Big reader))
      .ToLocalTime()

  let readPropertyValueBig tdsType (reader : BinaryReader) =
    match tdsType with
      | Type.I32 -> readInt32Big reader :> obj
      | Type.DoubleFloat -> readDoubleFloatBig reader :> obj
      | Type.String -> readStringBig reader :> obj
      | Type.Timestamp -> readTimestampBig reader :> obj
      | x -> printfn "%A" x :> obj

  let littleEndianMappings = { Int32 = readInt32; UInt32 = readUInt32; UInt64 = readUInt64; Single = readSingleFloat; Double = readDoubleFloat; String = readString; Timestamp = readTimestamp; Type = readType; PropertyValue = readPropertyValue }

  let bigEndianMappings = { Int32 = readInt32Big; UInt32 = readUInt32Big; UInt64 = readUInt64Big; Single = readSingleFloatBig; Double = readDoubleFloatBig; String = readStringBig; Timestamp = readTimestampBig; Type = readTypeBig; PropertyValue = readPropertyValueBig }
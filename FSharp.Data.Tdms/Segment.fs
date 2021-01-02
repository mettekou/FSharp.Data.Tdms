namespace FSharp.Data.Tdms

open System
open System.Buffers
open System.Buffers.Binary
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Text
open FSharp.Control.Tasks.NonAffine

type Tag =
  | Tdsm = 1834173524u
  | Tdsh = 1750287444u

[<Flags>]
type TableOfContents =
  | ContainsMetaData = 2u
  | ContainsRawData = 8u
  | ContainsDaqMxRawData = 128u
  | ContainsInterleavedData = 32u
  | ContainsBigEndianData = 64u
  | ContainsNewObjectList = 4u

type Version =
  | ``1.0`` = 4712u
  | ``2.0`` = 4713u

[<Struct>]
type LeadIn =
  { Tag: Tag
    TableOfContents : TableOfContents
    Version : Version
    NextSegmentOffset : uint64
    RawDataOffset : uint64 }

type Index = {
    Objects: FSharp.Data.Tdms.Object List
}

module Segment =

  let readInt16 (buffer: byte ReadOnlySpan byref) bigEndian =
    let value = if bigEndian then BinaryPrimitives.ReadInt16BigEndian buffer else BinaryPrimitives.ReadInt16LittleEndian buffer
    buffer <- buffer.Slice 2
    value

  let readInt (buffer: byte ReadOnlySpan byref) bigEndian =
    let value = if bigEndian then BinaryPrimitives.ReadInt32BigEndian buffer else BinaryPrimitives.ReadInt32LittleEndian buffer
    buffer <- buffer.Slice 4
    value

  let readInt64 (buffer: byte ReadOnlySpan byref) bigEndian =
    let value = if bigEndian then BinaryPrimitives.ReadInt64BigEndian buffer else BinaryPrimitives.ReadInt64LittleEndian buffer
    buffer <- buffer.Slice 8
    value

  let readUInt16 (buffer: byte ReadOnlySpan byref) bigEndian =
    let value = if bigEndian then BinaryPrimitives.ReadUInt16BigEndian buffer else BinaryPrimitives.ReadUInt16LittleEndian buffer
    buffer <- buffer.Slice 2
    value

  let readUInt (buffer: byte ReadOnlySpan byref) bigEndian =
    let value = if bigEndian then BinaryPrimitives.ReadUInt32BigEndian buffer else BinaryPrimitives.ReadUInt32LittleEndian buffer
    buffer <- buffer.Slice 4
    value

  let readUInt64 (buffer: byte ReadOnlySpan byref) bigEndian =
    let value = if bigEndian then BinaryPrimitives.ReadUInt64BigEndian buffer else BinaryPrimitives.ReadUInt64LittleEndian buffer
    buffer <- buffer.Slice 8
    value
  
  let readFloat32 (buffer: byte ReadOnlySpan byref) bigEndian =
    let value = if bigEndian then BinaryPrimitives.ReadSingleBigEndian buffer else BinaryPrimitives.ReadSingleLittleEndian buffer
    buffer <- buffer.Slice 4
    value

  let readFloat (buffer: byte ReadOnlySpan byref) bigEndian =
    let value = if bigEndian then BinaryPrimitives.ReadDoubleBigEndian buffer else BinaryPrimitives.ReadDoubleLittleEndian buffer
    buffer <- buffer.Slice 8
    value

  let readType (buffer: byte ReadOnlySpan byref) bigEndian =
    readUInt &buffer bigEndian |> LanguagePrimitives.EnumOfValue<uint32, FSharp.Data.Tdms.Type>

  let readString (buffer: byte ReadOnlySpan byref) bigEndian =
    let length = readUInt &buffer bigEndian |> int
    let bytes = buffer.Slice(0, length)
    buffer <- buffer.Slice length
    Encoding.GetEncoding(1252).GetString bytes

  let readLeadIn (buffer: byte ReadOnlySpan byref) =
    let tag = readUInt &buffer false |> LanguagePrimitives.EnumOfValue<uint32, Tag>
    let tableOfContents = readUInt &buffer false |> LanguagePrimitives.EnumOfValue<uint32, TableOfContents>
    let bigEndian = tableOfContents.HasFlag(TableOfContents.ContainsBigEndianData)
    { Tag = tag
      TableOfContents = tableOfContents
      Version = readUInt &buffer bigEndian |> LanguagePrimitives.EnumOfValue<uint32, Version>
      NextSegmentOffset = readUInt64 &buffer bigEndian
      RawDataOffset = readUInt64 &buffer bigEndian }

  let readLeadInMemory (memory: byte ReadOnlyMemory) =
    let mutable buffer = memory.Span
    readLeadIn &buffer

  let readPropertyValue (buffer: byte ReadOnlySpan byref) bigEndian =
    function
    | Type.Void ->
      buffer <- buffer.Slice 1
      box ()
    | Type.Boolean ->
      let value = box (buffer.[0] <> 0uy)
      buffer <- buffer.Slice 1
      value
    | Type.I8 -> 
      let value = box (MemoryMarshal.Cast<byte, sbyte> buffer).[0]
      buffer <- buffer.Slice 1
      value
    | Type.I16 -> readInt16 &buffer bigEndian |> box
    | Type.I32 -> readInt &buffer bigEndian |> box
    | Type.I64 -> readInt64 &buffer bigEndian |> box
    | Type.U8 -> 
      let value = box buffer.[0]
      buffer <- buffer.Slice 1
      value
    | Type.U16 -> readUInt16 &buffer bigEndian |> box
    | Type.U32 -> readUInt &buffer bigEndian |> box
    | Type.U64 -> readUInt64 &buffer bigEndian |> box
    | Type.SingleFloat | Type.SingleFloatWithUnit -> readFloat32 &buffer bigEndian |> box
    | Type.DoubleFloat | Type.DoubleFloatWithUnit -> readFloat &buffer bigEndian |> box
    | Type.ComplexSingleFloat -> Complex(readFloat32 &buffer bigEndian |> float, readFloat32 &buffer bigEndian |> float) |> box
    | Type.ComplexDoubleFloat -> Complex(readFloat &buffer bigEndian, readFloat &buffer bigEndian) |> box
    | Type.Timestamp ->
        (if bigEndian
         then { SecondsSinceNiEpoch = readInt64 &buffer bigEndian; FractionsOfASecond = readUInt64 &buffer bigEndian }
         else { FractionsOfASecond = readUInt64 &buffer bigEndian; SecondsSinceNiEpoch = readInt64 &buffer bigEndian }) |> box
    | Type.String -> readString &buffer bigEndian |> box

  let readRawDataIndex object (rawDataPosition: uint64) (buffer: byte ReadOnlySpan byref) bigEndian =
    match readUInt &buffer bigEndian with
      | 0u ->
        match object.LastRawDataIndex with
        | None -> failwith "Missing raw data index"
        | Some (OtherType(ty, dimension, length)) ->
          let bytes = length
          object.RawDataBlocks.Add (rawDataPosition, bytes)
          rawDataPosition + bytes
        | Some (String(ty, dimension, length, bytes)) ->
          object.RawDataBlocks.Add (rawDataPosition, bytes)
          rawDataPosition + bytes
      | 20u -> 
        let ty = readType &buffer bigEndian
        let dimension = readUInt &buffer bigEndian
        let length = readUInt64 &buffer bigEndian
        let bytes = length
        object.RawDataBlocks.Add (rawDataPosition, bytes)
        object.LastRawDataIndex <- Some (OtherType(ty, dimension, length))
        rawDataPosition + bytes
      | 28u ->
        let ty = readType &buffer bigEndian
        let dimension = readUInt &buffer bigEndian
        let length = readUInt64 &buffer bigEndian
        let bytes = readUInt64 &buffer bigEndian
        object.RawDataBlocks.Add (rawDataPosition, bytes)
        object.LastRawDataIndex <- Some (RawDataIndex.String(ty, dimension, length, bytes))
        rawDataPosition + bytes
      | 0xFFFFFFFFu -> rawDataPosition
      | 0x69120000u | 0x69130000u -> failwith "DAQmx raw data not implemented"
      | length -> failwithf "Invalid raw data index length: %i" length

  let createObject name (objects: _ List) bigEndian =
    match Seq.cast objects |> Seq.tryFind (fun object -> object.Name = name) with
    | Some object -> object
    | None ->
      let object = { Name = name; BigEndian = bigEndian; LastRawDataIndex = None; RawDataBlocks = List(); Properties = List() }
      objects.Add object
      object

  let readMetaData { Objects = objects } (rawDataOffset: uint64) (buffer: byte ReadOnlySpan byref) bigEndian =
    let objectCount = readInt &buffer bigEndian
    let mutable rawDataPosition = rawDataOffset
    for i = 0 to objectCount - 1 do
        let object = createObject (readString &buffer bigEndian) objects bigEndian
        rawDataPosition <- readRawDataIndex object rawDataPosition &buffer bigEndian
        let propertyCount = readUInt &buffer bigEndian |> int
        for j = 0 to propertyCount - 1 do
            let propertyName = readString &buffer bigEndian
            let propertyType = readType &buffer bigEndian
            let propertyValue = readPropertyValue &buffer bigEndian propertyType
            let property = { Name = propertyName; Value = { Type = propertyType; Raw = propertyValue } }
            match Seq.tryFindIndex (fun (property: Property) -> property.Name = propertyName) object.Properties with
              | None -> object.Properties.Add property
              | Some index -> object.Properties.[index] <- property

  let readMetaDataMemory index (rawDataOffset: uint64) (memory: byte ReadOnlyMemory) bigEndian =
    let mutable buffer = memory.Span
    readMetaData index rawDataOffset &buffer bigEndian

  (*let chunkDataSize indices = List.sum (List.map (fun i ->
      match i with
        | None -> 0uL
        | Some(OtherType(ty, d, c)) -> uint64 (Type.size ty |> Option.defaultValue 1u) * uint64 d * c
        | Some(String(_, _, _, s)) -> s
    ) indices)

  let countChunks indices nextSegmentOffset rawDataOffset =
    let totalDataSize = nextSegmentOffset - rawDataOffset
    let chunkSize = chunkDataSize indices
    if chunkSize = 0uL then 0uL else totalDataSize / chunkSize*)

  let tdsh = [| 0x54uy; 0x44uy; 0x53uy; 0x68uy |]

  let read offset fromIndex index leadInBuffer (stream : Stream) (indexStream : Stream) =
    stream.Read(leadInBuffer, 0, 28) |> ignore
    let mutable leadInSpan = ReadOnlySpan leadInBuffer
    let writeIndex = isNull indexStream |> not
    if writeIndex
    then
      indexStream.Write(tdsh, 0, 4)
      indexStream.Write(leadInBuffer, 4, 24)
    let leadIn = readLeadIn &leadInSpan
    let metaDataStart = offset + 28uL
    if leadIn.TableOfContents.HasFlag(TableOfContents.ContainsMetaData)
    then
      let remainingLength = int leadIn.RawDataOffset
      let buffer = ArrayPool<byte>.Shared.Rent remainingLength
      stream.Read(buffer, 0, remainingLength) |> ignore
      let mutable span = ReadOnlySpan buffer
      if writeIndex then indexStream.Write(buffer, 0, remainingLength)
      readMetaData index (metaDataStart + leadIn.RawDataOffset) &span (leadIn.TableOfContents.HasFlag(TableOfContents.ContainsBigEndianData))
      ArrayPool<byte>.Shared.Return(buffer, false)
    let nextSegmentOffset = metaDataStart + leadIn.NextSegmentOffset
    if not fromIndex then stream.Seek(int64 nextSegmentOffset, SeekOrigin.Begin) |> ignore
    nextSegmentOffset

  let readAsync offset fromIndex index leadInBuffer (stream : Stream) (indexStream : Stream) =
    task {
      let! _ = stream.ReadAsync(leadInBuffer, 0, 28)
      let mutable leadInMemory = ReadOnlyMemory leadInBuffer
      let writeIndex = isNull indexStream |> not
      if writeIndex
      then
        do! indexStream.WriteAsync(tdsh, 0, 4)
        do! indexStream.WriteAsync(leadInBuffer, 4, 24)
      let leadIn = readLeadInMemory leadInMemory
      let metaDataStart = offset + 28uL
      if leadIn.TableOfContents.HasFlag(TableOfContents.ContainsMetaData)
      then
        let remainingLength = int leadIn.RawDataOffset
        let buffer = ArrayPool<byte>.Shared.Rent remainingLength
        let! _ = stream.ReadAsync(buffer, 0, remainingLength)
        let mutable memory = ReadOnlyMemory buffer
        if writeIndex then do! indexStream.WriteAsync(buffer, 0, remainingLength)
        readMetaDataMemory index (metaDataStart + leadIn.RawDataOffset) memory (leadIn.TableOfContents.HasFlag(TableOfContents.ContainsBigEndianData))
        ArrayPool<byte>.Shared.Return(buffer, false)
      let nextSegmentOffset = metaDataStart + leadIn.NextSegmentOffset
      if not fromIndex then stream.Seek(int64 nextSegmentOffset, SeekOrigin.Begin) |> ignore
      return nextSegmentOffset
    }
namespace FSharp.Data.Tdms

open System
open System.Buffers
open System.Buffers.Binary
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Text

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

type RawDataIndex =
  | AsBefore
  | String of FSharp.Data.Tdms.Type * uint32 * uint64 * uint64
  | OtherType of FSharp.Data.Tdms.Type * uint32 * uint64

type Object = {
  Name : string
  RawDataIndex : RawDataIndex option
  Properties : Property []
 }

type Segment = {
  LeadIn : LeadIn
  Objects : Object list
  Offset : uint64
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
    { Tag = readUInt &buffer false |> LanguagePrimitives.EnumOfValue<uint32, Tag>
      TableOfContents = readUInt &buffer false |> LanguagePrimitives.EnumOfValue<uint32, TableOfContents>
      Version = readUInt &buffer false |> LanguagePrimitives.EnumOfValue<uint32, Version>
      NextSegmentOffset = readUInt64 &buffer false
      RawDataOffset = readUInt64 &buffer false }

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
      DateTime(1904, 1, 1, 0, 0, 0, DateTimeKind.Utc)
        .AddSeconds(float (readUInt64 &buffer bigEndian) / float(UInt64.MaxValue))
        .AddSeconds(float (readInt64 &buffer bigEndian))
        .ToLocalTime()
        |> box
    | Type.String -> readString &buffer bigEndian |> box

  let readRawDataIndex name previous (buffer: byte ReadOnlySpan byref) bigEndian =
    match readUInt &buffer bigEndian with
      | 0u -> Some AsBefore
      | 20u -> OtherType(readType &buffer bigEndian, readUInt &buffer bigEndian, readUInt64 &buffer bigEndian) |> Some
      | 28u -> String(readType &buffer bigEndian, readUInt &buffer bigEndian, readUInt64 &buffer bigEndian, readUInt64 &buffer bigEndian) |> Some
      | _ -> None

  let readMetaData previousSegment (buffer: byte ReadOnlySpan byref) bigEndian =
    let objectCount = readInt &buffer bigEndian
    let objects = Array.zeroCreate<Object> objectCount
    for i = 0 to objectCount - 1 do
        let name = readString &buffer bigEndian
        let rawDataIndex = readRawDataIndex name previousSegment &buffer bigEndian
        let propertyCount = readUInt &buffer bigEndian |> int
        let properties = Array.zeroCreate<Property> propertyCount
        for j = 0 to propertyCount - 1 do
            let propertyName = readString &buffer bigEndian
            let propertyType = readType &buffer bigEndian
            let propertyValue = readPropertyValue &buffer bigEndian propertyType
            properties.[j] <- { Name = propertyName; Value = { Type = propertyType; Raw = propertyValue } }
        objects.[i] <- { Name = name; RawDataIndex = rawDataIndex; Properties = properties }
    objects |> Array.toList

  let merge previousObjects objects =
    let _, newObjects = List.partition (fun o -> List.tryFind (fun o' -> o'.Name = o.Name) previousObjects |> Option.isNone) objects
    List.append (List.fold (fun os o ->
      List.tryFind (fun o' -> o'.Name = o.Name) objects |> Option.map (fun o -> o :: os) |> Option.defaultValue os
    ) [] previousObjects) newObjects

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

  let read fromIndex previousSegment leadInBuffer (stream : Stream) (indexStream : Stream) =
    let offset = if fromIndex then Option.map (fun s -> s.Offset + 28uL + s.LeadIn.NextSegmentOffset) previousSegment |> Option.defaultValue 0uL else uint64 stream.Position
    stream.Read(leadInBuffer, 0, 28) |> ignore
    let mutable leadInSpan = ReadOnlySpan leadInBuffer
    let writeIndex = isNull indexStream |> not
    if writeIndex
    then
      indexStream.Write(leadInBuffer, 0, 28)
    let leadIn = readLeadIn &leadInSpan
    let metaDataStart = stream.Position
    let objects =
      if leadIn.TableOfContents.HasFlag(TableOfContents.ContainsMetaData)
      then
        let remainingLength = int leadIn.RawDataOffset
        let buffer = ArrayPool<byte>.Shared.Rent remainingLength
        stream.Read(buffer, 0, remainingLength) |> ignore
        let mutable span = ReadOnlySpan buffer
        if writeIndex
        then
          indexStream.Write(buffer, 0, remainingLength)
        let os = readMetaData previousSegment &span (leadIn.TableOfContents.HasFlag(TableOfContents.ContainsBigEndianData))
        ArrayPool<byte>.Shared.Return(buffer, false)
        if leadIn.TableOfContents.HasFlag(TableOfContents.ContainsNewObjectList)
        then os
        else merge (Option.fold (fun _ s -> s.Objects) [] previousSegment) os
      else
        Option.map (fun s -> s.Objects) previousSegment |> Option.defaultValue []
    if not fromIndex then stream.Seek(metaDataStart + int64 leadIn.NextSegmentOffset, SeekOrigin.Begin) |> ignore
    { Offset = offset; LeadIn = leadIn; Objects = objects }
  
  let rawDataIndexFor previousObjects object =
    match object with
      | None -> None
      | Some o ->
          match o.RawDataIndex with
            | Some AsBefore -> Map.tryFind o.Name previousObjects |> Option.bind (fun o -> o.RawDataIndex)
            | Some _ -> o.RawDataIndex
            | None -> None

  let rawDataFor previousObjects (channel : string) (segment : Segment) =
    if not (segment.LeadIn.TableOfContents.HasFlag(TableOfContents.ContainsRawData))
    then []
    else
      let mutable position = segment.Offset
      let moi = List.tryFindIndex (fun o -> o.Name = channel) segment.Objects
      let mo = Option.bind (fun oi -> List.tryItem oi segment.Objects) moi
      match moi, mo with
        | None, _ | _, None -> []
        | Some oi, Some { RawDataIndex = mi } ->
            Option.fold (fun _ i ->
              match i with
                | AsBefore -> []
                | OtherType(ty, _, m) ->
                  position <- position + 28uL + segment.LeadIn.RawDataOffset
                  let before, _ = List.splitAt oi segment.Objects
                  //let indices = List.map (fun o -> rawDataIndexFor previousSegments (Some o)) segment.Objects
                  //let chunkCount = countChunks indices segment.LeadIn.NextSegmentOffset segment.LeadIn.RawDataOffset
                  //let chunkSize = chunkDataSize indices
                  List.iter (fun o ->
                        Option.iter (fun rdi ->
                          match rdi with
                            | AsBefore -> ()
                            | String _ -> raise (NotImplementedException ())
                            | OtherType(ty', _, m') -> position <- position + (uint64 (Type.size ty' |> Option.defaultValue 0u) * m'))
                         (rawDataIndexFor previousObjects (Some o))) before
                  [position, m]
                | String _ -> raise (NotImplementedException ())
            ) [] (rawDataIndexFor previousObjects mo)
  
  type ObjectPath =
    | Root
    | Group of string
    | Channel of string * string
  
  let structure (name : string) =
    match name.Split([| "'"; "/" |], StringSplitOptions.RemoveEmptyEntries) with
      | [||] -> Some Root
      | [| name |] -> Some (Group name)
      | [| groupName; name |] -> Some (Channel (groupName, name))
      | _ -> None
  
  let indexToType previousObjects (object : Object) =
    let indexToType' index =
      match index with
        | None | Some AsBefore -> None
        | Some (OtherType (ty, _, _)) -> Some ty
        | Some (String _) -> Some Type.String
    match object.RawDataIndex with
      | None -> None
      | Some AsBefore -> Map.tryFind object.Name previousObjects |> Option.bind (fun o -> o.RawDataIndex) |> indexToType'
      | Some (OtherType (ty, _, _)) -> Some ty
      | Some (String _) -> Some Type.String
  
  let addObject previousObjects segment ({ Properties = ps'; Groups = gs; } as index) ({ Name = n; Properties = ps'' } as object) =
    let ps = Array.map (fun (p : Property) -> p.Name, p.Value) ps'' |> Map.ofArray
    match structure n with
      | None -> index
      | Some Root -> { index with Properties = Map.fold (fun ps k v -> Map.add k v ps) ps' ps; Groups = gs }
      | Some (Group name') -> { index with Groups = Map.add name' { Properties = ps; Channels = Map.empty } gs }
      | Some (Channel (groupName, channelName)) ->
        let group = Map.tryFind groupName gs |> Option.defaultValue { Properties = Map.empty; Channels = Map.empty }
        let i = rawDataIndexFor previousObjects (Some object)
        let ty = indexToType previousObjects object |> Option.defaultValue Type.Void
        let channel = { BigEndian = segment.LeadIn.TableOfContents.HasFlag(TableOfContents.ContainsBigEndianData); Type = Type.system ty |> Option.defaultValue typeof<unit>; Properties = ps; RawDataBlocks = rawDataFor previousObjects n segment }
        { index with Groups = Map.add groupName { group with Channels = Map.add channelName channel group.Channels } index.Groups }
            
  let index previousObjects ({ LeadIn = l; Objects = os } as segment) =
    List.fold (addObject previousObjects segment) { Path = ""; Properties = Map.empty; Groups = Map.empty } os 
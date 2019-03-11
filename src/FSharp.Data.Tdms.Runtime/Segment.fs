namespace FSharp.Data.Tdms

open System
open System.Text

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

type LeadIn = {
  TableOfContents : TableOfContents
  Version : Version
  NextSegmentOffset : uint64
  RawDataOffset : uint64
 }

type RawDataIndex =
  | String of FSharp.Data.Tdms.Type * uint32 * uint64 * uint64
  | OtherType of FSharp.Data.Tdms.Type * uint32 * uint64

type Object = {
  Name : string
  RawDataIndex : RawDataIndex option
  Properties : Property list
 }

type Segment = {
  LeadIn : LeadIn
  Objects : Object list
  Offset : uint64
 }

module Segment =

  open System.IO

  let readTdsm (reader : BinaryReader) =
    reader.ReadBytes 4 |> ignore

  let readTableOfContents reader : TableOfContents =
    Reads.readUInt32 reader |> LanguagePrimitives.EnumOfValue

  let readVersion reader mappings =
    mappings.UInt32 reader |> LanguagePrimitives.EnumOfValue

  let readLeadIn reader =
    readTdsm reader
    let tableOfContents = readTableOfContents reader
    let mappings = if tableOfContents.HasFlag TableOfContents.ContainsBigEndianData then Reads.bigEndianMappings else Reads.littleEndianMappings
    { TableOfContents = tableOfContents; Version = readVersion reader mappings; NextSegmentOffset = mappings.UInt64 reader; RawDataOffset = mappings.UInt64 reader }, mappings

  let readRawDataIndex name previous reader mappings =
    match mappings.UInt32 reader with
      | 0u -> Option.bind (fun s -> List.tryFind (fun o -> o.Name = name) s.Objects) previous |> Option.bind (fun o -> o.RawDataIndex)
      | 20u -> OtherType(mappings.Type reader, mappings.UInt32 reader, mappings.UInt64 reader) |> Some
      | 28u -> String(mappings.Type reader, mappings.UInt32 reader, mappings.UInt64 reader, mappings.UInt64 reader) |> Some
      | _ -> None

  let readMetaData previousSegment reader mappings =
    let objectCount = mappings.UInt32 reader |> int
    [
      for i in 1..objectCount do
        let name = mappings.String reader
        let rawDataIndex = readRawDataIndex name previousSegment reader mappings
        let propertyCount = mappings.UInt32 reader |> int
        let properties = [
          for j in 1..propertyCount do
            let propertyName = mappings.String reader
            let propertyType = mappings.Type reader
            let propertyValue = mappings.PropertyValue propertyType reader
            yield { Name = propertyName; Value = { Type = propertyType; Raw = propertyValue } }
        ]
        yield { Name = name; RawDataIndex = rawDataIndex; Properties = properties }
    ]

  let tdsh = [| 0x54uy; 0x44uy; 0x53uy; 0x68uy |]

  let tdsm = [| 0x54uy; 0x44uy; 0x53uy; 0x6Duy |]

  let writeIndex (writer : BinaryWriter) segment =
    writer.Write(tdsh)
    writer.Write(uint32 segment.LeadIn.TableOfContents)
    writer.Write(uint32 segment.LeadIn.Version)
    writer.Write(segment.LeadIn.NextSegmentOffset)
    writer.Write(segment.LeadIn.RawDataOffset)
    writer.Write(uint32 segment.Objects.Length)
    List.iter (fun o ->
      //printfn "%s" o.Name
      let obs = Encoding.UTF8.GetBytes(o.Name)
      //Array.iter (printfn "%X") obs
      writer.Write(Array.length obs |> uint32)
      writer.Write(obs)
      match o.RawDataIndex with
        | None -> writer.Write(0xFFFFFFFFu)
        | Some(OtherType(ty, m, n)) ->
          writer.Write(20u)
          writer.Write(Type.id ty)
          writer.Write(m)
          writer.Write(n)
        | Some(String(ty, m, n, o)) ->
          writer.Write(28u)
          writer.Write(Type.id ty)
          writer.Write(m)
          writer.Write(n)
          writer.Write(o)
      writer.Write(List.length o.Properties |> uint32)
      List.iter (fun (p : Property) ->
        let pbs = Encoding.UTF8.GetBytes(p.Name)
        //printfn "%X" (Array.length obs)
        writer.Write(Array.length pbs |> uint32)
        writer.Write(pbs)
        writer.Write(Type.id p.Value.Type)
        match p.Value.Type with
          | Type.I32 -> writer.Write(p.Value.Raw :?> int32)
          | Type.I64 -> writer.Write(p.Value.Raw :?> int64)
          | Type.SingleFloat -> writer.Write(p.Value.Raw :?> float32)
          | Type.DoubleFloat -> writer.Write(p.Value.Raw :?> float)
          | Type.String ->
            let value = p.Value.Raw :?> string
            writer.Write(uint32 value.Length)
            writer.Write(value |> Encoding.UTF8.GetBytes))
          o.Properties
    ) segment.Objects

  let merge previousObjects objects =
    let updatedObjects, newObjects = List.partition (fun o -> List.tryFind (fun o' -> o'.Name = o.Name) previousObjects |> Option.isNone) objects
    List.append (List.fold (fun os o ->
      List.tryFind (fun o' -> o'.Name = o.Name) objects |> Option.map (fun o -> o :: os) |> Option.defaultValue os
    ) [] previousObjects) newObjects

  let chunkDataSize indices = List.sum (List.map (fun i ->
      match i with
        | None -> 0uL
        | Some(OtherType(ty, d, c)) -> uint64 (Type.size ty |> Option.defaultValue 1u) * uint64 d * c
        | Some(String(_, _, _, s)) -> s
    ) indices)

  let countChunks indices nextSegmentOffset rawDataOffset =
    let totalDataSize = nextSegmentOffset - rawDataOffset
    let chunkSize = chunkDataSize indices
    if chunkSize = 0uL then 0uL else totalDataSize / chunkSize

  let read previousSegment (reader : BinaryReader) =
    let offset = uint64 reader.BaseStream.Position
    let leadIn, mappings = readLeadIn reader
    let metaDataStart = reader.BaseStream.Position
    let objects =
      if leadIn.TableOfContents.HasFlag(TableOfContents.ContainsMetaData)
      then
        let os = readMetaData previousSegment reader mappings
        if leadIn.TableOfContents.HasFlag(TableOfContents.ContainsNewObjectList)
        then os
        else merge (Option.fold (fun _ s -> s.Objects) [] previousSegment) os
      else
        Option.map (fun s -> s.Objects) previousSegment |> Option.defaultValue []
    reader.BaseStream.Seek(metaDataStart + int64 leadIn.NextSegmentOffset, SeekOrigin.Begin) |> ignore
    { Offset = offset; LeadIn = leadIn; Objects = objects }
  
  let rawDataIndexFor previousObjects object =
    match object with
      | None -> None
      | Some o ->
          match o.RawDataIndex with
            | Some _ -> o.RawDataIndex
            | None -> Map.tryFind o.Name previousObjects |>
                      Option.bind (fun o -> o.RawDataIndex)

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
                | OtherType(ty, _, m) ->
                  position <- position + 28uL + segment.LeadIn.RawDataOffset
                  let before, _ = List.splitAt oi segment.Objects
                  //let indices = List.map (fun o -> rawDataIndexFor previousSegments (Some o)) segment.Objects
                  //let chunkCount = countChunks indices segment.LeadIn.NextSegmentOffset segment.LeadIn.RawDataOffset
                  //let chunkSize = chunkDataSize indices
                  List.iter (fun o ->
                        Option.iter (fun rdi ->
                          match rdi with
                            | String _ -> ()
                            | OtherType(ty', _, m') -> position <- position + (uint64 (Type.size ty' |> Option.defaultValue 0u) * m'))
                         (rawDataIndexFor previousObjects (Some o))) before
                  [position, m]
                | String _ -> []
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
  
  let indexToType index =
    match index with
      | OtherType (ty, _, _) -> ty
      | String _ -> Type.String
  
  let typeToRead ``type`` bigEndian =
    let mappings = if bigEndian then Reads.bigEndianMappings else Reads.littleEndianMappings
    //printfn "%A" ``type``
    match ``type`` with
      | Type.Void -> (fun (r : BinaryReader) -> r.ReadByte()) >> ignore >> box
      | Type.SingleFloat -> mappings.Single >> box
      | Type.DoubleFloat -> mappings.Double >> box
  
  let addObject previousObjects segment ({ Properties = ps'; Groups = gs; } as index) ({ Name = n; Properties = ps'' } as object) =
    let ps = List.map (fun (p : Property) -> p.Name, p.Value) ps'' |> Map.ofList
    match structure n with
      | None -> index
      | Some Root -> { index with Properties = Map.fold (fun ps k v -> Map.add k v ps) ps' ps; Groups = gs }
      | Some (Group name') -> { index with Groups = Map.add name' { Properties = ps; Channels = Map.empty } gs }
      | Some (Channel (groupName, channelName)) ->
        let group = Map.tryFind groupName gs |> Option.defaultValue { Properties = Map.empty; Channels = Map.empty }
        let i = rawDataIndexFor previousObjects (Some object)
        let ty = Option.fold (fun _ i' -> indexToType i') Type.Void i
        let channel = { Read = typeToRead ty (segment.LeadIn.TableOfContents.HasFlag(TableOfContents.ContainsBigEndianData)); Type = Type.system ty |> Option.defaultValue typeof<unit>; Properties = ps; RawDataBlocks = rawDataFor previousObjects n segment }
        { index with Groups = Map.add groupName { group with Channels = Map.add channelName channel group.Channels } index.Groups }
            
  let index previousObjects ({ LeadIn = l; Objects = os } as segment) =
    List.fold (addObject previousObjects segment) { Path = ""; Properties = Map.empty; Groups = Map.empty } os 
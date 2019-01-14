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

type Property = {
  Name : string
  Value : Value
 }

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
          writer.Write(uint32 ty)
          writer.Write(m)
          writer.Write(n)
        | Some(String(ty, m, n, o)) ->
          writer.Write(28u)
          writer.Write(uint32 ty)
          writer.Write(m)
          writer.Write(n)
          writer.Write(o)
      writer.Write(List.length o.Properties |> uint32)
      List.iter (fun (p : Property) ->
        let pbs = Encoding.UTF8.GetBytes(p.Name)
        //printfn "%X" (Array.length obs)
        writer.Write(Array.length pbs |> uint32)
        writer.Write(pbs)
        writer.Write(uint32 p.Value.Type)
        match p.Value.Type with
          | Type.I32 -> writer.Write(p.Value.Raw :?> int32)
          | Type.I64 -> writer.Write(p.Value.Raw :?> int64)
          | Type.SingleFloat -> writer.Write(p.Value.Raw :?> float32)
          | Type.DoubleFloat -> writer.Write(p.Value.Raw :?> float)
          ) (*| Type.String ->
            let value = p.Value.Raw :?> string
            writer.Write(uint32 value.Length)
            writer.Write(value |> Encoding.UTF8.GetBytes)*)
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
        | Some(OtherType(ty, d, c)) -> uint64 (Type.size ty) * uint64 d * c
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

  let rawDataIndexFor' object objects =
    List.tryFind (fun o -> o.Name = object.Name) objects |> Option.bind (fun o -> o.RawDataIndex)
  
  let rawDataIndexFor previousSegments object =
    match object with
      | None -> None
      | Some o ->
          match o.RawDataIndex with
            | Some _ -> o.RawDataIndex
            | None -> previousSegments |>
                List.map (fun s -> s.Objects) |>
                List.filter (List.exists (fun o' -> o.Name = o'.Name)) |>
                List.tryPick (rawDataIndexFor' o)

  let rawDataFor (previousSegments : Segment list) (channel : string) (reader : BinaryReader) (segment : Segment) =
    if not (segment.LeadIn.TableOfContents.HasFlag(TableOfContents.ContainsRawData))
    then [||]
    else
      reader.BaseStream.Seek(int64 segment.Offset, SeekOrigin.Begin) |> ignore
      let moi = List.tryFindIndex (fun o -> o.Name = channel) segment.Objects
      let mo = Option.bind (fun oi -> List.tryItem oi segment.Objects) moi
      match moi, mo with
        | None, _ | _, None -> [||]
        | Some oi, Some { RawDataIndex = mi } ->
            Option.fold (fun _ i ->
              match i with
                | OtherType(ty, _, m) ->
                  let ms = if segment.LeadIn.TableOfContents.HasFlag(TableOfContents.ContainsBigEndianData) then Reads.bigEndianMappings else Reads.littleEndianMappings
                  let readValue =
                    match ty with
                      | Type.I32 -> ms.Int32 >> box
                      | Type.U64 -> ms.UInt64 >> box
                      | Type.DoubleFloat -> ms.Double >> box
                      | _ -> fun _ -> box()
                  reader.BaseStream.Seek(28L + int64 segment.LeadIn.RawDataOffset, SeekOrigin.Current) |> ignore
                  let before, _ = List.splitAt oi segment.Objects
                  let indices = List.map (fun o -> rawDataIndexFor previousSegments (Some o)) segment.Objects
                  let chunkCount = countChunks indices segment.LeadIn.NextSegmentOffset segment.LeadIn.RawDataOffset
                  let chunkSize = chunkDataSize indices
                  // printfn "%i Chunks of %i Bytes" chunkCount chunkSize
                  [| 
                    //for i in 0uL..(chunkCount - 1) do
                      //printfn "Iteration %i" (i + 1uL)
                      //reader.BaseStream.Seek(int64 segment.Offset + int64 (i * chunkSize), SeekOrigin.Begin) |> ignore
                      List.iter (fun o ->
                        Option.iter (fun rdi ->
                          match rdi with
                            | String _ -> ()
                            | OtherType(ty', _, m') -> reader.BaseStream.Seek(int64 (Type.size ty') * int64 m', SeekOrigin.Current) |> ignore)
                         (rawDataIndexFor previousSegments (Some o))
                       ) before
                      for _ in 1uL..m ->
                        readValue reader
                  |]
                | String _ -> [||]
            ) [||] (rawDataIndexFor previousSegments mo)
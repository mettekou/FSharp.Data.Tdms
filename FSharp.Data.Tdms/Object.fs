namespace FSharp.Data.Tdms

open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Threading.Tasks
open FSharp.Control.Tasks.NonAffine

type RawDataIndex =
  | String of FSharp.Data.Tdms.Type * uint32 * uint64 * uint64
  | OtherType of FSharp.Data.Tdms.Type * uint32 * uint64

type Object = {
  Name : string
  BigEndian: bool
  mutable LastRawDataIndex : RawDataIndex option
  RawDataBlocks : (uint64 * uint64) List
  Properties : Property List
 }

module Object =
  
  let tryPropertyValue<'T> name { Properties = properties } =
    Seq.tryFind (fun (property: Property) -> property.Name = name) properties
    |> Option.map (fun property -> property.Value)
    |> Option.bind Value.tryGet<'T>
    
  let unsafePropertyValue name { Properties = properties } =
    Seq.find (fun (property: Property) -> property.Name = name) properties
    |> (fun property -> property.Value.Raw)
  
  let tryRawData<'t> path { LastRawDataIndex = rawDataIndex; RawDataBlocks = rawDataBlocks; BigEndian = bigEndian } =
    Option.bind (function | String _ -> Some typeof<string> | OtherType(ty', _, _) -> Type.system ty') rawDataIndex 
    |> Option.bind (fun ty ->
      if typeof<'t>.IsAssignableFrom ty then
        use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)
        if ty = typeof<bool> then
          Reader.readPrimitiveRawData<bool> fileStream rawDataBlocks bigEndian |> box |> tryUnbox<'t []>
        else if ty = typeof<sbyte> then
          Reader.readPrimitiveRawData<sbyte> fileStream rawDataBlocks bigEndian |> box |> tryUnbox<'t []>
        else if ty = typeof<int16> then
          Reader.readPrimitiveRawData<int16> fileStream rawDataBlocks bigEndian |> box |> tryUnbox<'t []>
        else if ty = typeof<int> then
          Reader.readPrimitiveRawData<int> fileStream rawDataBlocks bigEndian |> box |> tryUnbox<'t []>
        else if ty = typeof<int64> then
          Reader.readPrimitiveRawData<int64> fileStream rawDataBlocks bigEndian |> box |> tryUnbox<'t []>
        else if ty = typeof<byte> then
          Reader.readPrimitiveRawData<byte> fileStream rawDataBlocks bigEndian |> box |> tryUnbox<'t []>
        else if ty = typeof<uint16> then
          Reader.readPrimitiveRawData<uint16> fileStream rawDataBlocks bigEndian |> box |> tryUnbox<'t []>
        else if ty = typeof<uint> then
          Reader.readPrimitiveRawData<uint> fileStream rawDataBlocks bigEndian |> box |> tryUnbox<'t []>
        else if ty = typeof<uint64> then
          Reader.readPrimitiveRawData<uint64> fileStream rawDataBlocks bigEndian |> box |> tryUnbox<'t []>
        else if ty = typeof<float32> then
          Reader.readPrimitiveRawData<float32> fileStream rawDataBlocks bigEndian |> box |> tryUnbox<'t []>
        else if ty = typeof<float> then
          Reader.readPrimitiveRawData<float> fileStream rawDataBlocks bigEndian |> box |> tryUnbox<'t []>
        else if ty = typeof<Complex> then
          Reader.readComplexRawData fileStream rawDataBlocks bigEndian |> box |> tryUnbox<'t []>
        else if ty = typeof<Timestamp> then
          Reader.readTimestampRawData fileStream rawDataBlocks bigEndian |> box |> tryUnbox<'t []>
        else
          None
      else if ty = typeof<Timestamp> then
        if typeof<'t> = typeof<DateTime> then
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)
          Reader.readTimestampRawData fileStream rawDataBlocks bigEndian
          |> Array.map Timestamp.toDateTime
          |> box
          |> tryUnbox<'t []>
        else if typeof<'t> = typeof<DateTimeOffset> then
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)
          Reader.readTimestampRawData fileStream rawDataBlocks bigEndian
          |> Array.map Timestamp.toDateTimeOffset
          |> box
          |> tryUnbox<'t []>
        else if typeof<'t> = typeof<TimeSpan> then
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)
          Reader.readTimestampRawData fileStream rawDataBlocks bigEndian
          |> Array.map Timestamp.toTimeSpan
          |> box
          |> tryUnbox<'t []>
        else None
      else None)

  let tryRawDataAsync<'t> path { LastRawDataIndex = rawDataIndex; RawDataBlocks = rawDataBlocks; BigEndian = bigEndian } =
    Option.bind (function | String _ -> Some typeof<string> | OtherType(ty', _, _) -> Type.system ty') rawDataIndex
    |> Option.map (fun ty ->
    if ty = typeof<'t> then
      if ty = typeof<bool> then
        task {
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
          let! result = Reader.readPrimitiveRawDataAsync<bool> fileStream rawDataBlocks bigEndian
          return box result |> tryUnbox<'t []>
        } 
      else if ty = typeof<sbyte> then
        task {
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
          let! result = Reader.readPrimitiveRawDataAsync<sbyte> fileStream rawDataBlocks bigEndian
          return box result |> tryUnbox<'t []>
        }
      else if ty = typeof<int16> then
        task {
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
          let! result = Reader.readPrimitiveRawDataAsync<int16> fileStream rawDataBlocks bigEndian
          return box result |> tryUnbox<'t []>
        }
      else if ty = typeof<int> then
        task {
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
          let! result = Reader.readPrimitiveRawDataAsync<int> fileStream rawDataBlocks bigEndian
          return box result |> tryUnbox<'t []>
        }
      else if ty = typeof<int64> then
        task {
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
          let! result = Reader.readPrimitiveRawDataAsync<int64> fileStream rawDataBlocks bigEndian
          return box result |> tryUnbox<'t []>
        }
      else if ty = typeof<byte> then
        task {
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
          let! result = Reader.readPrimitiveRawDataAsync<byte> fileStream rawDataBlocks bigEndian
          return box result |> tryUnbox<'t []>
        }
      else if ty = typeof<uint16> then
        task {
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
          let! result = Reader.readPrimitiveRawDataAsync<uint16> fileStream rawDataBlocks bigEndian
          return box result |> tryUnbox<'t []>
        }
      else if ty = typeof<uint> then
        task {
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
          let! result = Reader.readPrimitiveRawDataAsync<uint> fileStream rawDataBlocks bigEndian
          return box result |> tryUnbox<'t []>
        }
      else if ty = typeof<uint64> then
        task {
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
          let! result = Reader.readPrimitiveRawDataAsync<uint64> fileStream rawDataBlocks bigEndian
          return box result |> tryUnbox<'t []>
        }
      else if ty = typeof<float32> then
        task {
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
          let! result = Reader.readPrimitiveRawDataAsync<float32> fileStream rawDataBlocks bigEndian
          return box result |> tryUnbox<'t []>
        } 
      else if ty = typeof<float> then
        task {
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
          let! result = Reader.readPrimitiveRawDataAsync<float> fileStream rawDataBlocks bigEndian
          return box result |> tryUnbox<'t []>
        } 
      else if ty = typeof<Complex> then
        task {
          use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
          let! result = Reader.readComplexRawDataAsync fileStream rawDataBlocks bigEndian
          return box result |> tryUnbox<'t []>
        } 
      else Task.FromResult None
    else if ty = typeof<Timestamp> then
        if typeof<'t> = typeof<DateTime> then
          task {
            use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)
            let! result = Reader.readTimestampRawDataAsync fileStream rawDataBlocks bigEndian
            return Array.map Timestamp.toDateTime result |> box |> tryUnbox<'t []>
          }
        else if typeof<'t> = typeof<DateTimeOffset> then
          task {
            use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)
            let! result = Reader.readTimestampRawDataAsync fileStream rawDataBlocks bigEndian
            return Array.map Timestamp.toDateTimeOffset result |> box |> tryUnbox<'t []>
          }
        else if typeof<'t> = typeof<TimeSpan> then
          task {
            use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)
            let! result = Reader.readTimestampRawDataAsync fileStream rawDataBlocks bigEndian
            return Array.map Timestamp.toTimeSpan result |> box |> tryUnbox<'t []>
          }
        else Task.FromResult None
    else Task.FromResult None)
    |> Option.defaultValue (Task.FromResult None)
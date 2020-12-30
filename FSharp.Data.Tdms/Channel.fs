namespace FSharp.Data.Tdms

open System
open System.IO
open System.Numerics
open System.Threading.Tasks
open FSharp.Control.Tasks.NonAffine

type Channel = {
  Type : Type
  Properties : Map<string, Value>
  BigEndian: bool
  RawDataBlocks : (uint64 * uint64) list
 }

module Channel =

  let merge { Properties = ps; RawDataBlocks = bs } { Type = ty'; Properties = ps'; BigEndian = bigEndian; RawDataBlocks = bs' } =
    { Type = ty'; Properties = Map.fold (fun ps k v -> Map.add k v ps) ps' ps; RawDataBlocks = List.append bs bs'; BigEndian = bigEndian }
  
  let tryPropertyValue<'T> name channel =
    Map.tryFind name channel.Properties |> Option.bind Value.tryGet<'T>
  
  let unsafePropertyValue name channel =
    Map.tryFind name channel.Properties |> Option.get |> (fun v -> v.Raw)
  
  let tryRawData<'t> path { Type = ty; RawDataBlocks = rawDataBlocks; BigEndian = bigEndian } =
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
        Reader.readComplexRawData fileStream rawDataBlocks true |> box |> tryUnbox<'t []>
      else
        (*use reader = new BinaryReader(fileStream)
        Some [| for (p, l) in rawDataBlocks do
                    fileStream.Seek(int64 p, SeekOrigin.Begin) |> ignore
                    for _ in 1uL..l ->
                      read reader :?> 't |]*)
        None
    else
      None
    
  let tryRawDataAsync<'t> path { Type = ty; RawDataBlocks = rawDataBlocks; BigEndian = bigEndian } =
    if ty <> typeof<'t> then Task.FromResult None
    else if ty = typeof<bool> then
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
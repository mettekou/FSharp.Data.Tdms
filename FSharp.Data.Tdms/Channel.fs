namespace FSharp.Data.Tdms

open System
open System.IO
open System.Threading.Tasks
open FSharp.Control.Tasks.NonAffine

type Channel = {
  Type : Type
  Properties : Map<string, Value>
  Read : BinaryReader -> obj
  RawDataBlocks : (uint64 * uint64) list
 }

module Channel =

  let merge { Properties = ps; RawDataBlocks = bs } { Read = r'; Type = ty'; Properties = ps';  RawDataBlocks = bs' } =
    { Type = ty'; Properties = Map.fold (fun ps k v -> Map.add k v ps) ps' ps; Read = r'; RawDataBlocks = List.append bs bs'  }
  
  let tryPropertyValue<'T> name channel =
    Map.tryFind name channel.Properties |> Option.bind Value.tryGet<'T>
  
  let unsafePropertyValue name channel =
    Map.tryFind name channel.Properties |> Option.get |> (fun v -> v.Raw)
  
  let rawData<'T> (reader : BinaryReader) channel =
    if typeof<'T>.IsAssignableFrom(channel.Type) then
          Some [| for (p, l) in channel.RawDataBlocks do
                    reader.BaseStream.Seek(int64 p, SeekOrigin.Begin) |> ignore
                    for _ in 1uL..l ->
                      channel.Read reader :?> 'T |]
      else None
    
  let tryRawDataAsync<'t> path { Type = ty; RawDataBlocks = rawDataBlocks } =
    if ty <> typeof<'t> then Task.FromResult None
    else if ty = typeof<bool> then
      task {
        use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
        let! result = Reader.readPrimitiveRawDataAsync<bool> fileStream rawDataBlocks false
        return box result |> tryUnbox<'t []>
      } 
    else if ty = typeof<sbyte> then
      task {
        use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
        let! result = Reader.readPrimitiveRawDataAsync<sbyte> fileStream rawDataBlocks false
        return box result |> tryUnbox<'t []>
      }
    else if ty = typeof<int16> then
      task {
        use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
        let! result = Reader.readPrimitiveRawDataAsync<int16> fileStream rawDataBlocks false
        return box result |> tryUnbox<'t []>
      }
    else if ty = typeof<int> then
      task {
        use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
        let! result = Reader.readPrimitiveRawDataAsync<int> fileStream rawDataBlocks false
        return box result |> tryUnbox<'t []>
      }
    else if ty = typeof<int64> then
      task {
        use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
        let! result = Reader.readPrimitiveRawDataAsync<int64> fileStream rawDataBlocks false
        return box result |> tryUnbox<'t []>
      }
    else if ty = typeof<byte> then
      task {
        use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
        let! result = Reader.readPrimitiveRawDataAsync<byte> fileStream rawDataBlocks false
        return box result |> tryUnbox<'t []>
      }
    else if ty = typeof<uint16> then
      task {
        use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
        let! result = Reader.readPrimitiveRawDataAsync<uint16> fileStream rawDataBlocks false
        return box result |> tryUnbox<'t []>
      }
    else if ty = typeof<uint> then
      task {
        use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
        let! result = Reader.readPrimitiveRawDataAsync<uint> fileStream rawDataBlocks false
        return box result |> tryUnbox<'t []>
      }
    else if ty = typeof<uint64> then
      task {
        use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
        let! result = Reader.readPrimitiveRawDataAsync<uint64> fileStream rawDataBlocks false
        return box result |> tryUnbox<'t []>
      }
    else if ty = typeof<float32> then
      task {
        use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
        let! result = Reader.readPrimitiveRawDataAsync<float32> fileStream rawDataBlocks false
        return box result |> tryUnbox<'t []>
      } 
    else if ty = typeof<float> then
      task {
        use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)
        let! result = Reader.readPrimitiveRawDataAsync<float> fileStream rawDataBlocks false
        return box result |> tryUnbox<'t []>
      } 
    else Task.FromResult None
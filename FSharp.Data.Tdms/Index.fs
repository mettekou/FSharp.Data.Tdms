namespace FSharp.Data.Tdms

open System.Buffers
open System.Collections.Generic
open System.IO
open System.Text
open System.Threading.Tasks
open FSharp.Collections

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Index =

  let readSegments fromIndex leadInBuffer (stream : Stream) indexStream =
    let segmentedIndex = { Objects = List() }
    while stream.Position < stream.Length do
      Segment.read fromIndex segmentedIndex leadInBuffer stream indexStream
    segmentedIndex
    
  let read fromIndex writeIndex (path : string) (indexPath: string) =
    use stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4_096, if fromIndex then FileOptions.SequentialScan else FileOptions.None)
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
    let leadInBuffer = ArrayPool<byte>.Shared.Rent 28
    if writeIndex then
      use indexStream = new FileStream(indexPath, FileMode.Create, FileAccess.Write, FileShare.None, 4_096, false)
      let segmentedIndex = readSegments fromIndex leadInBuffer stream indexStream
      ArrayPool<byte>.Shared.Return(leadInBuffer, false)
      segmentedIndex
    else
      let segmentedIndex = readSegments fromIndex leadInBuffer stream null
      ArrayPool<byte>.Shared.Return(leadInBuffer, false)
      segmentedIndex
    
  let tryPropertyValue<'t> name { Objects = objects } =
    Seq.cast<Object> objects
    |> Seq.tryFind (fun object -> object.Name = "/") 
    |> Option.bind (fun object -> Seq.tryFind (fun (property: Property) -> property.Name = name) object.Properties)
    |> Option.bind (fun property -> Value.tryGet<'t> property.Value)
    
  let unsafePropertyValue name { Objects = objects } =
    Seq.cast<Object> objects
    |> Seq.find (fun object -> object.Name = "/")
    |> (fun object -> Seq.find (fun (property: Property) -> property.Name = name) object.Properties)
    |> (fun property ->  Value.tryGet<'t> property.Value |> Option.get)
  
  let tryGroup groupName { Objects = objects } =
    Seq.cast<Object> objects
    |> Seq.tryFind (fun object -> object.Name = "/'" + groupName + "'")

  let unsafeGroup groupName { Objects = objects } =
    Seq.cast<Object> objects
    |> Seq.find (fun object -> object.Name = "/'" + groupName + "'")
  
  let unsafeChannel groupName channelName { Objects = objects } =
    Seq.cast<Object> objects
    |> Seq.find (fun object -> object.Name = "/'" + groupName + "'/'" + channelName + "'")
  
  let tryChannel groupName channelName { Objects = objects } =
    Seq.cast<Object> objects
    |> Seq.tryFind (fun object -> object.Name = "/'" + groupName + "'/'" + channelName + "'")

  let tryRawData<'t> path groupName channelName index =
    let channel = tryChannel groupName channelName index
    channel |> Option.bind (Object.tryRawData<'t> (Path.ChangeExtension(path, ".tdms")))

  let tryRawDataAsync<'t> path groupName channelName index =
    tryChannel groupName channelName index
    |> Option.map (Object.tryRawDataAsync<'t> (Path.ChangeExtension(path, ".tdms")))
    |> Option.defaultValue (Task.FromResult None)
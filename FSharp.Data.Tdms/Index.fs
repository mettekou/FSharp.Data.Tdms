namespace FSharp.Data.Tdms

open System.Threading.Tasks

type Index = {
  Path : string
  Properties : Map<string, Value>
  Groups : Map<string, Group>
}

module Index =

  open System.IO
  open FSharp.Collections
  
  let merge { Path = p; Properties = ps; Groups = gs } { Properties = ps'; Groups = gs' } =
    { Path = p; Properties = Map.fold (fun ps k v -> Map.add k v ps) ps' ps; Groups = Map.unionWith Group.merge gs gs' }
  
  let tryPropertyValue<'T> name index =
    Map.tryFind name index.Properties |> Option.bind Value.tryGet<'T>
    
  let unsafePropertyValue name index =
    Map.tryFind name index.Properties |> Option.get |> (fun v -> v.Raw)
  
  let unsafeGroup name index =
    Map.tryFind name index.Groups |> Option.get
  
  let tryRawData<'T> groupName channelName index =
    use reader = new BinaryReader(File.OpenRead(Path.ChangeExtension(index.Path, ".tdms")))
    Map.tryFind groupName index.Groups |> Option.bind (fun g -> Map.tryFind channelName g.Channels) |> Option.bind (Channel.rawData<'T> reader)

  let tryRawDataAsync<'t> groupName channelName { Path = path; Groups = groups } =
    Map.tryFind groupName groups
    |> Option.bind (fun g -> Map.tryFind channelName g.Channels)
    |> Option.map (Channel.tryRawDataAsync<'t> (Path.ChangeExtension(path, ".tdms")))
    |> Option.defaultValue (Task.FromResult None)

  let tryGroup groupName { Groups = groups } = Map.tryFind groupName groups

  let tryChannel groupName channelName index = tryGroup groupName index |> Option.bind (Group.tryChannel channelName)
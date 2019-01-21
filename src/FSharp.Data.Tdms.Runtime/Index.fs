namespace FSharp.Data.Tdms

open System.IO

type Index = {
  Path : string
  Properties : Map<string, Value>
  Groups : Map<string, Group>
}

module Index =
  
  let unionWith combiner m m' = Map.fold (fun ps k v ->
    let oldChannel = Map.tryFind k m'
    Map.add k (Option.fold (fun v c -> combiner v c) v oldChannel) ps) m' m
  
  let merge { Path = p; Properties = ps; Groups = gs } { Properties = ps'; Groups = gs' } =
    { Path = p; Properties = Map.fold (fun ps k v -> Map.add k v ps) ps' ps; Groups = unionWith Group.merge gs gs' }
  
  let propertyValue<'T> name index =
    Map.tryFind name index.Properties |> Option.bind (fun v -> if (Type.system v.Type).IsAssignableFrom(typeof<'T>) then Some(box v.Raw :?> 'T) else None)
    
  let unsafePropertyValue name index =
    Map.tryFind name index.Properties |> Option.get |> (fun v -> v.Raw)
  
  let unsafeGroup name index =
    Map.tryFind name index.Groups |> Option.get
  
  let rawData<'T> groupName channelName index =
    use reader = new BinaryReader(File.OpenRead(index.Path))
    Map.tryFind groupName index.Groups |> Option.bind (fun g -> Map.tryFind channelName g.Channels) |> Option.bind (Channel.rawData<'T> reader)
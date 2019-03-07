namespace FSharp.Data.Tdms

type Group = {
  Properties : Map<string, Value>
  Channels : Map<string, Channel>
}

module Group =

  open FSharp.Collections
  
  let merge { Properties = ps; Channels = cs } { Properties = ps'; Channels = cs' } =
    { Properties = Map.fold (fun ps k v -> Map.add k v ps) ps' ps; Channels = Map.unionWith Channel.merge cs cs' }
  
  let propertyValue<'T> name group =
    Map.tryFind name group.Properties |> Option.bind (fun v -> if (Type.system v.Type).IsAssignableFrom(typeof<'T>) then Some(v.Raw :?> 'T) else None)
    
  let unsafePropertyValue name group =
    Map.tryFind name group.Properties |> Option.get |> (fun v -> v.Raw)
  
  let unsafeChannel name group =
    Map.tryFind name group.Channels |> Option.get
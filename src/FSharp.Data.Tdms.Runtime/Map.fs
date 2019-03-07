namespace FSharp.Collections

[<AutoOpen>]
module Map =
    
    let unionWith f m m' = Map.fold (fun ps k v ->
        let v' = Map.tryFind k m'
        Map.add k (Option.fold f v v') ps) m' m


namespace FSharp.Data.Tdms

type Value = {
  Type : Type
  Raw : obj
}

module Value =
    
    let tryGet<'T> value =
        if typeof<'T>.IsAssignableFrom(Type.system value.Type |> Option.defaultValue typeof<unit>)
        then Some (box value.Raw :?> 'T)
        else None
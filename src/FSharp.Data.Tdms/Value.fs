namespace FSharp.Data.Tdms

type Value = {
  Type : Type
  Raw : obj
}

module Value =

  let tryCast<'a> value : 'a option =
    if (Type.system value.Type).IsAssignableFrom(typeof<'a>) then (box value.Type :?> 'a) |> Some else None
namespace FSharp.Data.Tdms

open System

type Value = {
  Type : FSharp.Data.Tdms.Type
  Raw : obj
}

module Value =
    
    let tryGet<'t> { Type = ty; Raw = raw } =
        let ty' = typeof<'t>
        if ty'.IsAssignableFrom(Type.system ty |> Option.defaultValue typeof<unit>)
        then tryUnbox<'t> raw
        else
          if ty = Type.Timestamp then
            let timestamp = raw :?> Timestamp
            if ty' = typeof<DateTime> then tryUnbox<'t> (Timestamp.toDateTime timestamp)
            else if ty' = typeof<DateTimeOffset> then tryUnbox<'t> (Timestamp.toDateTimeOffset timestamp)
            else if ty' = typeof<TimeSpan> then tryUnbox<'t> (Timestamp.toTimeSpan timestamp)
            else None
          else None
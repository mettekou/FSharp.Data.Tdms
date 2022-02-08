namespace FSharp.Data.Tdms

open System

type Property = {
  Name: string
  Type : Type
  Raw : obj
}

module Property =
    
    let tryGet<'t> { Type = ty; Raw = raw } =
        let ty' = typeof<'t>
        if ty'.IsAssignableFrom ty
        then tryUnbox<'t> raw
        else
          if ty = typeof<Timestamp> then
            let timestamp = raw :?> Timestamp
            if ty' = typeof<DateTime> then tryUnbox<'t> (Timestamp.toDateTime timestamp)
            else if ty' = typeof<DateTimeOffset> then tryUnbox<'t> (Timestamp.toDateTimeOffset timestamp)
            else if ty' = typeof<TimeSpan> then tryUnbox<'t> (Timestamp.toTimeSpan timestamp)
            else None
          else None
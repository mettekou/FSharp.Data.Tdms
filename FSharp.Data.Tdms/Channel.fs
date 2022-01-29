namespace FSharp.Data.Tdms

open System
open System.IO
open System.Numerics
open System.Threading
open System.Threading.Tasks

type Channel = {
    Name: string
    FilePath: string
    Properties: Property seq
    RawDataBlocks: RawDataBlocks option
    BigEndian: bool
}

module Channel =
    
    let tryGetPropertyValue<'t> propertyName { Properties = properties } =
        properties
        |> Seq.tryFind (fun { Name = propertyName' } -> propertyName' = propertyName)
        |> Option.bind Property.tryGet<'t>

    let getPropertyValue<'t> propertyName =
        tryGetPropertyValue<'t> propertyName >> Option.get
        
    let tryGetRawData<'t>
        { FilePath = path
          RawDataBlocks = rawDataBlocks
          BigEndian = bigEndian }
        =
        let bufferSize = 65_536
        match rawDataBlocks with
        | None -> None
        | Some (PrimitiveRawDataBlocks (ty, rawDataBlockArray)) ->

            if typeof<'t>.IsAssignableFrom ty then
                use fileStream =
                    new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.None)

                if ty = typeof<bool> then
                    Reader.readPrimitiveRawData<bool> fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                elif ty = typeof<sbyte> then
                    Reader.readPrimitiveRawData<sbyte> fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                elif ty = typeof<int16> then
                    Reader.readPrimitiveRawData<int16> fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                elif ty = typeof<int> then
                    Reader.readPrimitiveRawData<int> fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                elif ty = typeof<int64> then
                    Reader.readPrimitiveRawData<int64> fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                elif ty = typeof<byte> then
                    Reader.readPrimitiveRawData<byte> fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                elif ty = typeof<uint16> then
                    Reader.readPrimitiveRawData<uint16> fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                elif ty = typeof<uint> then
                    Reader.readPrimitiveRawData<uint> fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                elif ty = typeof<uint64> then
                    Reader.readPrimitiveRawData<uint64> fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                elif ty = typeof<float32> then
                    Reader.readPrimitiveRawData<float32> fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                elif ty = typeof<float> then
                    Reader.readPrimitiveRawData<float> fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                elif ty = typeof<Complex> then
                    Reader.readComplexRawData fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                elif ty = typeof<Timestamp> then
                    Reader.readTimestampRawData fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                elif ty = typeof<float80> then
                    Reader.readFloat80RawData fileStream rawDataBlockArray bigEndian
                    |> box
                    |> tryUnbox<'t []>
                else
                    None
            else if ty = typeof<Timestamp> then
                if typeof<'t> = typeof<DateTime> then
                    use fileStream =
                        new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.None)

                    Reader.readTimestampRawData fileStream rawDataBlockArray bigEndian
                    |> Array.map Timestamp.toDateTime
                    |> box
                    |> tryUnbox<'t []>
                else if typeof<'t> = typeof<DateTimeOffset> then
                    use fileStream =
                        new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.None)

                    Reader.readTimestampRawData fileStream rawDataBlockArray bigEndian
                    |> Array.map Timestamp.toDateTimeOffset
                    |> box
                    |> tryUnbox<'t []>
                else if typeof<'t> = typeof<TimeSpan> then
                    use fileStream =
                        new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.None)

                    Reader.readTimestampRawData fileStream rawDataBlockArray bigEndian
                    |> Array.map Timestamp.toTimeSpan
                    |> box
                    |> tryUnbox<'t []>
                else
                    None
            else
                None
        | Some (StringRawDataBlocks stringRawDataBlockArray) ->
            if typeof<'t> = typeof<string> then
                use fileStream =
                    new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.None)

                Reader.readStringRawData fileStream stringRawDataBlockArray bigEndian
                |> box
                |> tryUnbox<'t []>
            else
                None

    let tryGetRawDataCtAsync<'t>
        ct
        { FilePath = path
          RawDataBlocks = rawDataBlocks
          BigEndian = bigEndian }
        =
        let bufferSize = 65_536
        match rawDataBlocks with
        | None -> Task.FromResult None
        | Some (PrimitiveRawDataBlocks (ty, rawDataBlockArray)) ->

            if ty = typeof<'t> then
                if ty = typeof<bool> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readPrimitiveRawDataAsync<bool> ct fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<sbyte> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readPrimitiveRawDataAsync<sbyte> ct fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<int16> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readPrimitiveRawDataAsync<int16> ct fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<int> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readPrimitiveRawDataAsync<int> ct fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<int64> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readPrimitiveRawDataAsync<int64> ct fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<byte> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readPrimitiveRawDataAsync<byte> ct fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<uint16> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readPrimitiveRawDataAsync<uint16> ct fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<uint> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readPrimitiveRawDataAsync<uint> ct fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<uint64> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readPrimitiveRawDataAsync<uint64> ct fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<float32> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readPrimitiveRawDataAsync<float32> ct fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<float> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readPrimitiveRawDataAsync<float> ct fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<float80> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readFloat80RawDataAsync ct fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<Complex> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readComplexRawDataAsync ct fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else
                    Task.FromResult None
            else if ty = typeof<Timestamp> then
                if typeof<'t> = typeof<DateTime> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readTimestampRawDataAsync ct fileStream rawDataBlockArray bigEndian

                        return
                            Array.map Timestamp.toDateTime result
                            |> box
                            |> tryUnbox<'t []>
                    }
                else if typeof<'t> = typeof<DateTimeOffset> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readTimestampRawDataAsync ct fileStream rawDataBlockArray bigEndian

                        return
                            Array.map Timestamp.toDateTimeOffset result
                            |> box
                            |> tryUnbox<'t []>
                    }
                else if typeof<'t> = typeof<TimeSpan> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                        let! result = Reader.readTimestampRawDataAsync ct fileStream rawDataBlockArray bigEndian

                        return
                            Array.map Timestamp.toTimeSpan result
                            |> box
                            |> tryUnbox<'t []>
                    }
                else
                    Task.FromResult None
            else
                Task.FromResult None
        | Some (StringRawDataBlocks stringRawDataBlockArray) ->
            if typeof<'t> = typeof<string> then
                task {
                    use fileStream =
                        new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize, FileOptions.Asynchronous)

                    let! result = Reader.readStringRawDataAsync ct fileStream stringRawDataBlockArray bigEndian
                    return result |> box |> tryUnbox<'t []>
                }
            else
                Task.FromResult None

    let tryRawDataAsync<'t> = tryGetRawDataCtAsync<'t> CancellationToken.None

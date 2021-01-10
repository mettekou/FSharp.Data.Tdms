namespace FSharp.Data.Tdms

open System
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Threading.Tasks
open FSharp.Control.Tasks.NonAffine

type Object =
    { Name: string
      BigEndian: bool
      mutable RawDataBlocks: RawDataBlocks option
      Properties: Property ResizeArray }

module Object =

    let addPrimitiveRawDataBlock ty primitiveRawDataBlock object =
        match object.RawDataBlocks with
        | None ->
            let primitiveRawDataBlockArray = ResizeArray()
            primitiveRawDataBlockArray.Add primitiveRawDataBlock
            object.RawDataBlocks <- Some(PrimitiveRawDataBlocks(ty, primitiveRawDataBlockArray))
        | Some (PrimitiveRawDataBlocks (ty', primitiveRawDataBlockArray)) ->
            if ty <> ty' then
                failwithf
                    "Object %s already has type %A, cannot add a primitive data block of type %A to it; check whether the TDMS file is valid"
                    object.Name
                    ty'
                    ty
            else
                primitiveRawDataBlockArray.Add primitiveRawDataBlock
        | Some (StringRawDataBlocks _) ->
            failwithf
                "Object %s already has type string, cannot add a primitive data block of type %A to it; check whether the TDMS file is valid"
                object.Name
                ty

    let addStringRawDataBlock stringRawDataBlock object =
        match object.RawDataBlocks with
        | None ->
            let stringRawDataBlockArray = ResizeArray()
            stringRawDataBlockArray.Add stringRawDataBlock
            object.RawDataBlocks <- Some(StringRawDataBlocks stringRawDataBlockArray)
        | Some (PrimitiveRawDataBlocks (ty', _)) ->
            failwithf
                "Object %s already has type %A, cannot add a string data block to it; check whether the TDMS file is valid"
                object.Name
                ty'
        | Some (StringRawDataBlocks stringRawDataBlockArray) -> stringRawDataBlockArray.Add stringRawDataBlock

    let readRawDataIndex
        object
        (rawDataPosition: uint64)
        (buffer: byte ReadOnlySpan byref)
        bigEndian
        (interleaved: bool)
        =
        match Buffer.readUInt &buffer bigEndian with
        | 0u ->
            match object.RawDataBlocks with
            | None -> failwithf "Missing raw data index for object %s; check whether the TDMS file is valid" object.Name
            | Some (PrimitiveRawDataBlocks (ty, primitiveRawDataBlockArray)) ->
                match Seq.tryLast primitiveRawDataBlockArray with
                | None ->
                    failwithf
                        "Missing primitive raw data blocks for object %s; this is a bug in FSharp.Data.Tdms"
                        object.Name
                | Some (DecimatedPrimitiveRawDataBlock (_, bytes)) ->
                    if interleaved then
                        failwithf
                            "Object %s raw data changes from decimated to interleaved without a new raw data index; check whether the TDMS file is valid"
                            object.Name
                    else
                        primitiveRawDataBlockArray.Add(DecimatedPrimitiveRawDataBlock(rawDataPosition, bytes))
                        bytes
                | Some (InterleavedPrimitiveRawDataBlock _) ->
                    if not interleaved then
                        failwithf
                            "Object %s raw data changes from interleaved to decimated without a new raw data index; check whether the TDMS file is valid"
                            object.Name
                    else
                        uint64 (Marshal.SizeOf ty)
            | Some (StringRawDataBlocks stringRawDataBlockArray) ->
                match Seq.tryLast stringRawDataBlockArray with
                | None ->
                    failwithf
                        "Missing string raw data blocks for object %s; this is a bug in FSharp.Data.Tdms"
                        object.Name
                | Some (offset, length, bytes) ->
                    stringRawDataBlockArray.Add(offset, length, bytes)
                    bytes
        | 20u ->
            let ty = Buffer.readType &buffer bigEndian
            Buffer.readUInt &buffer bigEndian |> ignore
            let length = Buffer.readUInt64 &buffer bigEndian
            let size = Marshal.SizeOf ty

            if not interleaved then
                addPrimitiveRawDataBlock ty (DecimatedPrimitiveRawDataBlock(rawDataPosition, length)) object
                length * uint64 size
            else
                addPrimitiveRawDataBlock
                    ty
                    (InterleavedPrimitiveRawDataBlock
                        { Start = rawDataPosition
                          Count = length
                          Skip = 0uL })
                    object

                uint64 size
        | 28u ->
            Buffer.readType &buffer bigEndian |> ignore
            Buffer.readUInt &buffer bigEndian |> ignore

            let length = Buffer.readUInt64 &buffer bigEndian
            let bytes = Buffer.readUInt64 &buffer bigEndian
            addStringRawDataBlock (rawDataPosition, length, bytes) object
            bytes
        | 0xFFFFFFFFu -> 0uL
        | 0x1269u
        | 0x126Au ->
            let ty = Buffer.readType &buffer bigEndian
            let dimension = Buffer.readUInt &buffer bigEndian
            let chunkSize = Buffer.readUInt64 &buffer bigEndian
            let scalerCount = Buffer.readUInt &buffer bigEndian |> int

            let scalers =
                Array.zeroCreate<FormatChangingScaler> scalerCount

            for scalerIndex = 0 to scalerCount - 1 do
                scalers.[scalerIndex] <- RawDataBlock.readFormatChangingScaler &buffer bigEndian

            let widthCount = Buffer.readUInt &buffer bigEndian |> int
            let widths = Array.zeroCreate<uint> widthCount

            for widthIndex = 0 to widthCount - 1 do
                widths.[widthIndex] <- Buffer.readUInt &buffer bigEndian
            //object.LastRawDataIndex <- Some (DaqMx (ty, dimension, chunkSize, scalers, widths))
            0uL
        | length -> failwithf "Invalid raw data index length: %i" length

    let tryPropertyValue<'T> name { Properties = properties } =
        Seq.tryFind (fun (property: Property) -> property.Name = name) properties
        |> Option.bind Property.tryGet<'T>

    let unsafePropertyValue name { Properties = properties } =
        Seq.find (fun (property: Property) -> property.Name = name) properties
        |> (fun property -> property.Raw)

    let tryRawData<'t>
        path
        { RawDataBlocks = rawDataBlocks
          BigEndian = bigEndian }
        =
        match rawDataBlocks with
        | None -> None
        | Some (PrimitiveRawDataBlocks (ty, rawDataBlockArray)) ->

            if typeof<'t>.IsAssignableFrom ty then
                use fileStream =
                    new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)

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
                else
                    None
            else if ty = typeof<Timestamp> then
                if typeof<'t> = typeof<DateTime> then
                    use fileStream =
                        new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)

                    Reader.readTimestampRawData fileStream rawDataBlockArray bigEndian
                    |> Array.map Timestamp.toDateTime
                    |> box
                    |> tryUnbox<'t []>
                else if typeof<'t> = typeof<DateTimeOffset> then
                    use fileStream =
                        new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)

                    Reader.readTimestampRawData fileStream rawDataBlockArray bigEndian
                    |> Array.map Timestamp.toDateTimeOffset
                    |> box
                    |> tryUnbox<'t []>
                else if typeof<'t> = typeof<TimeSpan> then
                    use fileStream =
                        new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)

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
                    new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)

                Reader.readStringRawData fileStream stringRawDataBlockArray bigEndian
                |> box
                |> tryUnbox<'t []>
            else
                None

    let tryRawDataAsync<'t>
        path
        { RawDataBlocks = rawDataBlocks
          BigEndian = bigEndian }
        =
        match rawDataBlocks with
        | None -> Task.FromResult None
        | Some (PrimitiveRawDataBlocks (ty, rawDataBlockArray)) ->

            if ty = typeof<'t> then
                if ty = typeof<bool> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)

                        let! result = Reader.readPrimitiveRawDataAsync<bool> fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<sbyte> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)

                        let! result = Reader.readPrimitiveRawDataAsync<sbyte> fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<int16> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)

                        let! result = Reader.readPrimitiveRawDataAsync<int16> fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<int> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)

                        let! result = Reader.readPrimitiveRawDataAsync<int> fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<int64> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)

                        let! result = Reader.readPrimitiveRawDataAsync<int64> fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<byte> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)

                        let! result = Reader.readPrimitiveRawDataAsync<byte> fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<uint16> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)

                        let! result = Reader.readPrimitiveRawDataAsync<uint16> fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<uint> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)

                        let! result = Reader.readPrimitiveRawDataAsync<uint> fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<uint64> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)

                        let! result = Reader.readPrimitiveRawDataAsync<uint64> fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<float32> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)

                        let! result = Reader.readPrimitiveRawDataAsync<float32> fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<float> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)

                        let! result = Reader.readPrimitiveRawDataAsync<float> fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else if ty = typeof<Complex> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, true)

                        let! result = Reader.readComplexRawDataAsync fileStream rawDataBlockArray bigEndian
                        return box result |> tryUnbox<'t []>
                    }
                else
                    Task.FromResult None
            else if ty = typeof<Timestamp> then
                if typeof<'t> = typeof<DateTime> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)

                        let! result = Reader.readTimestampRawDataAsync fileStream rawDataBlockArray bigEndian

                        return
                            Array.map Timestamp.toDateTime result
                            |> box
                            |> tryUnbox<'t []>
                    }
                else if typeof<'t> = typeof<DateTimeOffset> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)

                        let! result = Reader.readTimestampRawDataAsync fileStream rawDataBlockArray bigEndian

                        return
                            Array.map Timestamp.toDateTimeOffset result
                            |> box
                            |> tryUnbox<'t []>
                    }
                else if typeof<'t> = typeof<TimeSpan> then
                    task {
                        use fileStream =
                            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)

                        let! result = Reader.readTimestampRawDataAsync fileStream rawDataBlockArray bigEndian

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
                        new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 131_072, false)

                    let! result = Reader.readStringRawDataAsync fileStream stringRawDataBlockArray bigEndian
                    return result |> box |> tryUnbox<'t []>
                }
            else
                Task.FromResult None

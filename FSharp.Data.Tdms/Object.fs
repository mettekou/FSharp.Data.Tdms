namespace FSharp.Data.Tdms

open System
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Threading.Tasks

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
                | Some (_, length, bytes) ->
                    stringRawDataBlockArray.Add(rawDataPosition, length, bytes)
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

    let toChannel
        filePath
        ({ Name = name
           Properties = properties
           RawDataBlocks = rawDataBlocks
           BigEndian = bigEndian }: Object)
        =
        { Name =
              name.Split('/', StringSplitOptions.RemoveEmptyEntries).[1]
                  .Trim('\'')
          FilePath = filePath
          Properties = properties
          RawDataBlocks = rawDataBlocks
          BigEndian = bigEndian }

    let toGroup
        filePath
        channels
        ({ Name = groupName
           Properties = groupProperties }: Object)
        =
        { Name =
              groupName.Split('/', StringSplitOptions.RemoveEmptyEntries).[0]
                  .Trim('\'')
          Properties = groupProperties
          Channels = Seq.map (toChannel filePath) channels }

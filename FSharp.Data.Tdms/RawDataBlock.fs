namespace FSharp.Data.Tdms

open System
open System.Runtime.InteropServices

module RawDataBlock =

    let readFormatChangingScaler (buffer: byte ReadOnlySpan byref) bigEndian =
        { DaqMxDataType = Buffer.readUInt &buffer bigEndian
          RawBufferIndex = Buffer.readUInt &buffer bigEndian
          RawByteOffsetWithinStride = Buffer.readUInt &buffer bigEndian
          SampleFormatBitmap = Buffer.readUInt &buffer bigEndian
          ScaleId = Buffer.readUInt &buffer bigEndian }

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

            if not interleaved then
                Object.addPrimitiveRawDataBlock ty (DecimatedPrimitiveRawDataBlock(rawDataPosition, length)) object
                length
            else
                let size = Marshal.SizeOf ty

                Object.addPrimitiveRawDataBlock
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
            Object.addStringRawDataBlock (rawDataPosition, length, bytes) object
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
                scalers.[scalerIndex] <- readFormatChangingScaler &buffer bigEndian

            let widthCount = Buffer.readUInt &buffer bigEndian |> int
            let widths = Array.zeroCreate<uint> widthCount

            for widthIndex = 0 to widthCount - 1 do
                widths.[widthIndex] <- Buffer.readUInt &buffer bigEndian
            //object.LastRawDataIndex <- Some (DaqMx (ty, dimension, chunkSize, scalers, widths))
            0uL
        | length -> failwithf "Invalid raw data index length: %i" length

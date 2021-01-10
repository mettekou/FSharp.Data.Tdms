namespace FSharp.Data.Tdms

open System
open System.Buffers
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open FSharp.Control.Tasks.NonAffine

type Tag =
    | Tdsm = 1834173524u
    | Tdsh = 1750287444u

[<Flags>]
type TableOfContents =
    | ContainsMetaData = 2u
    | ContainsRawData = 8u
    | ContainsDaqMxRawData = 128u
    | ContainsInterleavedData = 32u
    | ContainsBigEndianData = 64u
    | ContainsNewObjectList = 4u

type Version =
    | ``1.0`` = 4712u
    | ``2.0`` = 4713u

[<Struct>]
type LeadIn =
    { Tag: Tag
      TableOfContents: TableOfContents
      Version: Version
      NextSegmentOffset: uint64
      RawDataOffset: uint64 }

type Index =
    { Objects: FSharp.Data.Tdms.Object List }

module Segment =

    let readLeadIn (buffer: byte ReadOnlySpan byref) =
        let tag =
            Buffer.readUInt &buffer false
            |> LanguagePrimitives.EnumOfValue<uint32, Tag>

        let tableOfContents =
            Buffer.readUInt &buffer false
            |> LanguagePrimitives.EnumOfValue<uint32, TableOfContents>

        let bigEndian =
            tableOfContents.HasFlag(TableOfContents.ContainsBigEndianData)

        { Tag = tag
          TableOfContents = tableOfContents
          Version =
              Buffer.readUInt &buffer bigEndian
              |> LanguagePrimitives.EnumOfValue<uint32, Version>
          NextSegmentOffset = Buffer.readUInt64 &buffer bigEndian
          RawDataOffset = Buffer.readUInt64 &buffer bigEndian }

    let readLeadInMemory (memory: byte ReadOnlyMemory) =
        let mutable buffer = memory.Span
        readLeadIn &buffer

    let readPropertyValue (buffer: byte ReadOnlySpan byref) bigEndian propertyType =
        if propertyType = typeof<unit> then
            buffer <- buffer.Slice 1
            box ()
        elif propertyType = typeof<bool> then
            let value = box (buffer.[0] <> 0uy)
            buffer <- buffer.Slice 1
            value
        elif propertyType = typeof<int8> then
            let value =
                box (MemoryMarshal.Cast<uint8, int8> buffer).[0]

            buffer <- buffer.Slice 1
            value
        elif propertyType = typeof<int16> then
            Buffer.readInt16 &buffer bigEndian |> box
        elif propertyType = typeof<int> then
            Buffer.readInt &buffer bigEndian |> box
        elif propertyType = typeof<int64> then
            Buffer.readInt64 &buffer bigEndian |> box
        elif propertyType = typeof<uint8> then
            let value = box buffer.[0]
            buffer <- buffer.Slice 1
            value
        elif propertyType = typeof<uint16> then
            Buffer.readUInt16 &buffer bigEndian |> box
        elif propertyType = typeof<uint> then
            Buffer.readUInt &buffer bigEndian |> box
        elif propertyType = typeof<uint64> then
            Buffer.readUInt64 &buffer bigEndian |> box
        elif propertyType = typeof<float32> then
            Buffer.readFloat32 &buffer bigEndian |> box
        elif propertyType = typeof<float> then
            Buffer.readFloat &buffer bigEndian |> box
        elif propertyType = typeof<struct (float32 * float32)> then
            struct (Buffer.readFloat32 &buffer bigEndian |> float, Buffer.readFloat32 &buffer bigEndian |> float)
            |> box
        elif propertyType = typeof<Complex> then
            Complex(Buffer.readFloat &buffer bigEndian, Buffer.readFloat &buffer bigEndian)
            |> box
        elif propertyType = typeof<float80> then
            Buffer.readFloat80 &buffer bigEndian |> box
        elif propertyType = typeof<Timestamp> then
            (if bigEndian then
                 { SecondsSinceNiEpoch = Buffer.readInt64 &buffer bigEndian
                   FractionsOfASecond = Buffer.readUInt64 &buffer bigEndian }
             else
                 { FractionsOfASecond = Buffer.readUInt64 &buffer bigEndian
                   SecondsSinceNiEpoch = Buffer.readInt64 &buffer bigEndian })
            |> box
        elif propertyType = typeof<string> then
            Buffer.readString &buffer bigEndian |> box
        else
            failwithf "Property type not implemented: %A" propertyType

    let createObject name (objects: _ List) bigEndian =
        match Seq.cast objects
              |> Seq.tryFind (fun object -> object.Name = name) with
        | Some object -> object
        | None ->
            let object =
                { Name = name
                  BigEndian = bigEndian
                  RawDataBlocks = None
                  Properties = ResizeArray() }

            objects.Add object
            object

    let readMetaData { Objects = objects } rawDataOffset (buffer: _ byref) bigEndian interleaved =
        let objectCount = Buffer.readInt &buffer bigEndian
        let newOrUpdatedObjects = Array.zeroCreate objectCount
        let mutable rawDataPosition = rawDataOffset

        for i = 0 to objectCount - 1 do
            let object =
                createObject (Buffer.readString &buffer bigEndian) objects bigEndian

            newOrUpdatedObjects.[i] <- object

            rawDataPosition <-
                rawDataPosition
                + RawDataBlock.readRawDataIndex object rawDataPosition &buffer bigEndian interleaved

            let propertyCount = Buffer.readUInt &buffer bigEndian |> int

            for j = 0 to propertyCount - 1 do
                let propertyName = Buffer.readString &buffer bigEndian
                let propertyType = Buffer.readType &buffer bigEndian

                let propertyValue =
                    readPropertyValue &buffer bigEndian propertyType

                let property =
                    { Name = propertyName
                      Type = propertyType
                      Raw = propertyValue }

                match Seq.tryFindIndex (fun (property: Property) -> property.Name = propertyName) object.Properties with
                | None -> object.Properties.Add property
                | Some index -> object.Properties.[index] <- property

        if interleaved then
            Array.iter
                (fun { RawDataBlocks = rawDataBlocks } ->
                    match rawDataBlocks with
                    | None -> ()
                    | Some (StringRawDataBlocks _) -> ()
                    | Some (PrimitiveRawDataBlocks (ty, primitiveRawDataBlocksArray)) ->
                        Seq.tryLast primitiveRawDataBlocksArray
                        |> Option.iter
                            (function
                            | DecimatedPrimitiveRawDataBlock _ -> ()
                            | InterleavedPrimitiveRawDataBlock interleavedPrimitiveRawDataBlock ->
                                interleavedPrimitiveRawDataBlock.Skip <-
                                    (rawDataPosition - rawDataOffset)
                                    - uint64 (Marshal.SizeOf ty)))
                newOrUpdatedObjects

    let readMetaDataMemory index (rawDataOffset: uint64) (memory: byte ReadOnlyMemory) bigEndian interleaved =
        let mutable buffer = memory.Span
        readMetaData index rawDataOffset &buffer bigEndian interleaved

    (*let chunkDataSize indices = List.sum (List.map (fun i ->
      match i with
        | None -> 0uL
        | Some(OtherType(ty, d, c)) -> uint64 (Type.size ty |> Option.defaultValue 1u) * uint64 d * c
        | Some(String(_, _, _, s)) -> s
    ) indices)

  let countChunks indices nextSegmentOffset rawDataOffset =
    let totalDataSize = nextSegmentOffset - rawDataOffset
    let chunkSize = chunkDataSize indices
    if chunkSize = 0uL then 0uL else totalDataSize / chunkSize*)

    let tdsh = [| 0x54uy; 0x44uy; 0x53uy; 0x68uy |]

    let read offset fromIndex index leadInBuffer (stream: Stream) (indexStream: Stream) =
        stream.Read(leadInBuffer, 0, 28) |> ignore
        let mutable leadInSpan = ReadOnlySpan leadInBuffer
        let writeIndex = isNull indexStream |> not

        if writeIndex then
            indexStream.Write(tdsh, 0, 4)
            indexStream.Write(leadInBuffer, 4, 24)

        let leadIn = readLeadIn &leadInSpan
        let metaDataStart = offset + 28uL

        if leadIn.TableOfContents.HasFlag(TableOfContents.ContainsMetaData) then
            let remainingLength = int leadIn.RawDataOffset

            let buffer =
                ArrayPool<byte>.Shared.Rent remainingLength

            stream.Read(buffer, 0, remainingLength) |> ignore
            let mutable span = ReadOnlySpan buffer

            if writeIndex then
                indexStream.Write(buffer, 0, remainingLength)

            readMetaData
                index
                (metaDataStart + leadIn.RawDataOffset)
                &span
                (leadIn.TableOfContents.HasFlag(TableOfContents.ContainsBigEndianData))
                (leadIn.TableOfContents.HasFlag(TableOfContents.ContainsInterleavedData))

            ArrayPool<byte>.Shared.Return(buffer, false)

        let nextSegmentOffset = metaDataStart + leadIn.NextSegmentOffset

        if not fromIndex then
            stream.Seek(int64 nextSegmentOffset, SeekOrigin.Begin)
            |> ignore

        nextSegmentOffset

    let readAsync offset fromIndex index leadInBuffer (stream: Stream) (indexStream: Stream) =
        task {
            let! _ = stream.ReadAsync(leadInBuffer, 0, 28)
            let mutable leadInMemory = ReadOnlyMemory leadInBuffer
            let writeIndex = isNull indexStream |> not

            if writeIndex then
                do! indexStream.WriteAsync(tdsh, 0, 4)
                do! indexStream.WriteAsync(leadInBuffer, 4, 24)

            let leadIn = readLeadInMemory leadInMemory
            let metaDataStart = offset + 28uL

            if leadIn.TableOfContents.HasFlag(TableOfContents.ContainsMetaData) then
                let remainingLength = int leadIn.RawDataOffset

                let buffer =
                    ArrayPool<byte>.Shared.Rent remainingLength

                let! _ = stream.ReadAsync(buffer, 0, remainingLength)
                let mutable memory = ReadOnlyMemory buffer

                if writeIndex then
                    do! indexStream.WriteAsync(buffer, 0, remainingLength)

                readMetaDataMemory
                    index
                    (metaDataStart + leadIn.RawDataOffset)
                    memory
                    (leadIn.TableOfContents.HasFlag(TableOfContents.ContainsBigEndianData))
                    (leadIn.TableOfContents.HasFlag(TableOfContents.ContainsBigEndianData))

                ArrayPool<byte>.Shared.Return(buffer, false)

            let nextSegmentOffset = metaDataStart + leadIn.NextSegmentOffset

            if not fromIndex then
                stream.Seek(int64 nextSegmentOffset, SeekOrigin.Begin)
                |> ignore

            return nextSegmentOffset
        }

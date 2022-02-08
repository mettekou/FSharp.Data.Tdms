namespace FSharp.Data.Tdms

open System
open System.Buffers
open System.IO
open System.Numerics
open System.Runtime.InteropServices

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

module Segment =

    let readLeadIn (buffer: byte ReadOnlySpan byref) =
        let tag =
            Buffer.readUInt &buffer false
            |> LanguagePrimitives.EnumOfValue<uint, Tag>

        let tableOfContents =
            Buffer.readUInt &buffer false
            |> LanguagePrimitives.EnumOfValue<uint, TableOfContents>

        let bigEndian =
            tableOfContents.HasFlag(TableOfContents.ContainsBigEndianData)

        { Tag = tag
          TableOfContents = tableOfContents
          Version =
              Buffer.readUInt &buffer bigEndian
              |> LanguagePrimitives.EnumOfValue<uint, Version>
          NextSegmentOffset = Buffer.readUInt64 &buffer bigEndian
          RawDataOffset = Buffer.readUInt64 &buffer bigEndian }

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

    let createObject name (objects: _ ResizeArray) bigEndian =
        match Seq.tryFind (fun object -> object.Name = name) objects with
        | Some object -> object
        | None ->
            let object =
                { Name = name
                  BigEndian = bigEndian
                  RawDataBlocks = None
                  Properties = ResizeArray() }

            objects.Add object
            object

    let readMetaData objects rawDataOffset nextSegmentOffset (buffer: _ byref) bigEndian interleaved =
        let objectCount = Buffer.readInt &buffer bigEndian
        let newOrUpdatedObjects = Array.zeroCreate objectCount
        let objectsWithRawData = ResizeArray()
        let mutable rawDataPosition = rawDataOffset

        for i = 0 to objectCount - 1 do
            let object =
                createObject (Buffer.readString &buffer bigEndian) objects bigEndian

            newOrUpdatedObjects.[i] <- object

            let rawDataSkip =
                Object.readRawDataIndex object rawDataPosition &buffer bigEndian interleaved

            rawDataPosition <- rawDataPosition + rawDataSkip

            if rawDataSkip > 0uL then
                objectsWithRawData.Add object

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

        let sizes, rawDataBlocksToUpdate =
            Seq.choose
                (fun ({ RawDataBlocks = rawDataBlocks }: FSharp.Data.Tdms.Object) ->
                    Option.bind
                        (fun rawDataBlocks' ->
                            match rawDataBlocks' with
                            | PrimitiveRawDataBlocks (ty, primitiveRawDataBlockArray) ->
                                Seq.tryLast primitiveRawDataBlockArray
                                |> Option.map
                                    (function
                                    | DecimatedPrimitiveRawDataBlock (start, count) ->
                                        uint64 (Marshal.SizeOf ty) * count, rawDataBlocks'
                                    | InterleavedPrimitiveRawDataBlock { Start = start; Count = count } ->
                                        uint64 (Marshal.SizeOf ty) * count, rawDataBlocks')
                            | StringRawDataBlocks stringRawDataBlockArray ->
                                Seq.tryLast stringRawDataBlockArray
                                |> Option.map (fun (_, _, bytes) -> bytes, rawDataBlocks'))
                        rawDataBlocks)
                objectsWithRawData
            |> Seq.toArray
            |> Array.unzip

        let chunkSize = Array.sum sizes

        if chunkSize > 0uL then
            let chunkOffsets =
                [ chunkSize .. chunkSize .. (nextSegmentOffset - rawDataOffset) - chunkSize ]

            for rawDataBlocks in rawDataBlocksToUpdate do
                match rawDataBlocks with
                | PrimitiveRawDataBlocks (_, primitiveRawDataBlockArray) ->
                    Seq.tryLast primitiveRawDataBlockArray
                    |> Option.iter
                        (function
                        | DecimatedPrimitiveRawDataBlock (start, count) ->
                            primitiveRawDataBlockArray.AddRange(
                                List.map
                                    (fun chunkOffset -> DecimatedPrimitiveRawDataBlock(start + chunkOffset, count))
                                    chunkOffsets
                            )
                        | InterleavedPrimitiveRawDataBlock ({ Start = start } as block) ->
                            primitiveRawDataBlockArray.AddRange(
                                List.map
                                    (fun chunkOffset ->
                                        InterleavedPrimitiveRawDataBlock
                                            { block with
                                                  Start = start + chunkOffset })
                                    chunkOffsets
                            ))
                | StringRawDataBlocks stringRawDataBlockArray ->
                    Seq.tryLast stringRawDataBlockArray
                    |> Option.iter
                        (fun (start, count, bytes) ->
                            stringRawDataBlockArray.AddRange(
                                List.map (fun chunkOffset -> start + chunkOffset, count, bytes) chunkOffsets
                            ))

        if interleaved then
            Array.iter
                (fun ({ RawDataBlocks = rawDataBlocks }: FSharp.Data.Tdms.Object) ->
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

    let tdsh =
        ReadOnlyMemory [| 0x54uy
                          0x44uy
                          0x53uy
                          0x68uy |]

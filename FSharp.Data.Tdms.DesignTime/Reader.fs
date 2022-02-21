namespace FSharp.Data.Tdms

open System
open System.Buffers
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Text

module Reader =

    let readPrimitiveRawData<'t when 't: struct and 't: (new : unit -> 't) and 't :> ValueType>
        (stream: Stream)
        (segments: PrimitiveRawDataBlock seq)
        bigEndian
        =
        let mutable position = 0
        let size = sizeof<'t>

        let data =
            Seq.sumBy
                (function
                | DecimatedPrimitiveRawDataBlock (_, size) -> size
                | InterleavedPrimitiveRawDataBlock { Count = count } -> count)
                segments
            |> int
            |> Array.zeroCreate<'t>

        let buffer =
            ArrayPool<byte>.Shared.Rent
                (Seq.map
                    (function
                    | DecimatedPrimitiveRawDataBlock (_, length) -> uint64 size * length
                    | InterleavedPrimitiveRawDataBlock { Count = count } -> uint64 size * count)
                    segments
                 |> Seq.max
                 |> int)

        for segment in segments do
            match segment with
            | DecimatedPrimitiveRawDataBlock (start, length) ->

                let length' = int length

                stream.Seek(int64 start, SeekOrigin.Begin)
                |> ignore

                stream.Read(buffer, 0, length' * size)
                |> ignore

                if bigEndian && size > 1 then
                    buffer.AsSpan(0, length' * size).Reverse()

                Buffer.BlockCopy(buffer, 0, data, position * size, length' * size)

                if bigEndian && size > 1 then
                    data.AsSpan(position, length').Reverse()

                position <- position + length'
            | InterleavedPrimitiveRawDataBlock { Start = start
                                                 Count = count
                                                 Skip = skip } ->
                stream.Seek(int64 start, SeekOrigin.Begin)
                |> ignore

                for _ = 0 to int count - 1 do
                    stream.Read(buffer, 0, size)
                    |> ignore

                    if bigEndian && size > 1 then
                        buffer.AsSpan(0, size).Reverse()

                    Buffer.BlockCopy(buffer, 0, data, position, size)

                    stream.Seek(int64 skip, SeekOrigin.Current)
                    |> ignore

                    position <- position + 1

        ArrayPool.Shared.Return buffer
        data

    let readComplexRawData (stream: Stream) (segments: PrimitiveRawDataBlock seq) bigEndian =
        if not bigEndian then
            readPrimitiveRawData<Complex> stream segments false
        else
            let mutable position = 0
            let size = sizeof<Complex>

            let data =
                Seq.sumBy
                    (function
                    | DecimatedPrimitiveRawDataBlock (_, size) -> size
                    | InterleavedPrimitiveRawDataBlock { Count = count } -> count)
                    segments
                |> int
                |> Array.zeroCreate<Complex>

            let buffer =
                ArrayPool<byte>.Shared.Rent
                    (Seq.map
                        (function
                        | DecimatedPrimitiveRawDataBlock (_, length) -> uint64 size * length
                        | InterleavedPrimitiveRawDataBlock { Count = count } -> uint64 size * count)
                        segments
                     |> Seq.max
                     |> int)

            for segment in segments do
                match segment with
                | DecimatedPrimitiveRawDataBlock (start, length) ->
                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    let length' = int length

                    stream.Read(buffer, 0, length' * size)
                    |> ignore

                    if bigEndian then
                        buffer.AsSpan(0, length' * size).Reverse()

                    Buffer.BlockCopy(buffer, 0, data, position * size, length' * size)

                    if bigEndian then
                        MemoryMarshal
                            .Cast<Complex, double>(data.AsSpan(position, length'))
                            .Reverse()

                    position <- position + int length
                | InterleavedPrimitiveRawDataBlock { Start = start
                                                     Count = count
                                                     Skip = skip } ->
                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    for _ = 0 to int count - 1 do
                        stream.Read(buffer, 0, size)
                        |> ignore

                        if bigEndian then
                            buffer.AsSpan(0, size).Reverse()

                        Buffer.BlockCopy(buffer, 0, data, position, size)

                        if bigEndian then
                            MemoryMarshal
                                .Cast<Complex, double>(data.AsSpan(position, 1))
                                .Reverse()

                        stream.Seek(int64 skip, SeekOrigin.Current)
                        |> ignore

                        position <- position + 1

            ArrayPool.Shared.Return buffer
            data

    let readTimestampRawData (stream: Stream) (segments: PrimitiveRawDataBlock seq) bigEndian =
        if not bigEndian then
            readPrimitiveRawData<Timestamp> stream segments false
        else
            let mutable position = 0
            let size = sizeof<Timestamp>

            let data =
                Seq.sumBy
                    (function
                    | DecimatedPrimitiveRawDataBlock (_, size) -> size
                    | InterleavedPrimitiveRawDataBlock { Count = count } -> count)
                    segments
                |> int
                |> Array.zeroCreate<Timestamp>

            let buffer =
                ArrayPool<byte>.Shared.Rent
                    (Seq.map
                        (function
                        | DecimatedPrimitiveRawDataBlock (_, length) -> uint64 size * length
                        | InterleavedPrimitiveRawDataBlock { Count = count } -> uint64 size * count)
                        segments
                     |> Seq.max
                     |> int)

            for segment in segments do
                match segment with
                | DecimatedPrimitiveRawDataBlock (start, length) ->
                    let length' = int length

                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    stream.Read(buffer, 0, length' * size)
                    |> ignore

                    if bigEndian && size > 1 then
                        buffer.AsSpan(0, length' * size).Reverse()

                    Buffer.BlockCopy(buffer, 0, data, position * size, length' * size)

                    if bigEndian && size > 1 then
                        data.AsSpan(position, length').Reverse()

                    position <- position + length'
                | InterleavedPrimitiveRawDataBlock { Start = start
                                                     Count = count
                                                     Skip = skip } ->
                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    for _ = 0 to int count - 1 do
                        stream.Read(buffer, 0, size)
                        |> ignore

                        if bigEndian then
                            buffer.AsSpan(0, size).Reverse()

                        Buffer.BlockCopy(buffer, 0, data, position, size)

                        stream.Seek(int64 skip, SeekOrigin.Current)
                        |> ignore

                        position <- position + 1

            ArrayPool<byte>.Shared.Return buffer
            data

    let readFloat80RawData (stream: Stream) (segments: PrimitiveRawDataBlock seq) bigEndian =
        if not bigEndian then
            readPrimitiveRawData<float80> stream segments false
        else
            let mutable position = 0
            let size = sizeof<float80>

            let data =
                Seq.sumBy
                    (function
                    | DecimatedPrimitiveRawDataBlock (_, size) -> size
                    | InterleavedPrimitiveRawDataBlock { Count = count } -> count)
                    segments
                |> int
                |> Array.zeroCreate<float80>

            let buffer =
                ArrayPool<byte>.Shared.Rent
                    (Seq.map
                        (function
                        | DecimatedPrimitiveRawDataBlock (_, length) -> uint64 size * length
                        | InterleavedPrimitiveRawDataBlock { Count = count } -> uint64 size * count)
                        segments
                     |> Seq.max
                     |> int)

            for segment in segments do
                match segment with
                | DecimatedPrimitiveRawDataBlock (start, length) ->
                    let length' = int length
                    
                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    stream.Read(buffer, 0, length' * size)
                    |> ignore
                    
                    if bigEndian && size > 1 then
                        buffer.AsSpan(0, length' * size).Reverse()

                    Buffer.BlockCopy(buffer, 0, data, position * size, length' * size)

                    if bigEndian && size > 1 then
                        data.AsSpan(position, length').Reverse()

                    position <- position + length'
                | InterleavedPrimitiveRawDataBlock { Start = start
                                                     Count = count
                                                     Skip = skip } ->
                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    for _ = 0 to int count - 1 do
                        stream.Read(buffer, 0, size)
                        |> ignore

                        stream.Seek(int64 skip, SeekOrigin.Current)
                        |> ignore

                        position <- position + 1

            ArrayPool<byte>.Shared.Return buffer
            data

    let readStringRawData (stream: Stream) (segments: (uint64 * uint64 * uint64) seq) bigEndian =
        let offsets =
            Seq.map (fun (_, length, _) -> length) segments
            |> Seq.max
            |> int
            |> ArrayPool<byte>.Shared.Rent

        let data =
            Seq.map (fun (_, _, bytes) -> bytes) segments
            |> Seq.max
            |> int
            |> ArrayPool<uint8>.Shared.Rent

        let strings =
            Seq.sumBy (fun (_, length, _) -> length) segments
            |> int
            |> Array.zeroCreate<string>

        let mutable position = 0

        for (start, length, bytes) in segments do
            stream.Seek(int64 start, SeekOrigin.Begin)
            |> ignore

            let offsetsByteSpan = offsets.AsSpan().Slice(0, int length)

            stream.Read(offsets, 0, int length) |> ignore

            let mutable dataSpan = data.AsSpan().Slice(0, int bytes)

            stream.Read(data, 0, int bytes) |> ignore

            let mutable offset = 0

            for index = 0 to offsetsByteSpan.Length - 1 do
                strings.[position] <-
                    Encoding.UTF8.GetString(
                        dataSpan
                            .Slice(0, int (sbyte offsets.[index]) - offset)
                            .ToArray()
                    )

                dataSpan <- dataSpan.Slice(int offsets.[index] - offset)
                offset <- int offsets.[index]
                position <- position + 1

        ArrayPool<byte>.Shared.Return (data, false)
        ArrayPool<byte>.Shared.Return (offsets, false)
        strings

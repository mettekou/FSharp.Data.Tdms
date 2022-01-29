namespace FSharp.Data.Tdms

open System
open System.Buffers
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Text

module Reader =

    type CastMemoryManager<'tfrom, 'tto when 'tfrom: struct and 'tfrom: (new: unit -> 'tfrom) and 'tfrom :> ValueType and 'tto: struct and 'tto: (new: unit
                                                                                                                                                       -> 'tto) and 'tto :> ValueType>(memory: Memory<'tfrom>) =
        inherit MemoryManager<'tto>()

        override _.GetSpan() =
            MemoryMarshal.Cast<'tfrom, 'tto> memory.Span

        override _.Dispose _ = ()
        override _.Pin _ = raise (NotSupportedException())
        override _.Unpin() = raise (NotSupportedException())

    let cast<'tfrom, 'tto when 'tfrom: struct and 'tfrom: (new: unit -> 'tfrom) and 'tfrom :> ValueType and 'tto: struct and 'tto: (new: unit
                                                                                                                                         -> 'tto) and 'tto :> ValueType>
        (memory: Memory<'tfrom>)
        =
        if typeof<'tfrom> = typeof<'tto> then
            box memory :?> Memory<'tto>
        else
            (new CastMemoryManager<'tfrom, 'tto>(memory))
                .Memory

    let readPrimitiveRawData<'t when 't: struct and 't: (new: unit -> 't) and 't :> ValueType>
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

        let span =
            MemoryMarshal.Cast<'t, byte>(data.AsSpan())

        for segment in segments do
            match segment with
            | DecimatedPrimitiveRawDataBlock (start, length) ->
                stream.Seek(int64 start, SeekOrigin.Begin)
                |> ignore

                stream.Read(span.Slice(position * size, int length * size))
                |> ignore

                position <- position + int length
            | InterleavedPrimitiveRawDataBlock { Start = start
                                                 Count = count
                                                 Skip = skip } ->
                stream.Seek(int64 start, SeekOrigin.Begin)
                |> ignore

                for _ = 0 to int count - 1 do
                    stream.Read(span.Slice(position * size, size))
                    |> ignore

                    stream.Seek(int64 skip, SeekOrigin.Current)
                    |> ignore

                    position <- position + 1


        if bigEndian && size > 1 then
            span.Reverse()
            Array.Reverse data

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

            let span =
                MemoryMarshal.Cast<Complex, byte>(data.AsSpan())

            for segment in segments do
                match segment with
                | DecimatedPrimitiveRawDataBlock (start, length) ->
                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    stream.Read(span.Slice(position * size, int length * size))
                    |> ignore

                    position <- position + int length
                | InterleavedPrimitiveRawDataBlock { Start = start
                                                     Count = count
                                                     Skip = skip } ->
                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    for _ = 0 to int count - 1 do
                        stream.Read(span.Slice(position * size, size))
                        |> ignore

                        stream.Seek(int64 skip, SeekOrigin.Current)
                        |> ignore

                        position <- position + 1

            span.Reverse()

            MemoryMarshal
                .Cast<Complex, double>(data.AsSpan())
                .Reverse()

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

            let span =
                MemoryMarshal.Cast<Timestamp, byte>(data.AsSpan())

            for segment in segments do
                match segment with
                | DecimatedPrimitiveRawDataBlock (start, length) ->
                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    stream.Read(span.Slice(position * size, int length * size))
                    |> ignore

                    position <- position + int length
                | InterleavedPrimitiveRawDataBlock { Start = start
                                                     Count = count
                                                     Skip = skip } ->
                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    for _ = 0 to int count - 1 do
                        stream.Read(span.Slice(position * size, size))
                        |> ignore

                        stream.Seek(int64 skip, SeekOrigin.Current)
                        |> ignore

                        position <- position + 1

            span.Reverse()
            Array.Reverse data
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

            let span =
                MemoryMarshal.Cast<float80, byte>(data.AsSpan())

            for segment in segments do
                match segment with
                | DecimatedPrimitiveRawDataBlock (start, length) ->

                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    stream.Read(span.Slice(position * size, int length * size))
                    |> ignore

                    position <- position + int length
                | InterleavedPrimitiveRawDataBlock { Start = start
                                                     Count = count
                                                     Skip = skip } ->
                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    for _ = 0 to int count - 1 do
                        stream.Read(span.Slice(position * size, size))
                        |> ignore

                        stream.Seek(int64 skip, SeekOrigin.Current)
                        |> ignore

                        position <- position + 1

            MemoryMarshal.Cast<byte, float80>(span).Reverse()

            Array.Reverse data
            data

    let readStringRawData (stream: Stream) (segments: (uint64 * uint64 * uint64) seq) bigEndian =
        let offsets =
            Seq.map (fun (_, length, _) -> length) segments
            |> Seq.max
            |> int
            |> ArrayPool<uint>.Shared.Rent

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

            let offsetsSpan = offsets.AsSpan().Slice(0, int length)

            let offsetsByteSpan =
                MemoryMarshal.Cast<uint, uint8>(offsetsSpan)

            stream.Read(offsetsByteSpan) |> ignore

            if bigEndian then
                offsetsByteSpan.Reverse()
                offsetsSpan.Reverse()

            let mutable dataSpan = data.AsSpan().Slice(0, int bytes)

            stream.Read(dataSpan) |> ignore

            let mutable offset = 0

            for index = 0 to offsetsSpan.Length - 1 do
                strings.[position] <-
                    Encoding.UTF8.GetString(
                        dataSpan
                            .Slice(0, int offsets.[index] - offset)
                            .ToArray()
                    )

                dataSpan <- dataSpan.Slice(int offsets.[index] - offset)
                offset <- int offsets.[index]
                position <- position + 1

        ArrayPool<byte>.Shared.Return(data, false)
        ArrayPool<uint>.Shared.Return(offsets, false)
        strings

    let readPrimitiveRawDataAsync<'t when 't: struct and 't: (new: unit -> 't) and 't :> ValueType>
        ct
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

        let memory = cast<'t, byte> (data.AsMemory())

        task {
            for segment in segments do
                match segment with
                | DecimatedPrimitiveRawDataBlock (start, length) ->
                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    let! _ = stream.ReadAsync(memory.Slice(position * size, int length * size), ct)

                    position <- position + int length
                | InterleavedPrimitiveRawDataBlock { Start = start
                                                     Count = count
                                                     Skip = skip } ->
                    stream.Seek(int64 start, SeekOrigin.Begin)
                    |> ignore

                    for _ = 0 to int count - 1 do
                        let! _ = stream.ReadAsync(memory.Slice(position * size, size), ct)

                        stream.Seek(int64 skip, SeekOrigin.Current)
                        |> ignore

                        position <- position + 1

            if bigEndian then
                memory.Span.Reverse()
                Array.Reverse data

            return data
        }

    let readFloat80RawDataAsync ct (stream: Stream) (segments: PrimitiveRawDataBlock seq) bigEndian =
        if not bigEndian then
            readPrimitiveRawDataAsync<float80> ct stream segments false
        else
            task {
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

                let memory = cast<float80, byte> (data.AsMemory())

                for segment in segments do
                    match segment with
                    | DecimatedPrimitiveRawDataBlock (start, length) ->

                        stream.Seek(int64 start, SeekOrigin.Begin)
                        |> ignore

                        let! _ = stream.ReadAsync(memory.Slice(position * size, int length * size), ct)

                        position <- position + int length
                    | InterleavedPrimitiveRawDataBlock { Start = start
                                                         Count = count
                                                         Skip = skip } ->
                        stream.Seek(int64 start, SeekOrigin.Begin)
                        |> ignore

                        for _ = 0 to int count - 1 do
                            let! _ = stream.ReadAsync(memory.Slice(position * size, size), ct)

                            stream.Seek(int64 skip, SeekOrigin.Current)
                            |> ignore

                            position <- position + 1

                MemoryMarshal
                    .Cast<byte, float80>(memory.Span)
                    .Reverse()

                Array.Reverse data
                return data
            }

    let readComplexRawDataAsync ct (stream: Stream) (segments: PrimitiveRawDataBlock seq) bigEndian =
        if not bigEndian then
            readPrimitiveRawDataAsync<Complex> ct stream segments false
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

            let memory = cast<Complex, byte> (data.AsMemory())

            task {
                for segment in segments do
                    match segment with
                    | DecimatedPrimitiveRawDataBlock (start, length) ->
                        stream.Seek(int64 start, SeekOrigin.Begin)
                        |> ignore

                        let! _ = stream.ReadAsync(memory.Slice(position * size, int length * size), ct)

                        position <- position + int length
                    | InterleavedPrimitiveRawDataBlock { Start = start
                                                         Count = count
                                                         Skip = skip } ->
                        stream.Seek(int64 start, SeekOrigin.Begin)
                        |> ignore

                        for _ = 0 to int count - 1 do
                            let! _ = stream.ReadAsync(memory.Slice(position * size, size), ct)

                            stream.Seek(int64 skip, SeekOrigin.Current)
                            |> ignore

                            position <- position + 1

                memory.Span.Reverse()

                cast<Complex, double>(data.AsMemory())
                    .Span.Reverse()

                return data
            }

    let readTimestampRawDataAsync ct (stream: Stream) (segments: PrimitiveRawDataBlock seq) bigEndian =
        if not bigEndian then
            readPrimitiveRawDataAsync<Timestamp> ct stream segments false
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

            let memory = cast<Timestamp, byte> (data.AsMemory())

            task {
                for segment in segments do
                    match segment with
                    | DecimatedPrimitiveRawDataBlock (start, length) ->
                        stream.Seek(int64 start, SeekOrigin.Begin)
                        |> ignore

                        let! _ = stream.ReadAsync(memory.Slice(position * size, int length * size), ct)

                        position <- position + int length
                    | InterleavedPrimitiveRawDataBlock { Start = start
                                                         Count = count
                                                         Skip = skip } ->
                        stream.Seek(int64 start, SeekOrigin.Begin)
                        |> ignore

                        for _ = 0 to int count - 1 do
                            let! _ = stream.ReadAsync(memory.Slice(position * size, size), ct)

                            stream.Seek(int64 skip, SeekOrigin.Current)
                            |> ignore

                            position <- position + 1

                memory.Span.Reverse()
                Array.Reverse data
                return data
            }

    let readStringRawDataAsync ct (stream: Stream) (segments: (uint64 * uint64 * uint64) seq) bigEndian =
        task {
            let offsets =
                Seq.map (fun (_, length, _) -> length) segments
                |> Seq.max
                |> int
                |> ArrayPool<uint>.Shared.Rent

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

                let offsetsMemory = offsets.AsMemory().Slice(0, int length)

                let offsetsByteMemory = cast<uint, uint8> offsetsMemory

                let! _ = stream.ReadAsync(offsetsByteMemory, ct)

                if bigEndian then
                    offsetsByteMemory.Span.Reverse()
                    offsetsMemory.Span.Reverse()

                let mutable dataMemory = data.AsMemory().Slice(0, int bytes)

                let! _ = stream.ReadAsync(dataMemory, ct)

                let mutable offset = 0

                for index = 0 to offsetsMemory.Length - 1 do
                    strings.[position] <-
                        Encoding.UTF8.GetString(
                            dataMemory
                                .Slice(0, int offsets.[index] - offset)
                                .ToArray()
                        )

                    dataMemory <- dataMemory.Slice(int offsets.[index] - offset)
                    offset <- int offsets.[index]
                    position <- position + 1

            ArrayPool<byte>.Shared.Return(data, false)
            ArrayPool<uint>.Shared.Return(offsets, false)
            return strings
        }

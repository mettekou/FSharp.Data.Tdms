namespace FSharp.Data.Tdms

open System
open System.Buffers
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open FSharp.Control.Tasks.NonAffine

module Reader =

    type CastMemoryManager<'tfrom, 'tto when 'tfrom : struct and 'tfrom : (new : unit -> 'tfrom) and 'tfrom :> ValueType and 'tto: struct and 'tto : (new : unit -> 'tto) and 'tto :> ValueType>(memory: Memory<'tfrom>) =
        inherit MemoryManager<'tto>()
        override _.GetSpan() = MemoryMarshal.Cast<'tfrom, 'tto> memory.Span
        override _.Dispose _ = ()
        override _.Pin _ = raise (NotSupportedException())
        override _.Unpin() = raise (NotSupportedException())

    let cast<'tfrom, 'tto when 'tfrom : struct and 'tfrom : (new : unit -> 'tfrom) and 'tfrom :> ValueType and 'tto: struct and 'tto : (new : unit -> 'tto) and 'tto :> ValueType> (memory: Memory<'tfrom>) =
        if typeof<'tfrom> = typeof<'tto>
        then box memory :?> Memory<'tto>
        else (new CastMemoryManager<'tfrom, 'tto>(memory)).Memory

    let readPrimitiveRawData<'t when 't : struct and 't : (new : unit -> 't) and 't :> ValueType> (stream: Stream) (segments: (uint64 * uint64) seq) bigEndian =
        let mutable position = 0
        let size = sizeof<'t>
        let data = Seq.sumBy snd segments |> int |> Array.zeroCreate<'t>
        let span = MemoryMarshal.Cast<'t, byte>(data.AsSpan())
        for (start, length) in segments do
            stream.Seek(int64 start, SeekOrigin.Begin) |> ignore
            stream.Read(span.Slice(position * size, int length * size)) |> ignore
            position <- position + int length
        if bigEndian && size > 1 then 
            span.Reverse()
            Array.Reverse data
        data

    let readComplexRawData (stream: Stream) (segments: (uint64 * uint64) seq) bigEndian =
        if not bigEndian then
            readPrimitiveRawData<Complex> stream segments false
        else
            let mutable position = 0
            let size = sizeof<Complex>
            let data = Seq.sumBy snd segments |> int |> Array.zeroCreate<Complex>
            let span = MemoryMarshal.Cast<Complex, byte>(data.AsSpan())
            for (start, length) in segments do
                stream.Seek(int64 start, SeekOrigin.Begin) |> ignore
                stream.Read(span.Slice(position * size, int length * size)) |> ignore
                position <- position + int length
            span.Reverse()
            MemoryMarshal.Cast<Complex, double>(data.AsSpan()).Reverse()
            data

    let readPrimitiveRawDataAsync<'t when 't : struct and 't : (new : unit -> 't) and 't :> ValueType> (stream: Stream) (segments: (uint64 * uint64) seq) bigEndian =
        let mutable position = 0
        let size = sizeof<'t>
        let data = Seq.sumBy snd segments |> int |> Array.zeroCreate<'t>
        let memory = cast<'t, byte> (data.AsMemory())

        task {
            for (start, length) in segments do
                stream.Seek(int64 start, SeekOrigin.Begin) |> ignore

                let! _ = stream.ReadAsync(memory.Slice(position * size, int length * size))

                position <- position + int length

            if bigEndian then 
                memory.Span.Reverse()
                Array.Reverse data
            return data
        }

    let readComplexRawDataAsync (stream: Stream) (segments: (uint64 * uint64) seq) bigEndian =
        if not bigEndian then
            readPrimitiveRawDataAsync<Complex> stream segments false
        else
            let mutable position = 0
            let size = sizeof<Complex>
            let data = Seq.sumBy snd segments |> int |> Array.zeroCreate<Complex>
            let memory = cast<Complex, byte> (data.AsMemory())

            task {
                for (start, length) in segments do
                    stream.Seek(int64 start, SeekOrigin.Begin) |> ignore

                    let! _ = stream.ReadAsync(memory.Slice(position * size, int length * size))

                    position <- position + int length

                memory.Span.Reverse()
                cast<Complex, double>(data.AsMemory()).Span.Reverse()
                return data
            }
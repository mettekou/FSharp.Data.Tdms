namespace FSharp.Data.Tdms

open System
open System.Buffers
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices
open FSharp.Control.Tasks.NonAffine

module Reader =

    //let readString = failwith "Not implemented"

    let readRawDataIndex (bytes: byref<Span<byte>>) existingObjectMetadata bigEndian interleaved =
        failwith "Not implemented"

    let readObjectProperties (bytes: byref<Span<byte>>) existingObjectMetadata bigEndian = failwith "Not implemented"

    let readMetadata (bytes: byref<ReadOnlySpan<byte>>) (index: Dictionary<string, obj>) bigEndian interleaved =
        let objectCountBytes = bytes.Slice(0, sizeof<uint>)
        (*if bigEndian then
            objectCountBytes.Reverse()*)
        let objectCount = BitConverter.ToUInt32 objectCountBytes

        let objectPaths = List<string>(int objectCount)

        bytes <- bytes.Slice sizeof<uint>
        for objectIndex = 0 to int objectCount - 1 do
            let objectPath = ""//readString (&bytes)
            objectPaths.Add(objectPath)

            let gotExistingObjectMetadata, existingObjectMetadata = index.TryGetValue objectPath

            if not gotExistingObjectMetadata then
                //existingObjectMetadata <- TdmsObjectMetadata bigEndian
                index.Add(objectPath, existingObjectMetadata)

    // readRawDataIndex &bytes existingObjectMetadata bigEndian interleaved
    // readObjectProperties &bytes existingObjectMetadata bigEndian

    let readIndexAsync path =
        let mutable bufferLength = 8_192

        use fileStream =
            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, bufferLength, true)

        let mutable filePosition = 0L
        let index = Dictionary<string, obj>()
        let fileLength = fileStream.Length
        let arrayPool = ArrayPool<byte>.Shared
        let mutable buffer = arrayPool.Rent bufferLength
        bufferLength <- buffer.Length // We may get back a different length array from the pool than the length we asked for
        let mutable refill = bufferLength

        task {
            while filePosition < fileLength do
                while (*not memory.IsEmpty &&*) refill = 0 do
                    let mutable memory = (ReadOnlyMemory<byte> buffer).Slice 4
                    let tableOfContents = BitConverter.ToInt64 memory.Span
                    let metadata = (tableOfContents &&& (1L <<< 1)) <> 0L
                    let newObjectList = (tableOfContents &&& (1L <<< 2)) <> 0L
                    let rawData = (tableOfContents &&& (1L <<< 3)) <> 0L
                    let interleaved = (tableOfContents &&& (1L <<< 5)) <> 0L
                    let bigEndian = (tableOfContents &&& (1L <<< 6)) <> 0L
                    let daqMxRawData = (tableOfContents &&& (1L <<< 7)) <> 0L
                    memory <- memory.Slice 8
                    let remainingLengthMemory = memory.Slice(0, 8)

                    (*if bigEndian then
                        remainingLengthMemory.Span.Reverse()*)

                    let remainingLength =
                        BitConverter.ToUInt64(remainingLengthMemory.Span)

                    memory <- memory.Slice 8

                    let metadataLengthMemory = memory.Slice(0, 8)

                    (*if bigEndian then
                        metadataLengthMemory.Span.Reverse()*)

                    let metadataLength =
                        BitConverter.ToUInt64 metadataLengthMemory.Span

                    memory <- memory.Slice 8

                    if metadataLength > uint64 memory.Length then refill <- int metadataLength
                    //break

                    if metadata then ()
                    // readMetadata memory.Span index bigEndian interleaved

                    memory <- memory.Slice(int remainingLength)
                    filePosition <- filePosition + 28L + int64 remainingLength

                if refill > bufferLength then
                    arrayPool.Return buffer
                    buffer <- arrayPool.Rent refill
                    bufferLength <- buffer.Length


                fileStream.Seek(filePosition, SeekOrigin.Begin)
                |> ignore

                let! _ = fileStream.ReadAsync(buffer.AsMemory())
                refill <- 0

            arrayPool.Return buffer

            return index
        }

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
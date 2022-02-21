namespace FSharp.Data.Tdms

open System
open System.Buffers.Binary
open System.Numerics
#if IS_DESIGNTIME
open System.Runtime.InteropServices
#endif
open System.Text

module Buffer =

    let readInt16 (buffer: byte ReadOnlySpan byref) bigEndian =
        let value =
            if bigEndian then
                BinaryPrimitives.ReadInt16BigEndian buffer
            else
                BinaryPrimitives.ReadInt16LittleEndian buffer

        buffer <- buffer.Slice 2
        value

    let readInt (buffer: byte ReadOnlySpan byref) bigEndian =
        let value =
            if bigEndian then
                BinaryPrimitives.ReadInt32BigEndian buffer
            else
                BinaryPrimitives.ReadInt32LittleEndian buffer

        buffer <- buffer.Slice 4
        value

    let readInt64 (buffer: byte ReadOnlySpan byref) bigEndian =
        let value =
            if bigEndian then
                BinaryPrimitives.ReadInt64BigEndian buffer
            else
                BinaryPrimitives.ReadInt64LittleEndian buffer

        buffer <- buffer.Slice 8
        value

    let readUInt16 (buffer: byte ReadOnlySpan byref) bigEndian =
        let value =
            if bigEndian then
                BinaryPrimitives.ReadUInt16BigEndian buffer
            else
                BinaryPrimitives.ReadUInt16LittleEndian buffer

        buffer <- buffer.Slice 2
        value

    let readUInt (buffer: byte ReadOnlySpan byref) bigEndian =
        let value =
            if bigEndian then
                BinaryPrimitives.ReadUInt32BigEndian buffer
            else
                BinaryPrimitives.ReadUInt32LittleEndian buffer

        buffer <- buffer.Slice 4
        value

    let readUInt64 (buffer: byte ReadOnlySpan byref) bigEndian =
        let value =
            if bigEndian then
                BinaryPrimitives.ReadUInt64BigEndian buffer
            else
                BinaryPrimitives.ReadUInt64LittleEndian buffer

        buffer <- buffer.Slice 8
        value

    let readFloat32 (buffer: byte ReadOnlySpan byref) bigEndian =
        let value =
            if bigEndian then
                #if IS_DESIGNTIME
                if BitConverter.IsLittleEndian
                then
                   BitConverter.ToSingle(BitConverter.GetBytes(BinaryPrimitives.ReverseEndianness(MemoryMarshal.Read<int> buffer)), 0)
                else
                    MemoryMarshal.Read<float32> buffer
                #else
                BinaryPrimitives.ReadSingleBigEndian buffer
                #endif
            else
                #if IS_DESIGNTIME
                if not BitConverter.IsLittleEndian
                then
                   BitConverter.ToSingle(BitConverter.GetBytes(BinaryPrimitives.ReverseEndianness(MemoryMarshal.Read<int> buffer)), 0)
                else
                    MemoryMarshal.Read<float32> buffer
                #else
                BinaryPrimitives.ReadSingleLittleEndian buffer
                #endif

        buffer <- buffer.Slice 4
        value

    let readFloat (buffer: byte ReadOnlySpan byref) bigEndian =
        let value =
            if bigEndian then
                #if IS_DESIGNTIME
                if BitConverter.IsLittleEndian
                then
                   BitConverter.ToDouble(BitConverter.GetBytes(BinaryPrimitives.ReverseEndianness(MemoryMarshal.Read<int64> buffer)), 0)
                else
                    MemoryMarshal.Read<float> buffer
                #else
                BinaryPrimitives.ReadDoubleBigEndian buffer
                #endif
            else
                #if IS_DESIGNTIME
                if not BitConverter.IsLittleEndian
                then
                   BitConverter.ToDouble(BitConverter.GetBytes(BinaryPrimitives.ReverseEndianness(MemoryMarshal.Read<int64> buffer)), 0)
                else
                    MemoryMarshal.Read<float> buffer
                #else
                BinaryPrimitives.ReadDoubleLittleEndian buffer
                #endif

        buffer <- buffer.Slice 8
        value

    let readFloat80 (buffer: byte ReadOnlySpan byref) bigEndian =
        if bigEndian then
            { RawSignificand = readUInt64 &buffer bigEndian
              RawSignExponent = readUInt16 &buffer bigEndian }
        else
            { RawSignExponent = readUInt16 &buffer bigEndian
              RawSignificand = readUInt64 &buffer bigEndian }

    let readType (buffer: byte ReadOnlySpan byref) bigEndian =
        readUInt &buffer bigEndian
        |> function
        | 0u -> typeof<unit>
        | 0x21u -> typeof<bool>
        | 1u -> typeof<int8>
        | 2u -> typeof<int16>
        | 3u -> typeof<int>
        | 4u -> typeof<int64>
        | 5u -> typeof<uint8>
        | 6u -> typeof<uint16>
        | 7u -> typeof<uint>
        | 8u -> typeof<uint64>
        | 9u
        | 0x19u -> typeof<float32>
        | 10u
        | 0x1Au -> typeof<float>
        | 0x08000Cu -> typeof<struct (float32 * float32)>
        | 0x10000Du -> typeof<Complex>
        | 11u
        | 0x1Bu -> typeof<float80>
        | 0x20u -> typeof<string>
        | 0x44u -> typeof<Timestamp>
        | value -> failwithf "Unknown type: %i" value

    let readString (buffer: byte ReadOnlySpan byref) bigEndian =
        let length = readUInt &buffer bigEndian |> int
        let bytes = buffer.Slice(0, length)
        buffer <- buffer.Slice length
        #if IS_DESIGNTIME
        Encoding.UTF8.GetString (bytes.ToArray())
        #else
        Encoding.UTF8.GetString bytes
        #endif

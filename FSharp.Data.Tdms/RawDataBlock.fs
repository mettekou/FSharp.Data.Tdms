namespace FSharp.Data.Tdms

open System

type FormatChangingScaler =
    { DaqMxDataType: uint
      RawBufferIndex: uint
      RawByteOffsetWithinStride: uint
      SampleFormatBitmap: uint
      ScaleId: uint }

type InterleavedPrimitiveRawDataBlock =
    { Start: uint64
      Count: uint64
      mutable Skip: uint64 }

type PrimitiveRawDataBlock =
    | DecimatedPrimitiveRawDataBlock of (uint64 * uint64)
    | InterleavedPrimitiveRawDataBlock of InterleavedPrimitiveRawDataBlock

type RawDataBlocks =
    | PrimitiveRawDataBlocks of Type * PrimitiveRawDataBlock ResizeArray
    | StringRawDataBlocks of (uint64 * uint64 * uint64) ResizeArray

module RawDataBlock =

    let readFormatChangingScaler (buffer: byte ReadOnlySpan byref) bigEndian =
        { DaqMxDataType = Buffer.readUInt &buffer bigEndian
          RawBufferIndex = Buffer.readUInt &buffer bigEndian
          RawByteOffsetWithinStride = Buffer.readUInt &buffer bigEndian
          SampleFormatBitmap = Buffer.readUInt &buffer bigEndian
          ScaleId = Buffer.readUInt &buffer bigEndian }

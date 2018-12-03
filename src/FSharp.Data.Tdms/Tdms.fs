namespace FSharp.Data

module Tdms =
  open System
  open System.IO

  let readUInt32 (reader : BinaryReader) = reader.ReadUInt32()

  let readUInt64 (reader : BinaryReader) = reader.ReadUInt64()

  let readUInt32Big (reader : BinaryReader) = BitConverter.ToUInt32(reader.ReadBytes 4 |> Array.rev, 0)

  let readUInt64Big (reader : BinaryReader) = BitConverter.ToUInt64(reader.ReadBytes 8 |> Array.rev, 0)

  type Mappings = {
    UInt32 : BinaryReader -> uint32
    UInt64 : BinaryReader -> uint64
  }

  let littleEndianMappings = { UInt32 = readUInt32; UInt64 = readUInt64 }

  let bigEndianMappings = { UInt32 = readUInt32Big; UInt64 = readUInt64Big }

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
  
  type LeadIn = {
    TableOfContents : TableOfContents
    Version : Version
    NextSegmentOffset : uint64
    RawDataOffset : uint64
  }

  let readTdsm (reader : BinaryReader) =
    reader.ReadBytes 4 |> ignore

  let readTableOfContents (reader : BinaryReader) : TableOfContents =
    readUInt32 reader |> LanguagePrimitives.EnumOfValue
  
  let readVersion (reader : BinaryReader) mappings : Version = 
    mappings.UInt32 reader |> LanguagePrimitives.EnumOfValue

  let readLeadIn reader =
    readTdsm reader
    let tableOfContents = readTableOfContents reader
    let mappings = if tableOfContents.HasFlag TableOfContents.ContainsBigEndianData then bigEndianMappings else littleEndianMappings
    { TableOfContents = tableOfContents; Version = readVersion reader mappings; NextSegmentOffset = mappings.UInt64 reader; RawDataOffset = mappings.UInt64 reader }, mappings
  
  let readMetaData (reader : BinaryReader) mappings =
    let 
  
  let readRawData (reader : BinaryReader) mappings =
    ()
  
  let readSegment reader =
    let leadIn, mappings = readLeadIn reader
    let metaData = readMetaData reader mappings
    let rawData = readRawData reader mappings
    (leadIn, metaData, rawData)
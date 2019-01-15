namespace FSharp.Data.Tdms

type SegmentedIndex = {
    Path : string
    Segments : Segment list
}

module SegmentedIndex =
  
  open System.IO

  let readSegments (reader : BinaryReader) =
    List.unfold (fun s ->
      if reader.BaseStream.Position = reader.BaseStream.Length
      then None
      else
        let s' = Segment.read s reader
        Some (s', Some s')
    ) None
    
  let read (path : string) =
    use reader = new BinaryReader(File.OpenRead(path))
    { Path = path; Segments = readSegments reader }

  let write index =
    use writer = new BinaryWriter(File.OpenWrite(index.Path + "_index"))
    List.iter (Segment.writeIndex writer) index.Segments
  
  let amalgamate { Path = path; Segments = segments } =
    List.unfold (fun (todo, previousSegments) ->
      match todo with
         | [] -> None
         | next :: todo' ->
             let index = Segment.index previousSegments next
             Some (index, (todo', next :: previousSegments))) (segments, [])
    |> List.fold Index.merge { Path = path; Properties = Map.empty; Groups = Map.empty }
    

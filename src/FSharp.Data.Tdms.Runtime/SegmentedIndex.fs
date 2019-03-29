namespace FSharp.Data.Tdms

type SegmentedIndex = {
    Path : string
    Segments : Segment list
}

module SegmentedIndex =
  
  open System.IO
  open FSharp.Collections

  let readSegments fromIndex (reader : BinaryReader) =
    List.unfold (fun s ->
      if reader.BaseStream.Position = reader.BaseStream.Length
      then None
      else
        let s' = Segment.read fromIndex s reader
        Some (s', Some s')
    ) None
    
  let read fromIndex (path : string) =
    use reader = new BinaryReader(File.OpenRead(path))
    { Path = path; Segments = readSegments fromIndex reader }

  let write index =
    use writer = new BinaryWriter(File.OpenWrite(Path.ChangeExtension(index.Path, ".tdms_index")))
    List.iter (Segment.writeIndex writer) index.Segments
  
  let amalgamate { Path = path; Segments = segments } =
    List.unfold (fun (todo, previousObjects) ->
      match todo with
         | [] -> None
         | next :: todo' ->
             let index = Segment.index previousObjects next
             Some (index, (todo', List.fold (fun m o -> match o.RawDataIndex with | None -> m | Some _ -> Map.add o.Name o m) previousObjects next.Objects))) (segments, Map.empty)
    |> List.fold Index.merge { Path = path; Properties = Map.empty; Groups = Map.empty }
    

namespace FSharp.Data.Tdms

type Index = {
    Path : string
    Segments : Segment list
}

module Index =

  open System.IO

  let readSegments (reader : BinaryReader) =
    List.unfold (fun s ->
      if reader.BaseStream.Position = reader.BaseStream.Length
      then None
      else
        let s' = Segment.read s reader
        Some (s', Some s')
    ) None

  let write index =
    use writer = new BinaryWriter(File.OpenWrite(index.Path + "_index"))
    List.iter (Segment.writeIndex writer) index.Segments

  let read (path : string) =
    use reader = new BinaryReader(File.OpenRead(path))
    { Path = path; Segments = readSegments reader }
  
  let rawDataFor channel index =
    use reader = new BinaryReader(File.OpenRead(index.Path))
    List.unfold (fun (todo, previousSegments) ->
      match todo with
         | [] -> None
         | next :: todo ->
             let data = Segment.rawDataFor previousSegments channel reader next
             Some (data, (todo, next :: previousSegments))) (index.Segments, []) |> Array.concat
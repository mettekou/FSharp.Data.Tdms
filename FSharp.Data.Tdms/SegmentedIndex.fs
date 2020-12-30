namespace FSharp.Data.Tdms

open System.Buffers
open System.IO
open System.Text
open System.Text.Encodings
open FSharp.Collections

type SegmentedIndex = {
    Path : string
    Segments : Segment list
}

module SegmentedIndex =

  let readSegments fromIndex leadInBuffer (stream : Stream) indexStream =
    List.unfold (fun s ->
      if stream.Position = stream.Length
      then None
      else
        let s' = Segment.read fromIndex s leadInBuffer stream indexStream
        Some (s', Some s')
    ) None
    
  let read fromIndex writeIndex (path : string) (indexPath: string) =
    use stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, 4_096, false)
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
    let leadInBuffer = ArrayPool<byte>.Shared.Rent 28
    if writeIndex then
      use indexStream = new FileStream(indexPath, FileMode.Create, FileAccess.Write, FileShare.None, 4_096, false)
      let segments = readSegments fromIndex leadInBuffer stream indexStream
      ArrayPool<byte>.Shared.Return(leadInBuffer, false)
      { Path = path; Segments = segments }
    else
      let segments = readSegments fromIndex leadInBuffer stream null
      ArrayPool<byte>.Shared.Return(leadInBuffer, false)
      { Path = path; Segments = segments }
  
  let amalgamate { Path = path; Segments = segments } =
    List.unfold (fun (todo, previousObjects) ->
      match todo with
         | [] -> None
         | next :: todo' ->
             let index = Segment.index previousObjects next
             Some (index, (todo', List.fold (fun m o -> match o.RawDataIndex with | None | Some AsBefore -> m | Some _ -> Map.add o.Name o m) previousObjects next.Objects))) (segments, Map.empty)
    |> List.fold Index.merge { Path = path; Properties = Map.empty; Groups = Map.empty }
    

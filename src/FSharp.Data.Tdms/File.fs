namespace FSharp.Data.Tdms
  
module File =  

  let read path =
    let si = SegmentedIndex.read path
    //SegmentedIndex.write si
    SegmentedIndex.amalgamate si
    
  let rawData<'T> group channel index =
    Index.rawData<'T> group channel index
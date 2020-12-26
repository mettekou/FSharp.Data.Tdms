#if INTERACTIVE
  #load "../../paket-files/fsprojects/FSharp.TypeProviders.SDK/src/ProvidedTypes.fsi"
  #load "../../paket-files/fsprojects/FSharp.TypeProviders.SDK/src/ProvidedTypes.fs"
#endif

namespace FSharp.Data

// Put any runtime constructs here
// type DataSource(filename:string) = 
//     member this.FileName = filename

open FSharp.Data.Tdms

module Helpers =

  type RawDataHelper =
    
    static member RawData<'T> (group, channel, index) =
      File.tryRawData<'T> group channel index
      |> Option.defaultValue [||]
      
  let rawData ty group channel index =
    let generic = typeof<RawDataHelper>.GetMethod "RawData"
    let concrete = generic.MakeGenericMethod [| ty |]
    concrete.Invoke(null, [| group; channel; index |])


#if !IS_DESIGNTIME
// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("FSharp.Data.Tdms.dll")>]
do ()
#endif

namespace FSharp.Data

open Microsoft.FSharp.Core.CompilerServices
open System.Reflection

open ProviderImplementation.ProvidedTypes
open FSharp.Data.Tdms

[<TypeProvider>]
type TdmsProvider (config : TypeProviderConfig) as this =
  inherit TypeProviderForNamespaces (config)

  let ns = "FSharp.Data"
  let asm = Assembly.GetExecutingAssembly()

  let tdmsProvider = ProvidedTypeDefinition(asm, ns, "TdmsProvider", Some typeof<obj>)

  let parameters = [ProvidedStaticParameter("Sample", typeof<string>)]

  (*do tdmsProvider.DefineStaticParameters(parameters, fun typeName args ->
    let pathToTdms = args.[0] :?> string
    let provider = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, HideObjectMethods = true)
    let tdms = read pathToTdms
    Map.iter (fun name metaData ->
      let accessor = ProvidedProperty(name, typeof<obj>, <@@ fun [_] -> obj () @@>)
      provider.AddMember(accessor)
      Map.iter (fun name value ->

      ) metaData.Properties
    ) tdms
  )*)

  

[<assembly:TypeProviderAssembly>]
do ()

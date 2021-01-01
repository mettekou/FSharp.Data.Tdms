namespace FSharp.Data

open System.IO
open FSharp.Core.CompilerServices
open FSharp.Quotations
open System.Reflection
open FSharp.Data.Helpers

open ProviderImplementation.ProvidedTypes

open FSharp.Data.Tdms

[<TypeProvider>]
type TdmsProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("FSharp.Data.Tdms", "FSharp.Data.Tdms")],  addDefaultProbingLocation = true)

    let ns = "FSharp.Data"
    let execAsm = Assembly.GetExecutingAssembly()
    
    do execAsm.Location |> Path.GetDirectoryName |> this.RegisterProbingFolder

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    //do assert (typeof<FSharp.Data.DataSource>.Assembly.GetName().Name = execAsm.GetName().Name)  
    let staticParameters = [ProvidedStaticParameter("Sample", typeof<string>); ProvidedStaticParameter("WriteIndex", typeof<bool>)]
  
    let generatedType typeName path writeIndex =
      let file = File.read path writeIndex
      let asm = ProvidedAssembly()
      let root = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased = false, hideObjectMethods = true)
      let fileField = ProvidedField("_file", typeof<File>)
      let constructor = ProvidedConstructor([ProvidedParameter("path", typeof<string>); ProvidedParameter("writeIndex", typeof<bool>)], invokeCode = fun args -> Expr.FieldSet(args.[0], fileField, <@@ File.read (%%args.[1] : string) (%%args.[2] : bool) @@>))
      fileField.SetFieldAttributes(FieldAttributes.Private)
      root.AddMember(fileField)
      root.AddMember(constructor)
      let objects = Seq.cast<Object> file.Index.Objects
      Seq.tryFind (fun object -> object.Name = "/") objects
      |> Option.iter (fun { Properties = properties } ->
          for { Name = n; Value = p } in properties do
            ProvidedProperty(n, Type.system p.Type |> Option.defaultValue typeof<unit>, getterCode = fun args -> <@@ Index.unsafePropertyValue n (%%(Expr.FieldGet(args.[0], fileField)) : File).Index @@>) |> root.AddMember)
      for g in Seq.filter (fun object -> Array.length (object.Name.Split('/')) = 2) objects do
        let group = ProvidedTypeDefinition(asm, ns, g.Name, Some typeof<obj>, isErased = false, hideObjectMethods = true)
        root.AddMember(group)
        let groupField = ProvidedField("_group", typeof<Object>)
        let groupPathField = ProvidedField("_file", typeof<File>)
        let groupConstructor = ProvidedConstructor([ProvidedParameter("group", typeof<Object>); ProvidedParameter("file", typeof<File>)], invokeCode = fun args -> Expr.Sequential(Expr.FieldSet(args.[0], groupField, <@@ (%%args.[1] : Object) @@>), Expr.FieldSet(args.[0], groupPathField, <@@ (%%args.[2] : File) @@>)))
        groupField.SetFieldAttributes(FieldAttributes.Private)
        group.AddMember(groupField)
        group.AddMember(groupPathField)
        group.AddMember(groupConstructor)
        ProvidedProperty(propertyName = g.Name, propertyType = group.AsType(), getterCode = fun args -> Expr.NewObject(groupConstructor, [(<@@ Index.unsafeGroup g.Name (%%(Expr.FieldGet(args.[0], fileField)) : File).Index @@>); <@@ (%%(Expr.FieldGet(args.[0], fileField)) : File) @@>])) |> root.AddMember
        for { Name = n; Value = p } in g.Properties do
          ProvidedProperty(propertyName = n, propertyType = (Type.system p.Type |> Option.defaultValue typeof<unit>), getterCode = fun args -> <@@ Object.unsafePropertyValue n (%%(Expr.FieldGet(args.[0], groupField)) : Object) @@>) |> group.AddMember
        for c in Seq.filter (fun object -> Array.length (object.Name.Split('/')) = 3 && object.Name.Split('/').[1] = g.Name.Split('/').[1]) objects do
          let channel = ProvidedTypeDefinition(asm, ns, c.Name, Some typeof<obj>, isErased = false, hideObjectMethods = true)
          group.AddMember(channel)
          let channelField = ProvidedField("_channel", typeof<Object>)
          let channelPathField = ProvidedField("_file", typeof<File>)
          let channelConstructor = ProvidedConstructor([ProvidedParameter("channel", typeof<Object>); ProvidedParameter("file", typeof<File>)], invokeCode = fun args -> Expr.Sequential(Expr.FieldSet(args.[0], channelField, <@@ (%%args.[1] : Object) @@>), Expr.FieldSet(args.[0], channelPathField, <@@ (%%args.[2] : File) @@>)))
          channel.AddMember(channelField)
          channel.AddMember(channelPathField)
          channel.AddMember(channelConstructor)
          ProvidedProperty(propertyName = c.Name, propertyType = channel.AsType(), getterCode = fun args -> Expr.NewObject(channelConstructor, [(<@@ Index.unsafeChannel g.Name c.Name (%%(Expr.FieldGet(args.[0], fileField)) : File).Index @@>); (<@@ (%%(Expr.FieldGet(args.[0], groupPathField)) : File) @@>)])) |> group.AddMember
          for { Name = cpn; Value = cp } in c.Properties do
            ProvidedProperty(propertyName = cpn, propertyType = (Type.system cp.Type |> Option.defaultValue typeof<unit>), getterCode = fun args -> <@@ Object.unsafePropertyValue cpn (%%(Expr.FieldGet(args.[0], channelField)) : Object) @@>) |> channel.AddMember
          let ty = match c.LastRawDataIndex.Value with | String _ -> typeof<string> | OtherType (ty, _, _) -> Type.system ty |> Option.defaultValue typeof<obj>
          ProvidedProperty(propertyName = "Data", propertyType = ty.MakeArrayType(), getterCode = fun args -> <@@ rawData ty g.Name c.Name (%%(Expr.FieldGet(args.[0], channelPathField)) : File) @@>) |> channel.AddMember
      
      asm.AddTypes([root])
      root
  
    let providerType =
      let tdmsProvider = ProvidedTypeDefinition(execAsm, ns, "TdmsProvider", baseType = Some typeof<obj>, isErased = false)
      tdmsProvider.DefineStaticParameters(staticParameters, fun typeName args -> generatedType typeName (args.[0] :?> string) (args.[1] :?> bool))
      tdmsProvider
      
    do
      this.AddNamespace(ns, [providerType])

[<TypeProviderAssembly>]
do ()

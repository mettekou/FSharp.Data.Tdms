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
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("FSharp.Data.Tdms.DesignTime", "FSharp.Data.Tdms.Runtime")],  addDefaultProbingLocation = true)

    let ns = "FSharp.Data"
    let execAsm = Assembly.GetExecutingAssembly()
    
    do execAsm.Location |> Path.GetDirectoryName |> this.RegisterProbingFolder

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    //do assert (typeof<FSharp.Data.DataSource>.Assembly.GetName().Name = execAsm.GetName().Name)  
    let staticParameters = [ProvidedStaticParameter("Sample", typeof<string>); ProvidedStaticParameter("WriteIndex", typeof<bool>)]
  
    let generatedType typeName path writeIndex =
      let {Index = index} = File.read path writeIndex
      let asm = ProvidedAssembly()
      let root = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased = false, hideObjectMethods = true)
      let indexField = ProvidedField("_index", typeof<Index>)
      let constructor = ProvidedConstructor([ProvidedParameter("path", typeof<string>); ProvidedParameter("writeIndex", typeof<bool>)], invokeCode = fun args -> Expr.FieldSet(args.[0], indexField, <@@ File.read (%%args.[1] : string) (%%args.[2] : bool) @@>))
      indexField.SetFieldAttributes(FieldAttributes.Private)
      root.AddMember(indexField)
      root.AddMember(constructor)
      for (n, p) in Map.toList index.Properties do
        ProvidedProperty(n, Type.system p.Type |> Option.defaultValue typeof<unit>, getterCode = fun args -> <@@ Index.unsafePropertyValue n (%%(Expr.FieldGet(args.[0], indexField)) : Index) @@>) |> root.AddMember
      for (gn, g) in Map.toList index.Groups do
        let group = ProvidedTypeDefinition(asm, ns, gn, Some typeof<obj>, isErased = false, hideObjectMethods = true)
        root.AddMember(group)
        let groupField = ProvidedField("_group", typeof<Group>)
        let groupPathField = ProvidedField("_index", typeof<Index>)
        let groupConstructor = ProvidedConstructor([ProvidedParameter("group", typeof<Group>); ProvidedParameter("index", typeof<Index>)], invokeCode = fun args -> Expr.Sequential(Expr.FieldSet(args.[0], groupField, <@@ (%%args.[1] : Group) @@>), Expr.FieldSet(args.[0], groupPathField, <@@ (%%args.[2] : Index) @@>)))
        groupField.SetFieldAttributes(FieldAttributes.Private)
        group.AddMember(groupField)
        group.AddMember(groupPathField)
        group.AddMember(groupConstructor)
        ProvidedProperty(propertyName = gn, propertyType = group.AsType(), getterCode = fun args -> Expr.NewObject(groupConstructor, [(<@@ Index.unsafeGroup gn (%%(Expr.FieldGet(args.[0], indexField)) : Index) @@>); <@@ (%%(Expr.FieldGet(args.[0], indexField)) : Index) @@>])) |> root.AddMember
        for (n, p) in Map.toList g.Properties do
          ProvidedProperty(propertyName = n, propertyType = (Type.system p.Type |> Option.defaultValue typeof<unit>), getterCode = fun args -> <@@ Group.unsafePropertyValue n (%%(Expr.FieldGet(args.[0], groupField)) : Group) @@>) |> group.AddMember
        for (cn, c) in Map.toList g.Channels do
          let channel = ProvidedTypeDefinition(asm, ns, cn, Some typeof<obj>, isErased = false, hideObjectMethods = true)
          group.AddMember(channel)
          let channelField = ProvidedField("_channel", typeof<Channel>)
          let channelPathField = ProvidedField("_index", typeof<Index>)
          let channelConstructor = ProvidedConstructor([ProvidedParameter("channel", typeof<Channel>); ProvidedParameter("index", typeof<Index>)], invokeCode = fun args -> Expr.Sequential(Expr.FieldSet(args.[0], channelField, <@@ (%%args.[1] : Channel) @@>), Expr.FieldSet(args.[0], channelPathField, <@@ (%%args.[2] : Index) @@>)))
          channel.AddMember(channelField)
          channel.AddMember(channelPathField)
          channel.AddMember(channelConstructor)
          ProvidedProperty(propertyName = cn, propertyType = channel.AsType(), getterCode = fun args -> Expr.NewObject(channelConstructor, [(<@@ Group.unsafeChannel cn (%%(Expr.FieldGet(args.[0], groupField)) : Group) @@>); (<@@ (%%(Expr.FieldGet(args.[0], groupPathField)) : Index) @@>)])) |> group.AddMember
          for (cpn, cp) in Map.toList c.Properties do
            ProvidedProperty(propertyName = cpn, propertyType = (Type.system cp.Type |> Option.defaultValue typeof<unit>), getterCode = fun args -> <@@ Channel.unsafePropertyValue cpn (%%(Expr.FieldGet(args.[0], channelField)) : Channel) @@>) |> channel.AddMember
          ProvidedProperty(propertyName = "Data", propertyType = c.Type.MakeArrayType(), getterCode = let ty = c.Type in fun args -> <@@ rawData ty gn cn (%%(Expr.FieldGet(args.[0], channelPathField)) : Index) @@>) |> channel.AddMember
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

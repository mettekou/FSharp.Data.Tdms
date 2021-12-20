namespace FSharp.Data

open System
open System.IO
open FSharp.Core.CompilerServices
open FSharp.Quotations
open System.Reflection
open FSharp.Data.Helpers

open ProviderImplementation.ProvidedTypes

open FSharp.Data.Tdms

[<TypeProvider>]
type TdmsProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config,
                                      assemblyReplacementMap = [ ("FSharp.Data.Tdms.DesignTime", "FSharp.Data.Tdms") ])

    let ns = "FSharp.Data"
    let execAsm = Assembly.GetExecutingAssembly()

    do
        execAsm.Location
        |> Path.GetDirectoryName
        |> this.RegisterProbingFolder

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    //do assert (typeof<FSharp.Data.DataSource>.Assembly.GetName().Name = execAsm.GetName().Name)
    let staticParameters =
        [ ProvidedStaticParameter("Sample", typeof<string>)
          ProvidedStaticParameter("WriteIndex", typeof<bool>) ]

    let generatedType typeName path writeIndex =
        let file = File.read path writeIndex
        let asm = ProvidedAssembly()

        let root =
            ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased = false, hideObjectMethods = true)

        let fileField = ProvidedField("_file", typeof<File>)

        let constructor =
            ProvidedConstructor(
                [ ProvidedParameter("path", typeof<string>)
                  ProvidedParameter("writeIndex", typeof<bool>) ],
                invokeCode =
                    fun args ->
                        Expr.FieldSet(args.[0], fileField, <@@ File.read (%%args.[1]: string) (%%args.[2]: bool) @@>)
            )

        fileField.SetFieldAttributes(FieldAttributes.Private)
        root.AddMember(fileField)
        root.AddMember(constructor)
        let properties = file.Properties
        let groups = file.Groups

        for { Name = n; Type = pty } in properties do
            ProvidedProperty(
                n,
                pty,
                getterCode = fun args -> <@@ File.getPropertyValue n (%%(Expr.FieldGet(args.[0], fileField)): File) @@>
            )
            |> root.AddMember

        for g in groups do
            let gn = g.Name

            let group =
                ProvidedTypeDefinition(asm, ns, gn, Some typeof<obj>, isErased = false, hideObjectMethods = true)

            root.AddMember(group)
            let groupField = ProvidedField("_group", typeof<Group>)
            let groupPathField = ProvidedField("_file", typeof<File>)

            let groupConstructor =
                ProvidedConstructor(
                    [ ProvidedParameter("group", typeof<Group>)
                      ProvidedParameter("file", typeof<File>) ],
                    invokeCode =
                        fun args ->
                            Expr.Sequential(
                                Expr.FieldSet(args.[0], groupField, <@@ (%%args.[1]: Group) @@>),
                                Expr.FieldSet(args.[0], groupPathField, <@@ (%%args.[2]: File) @@>)
                            )
                )

            groupField.SetFieldAttributes(FieldAttributes.Private)
            group.AddMember(groupField)
            group.AddMember(groupPathField)
            group.AddMember(groupConstructor)

            ProvidedProperty(
                propertyName = gn,
                propertyType = group.AsType(),
                getterCode =
                    fun args ->
                        Expr.NewObject(
                            groupConstructor,
                            [ (<@@ File.findGroup gn (%%(Expr.FieldGet(args.[0], fileField)): File) @@>)
                              <@@ (%%(Expr.FieldGet(args.[0], fileField)): File) @@> ]
                        )
            )
            |> root.AddMember

            for { Name = n; Type = gpty } in g.Properties do
                ProvidedProperty(
                    propertyName = n,
                    propertyType = gpty,
                    getterCode =
                        fun args -> <@@ Group.getPropertyValue n (%%(Expr.FieldGet(args.[0], groupField)): Group) @@>
                )
                |> group.AddMember

                let channels = g.Channels

                for c in channels do
                    let cn = c.Name

                    let channel =
                        ProvidedTypeDefinition(
                            asm,
                            ns,
                            cn,
                            Some typeof<obj>,
                            isErased = false,
                            hideObjectMethods = true
                        )

                    group.AddMember(channel)

                    let channelField =
                        ProvidedField("_channel", typeof<Channel>)

                    let channelPathField = ProvidedField("_file", typeof<File>)

                    let channelConstructor =
                        ProvidedConstructor(
                            [ ProvidedParameter("channel", typeof<Channel>)
                              ProvidedParameter("file", typeof<File>) ],
                            invokeCode =
                                fun args ->
                                    Expr.Sequential(
                                        Expr.FieldSet(args.[0], channelField, <@@ (%%args.[1]: Channel) @@>),
                                        Expr.FieldSet(args.[0], channelPathField, <@@ (%%args.[2]: File) @@>)
                                    )
                        )

                    channel.AddMember(channelField)
                    channel.AddMember(channelPathField)
                    channel.AddMember(channelConstructor)

                    ProvidedProperty(
                        propertyName = cn,
                        propertyType = channel.AsType(),
                        getterCode =
                            fun args ->
                                Expr.NewObject(
                                    channelConstructor,
                                    [ (<@@ File.findChannel gn cn (%%(Expr.FieldGet(args.[0], groupPathField)): File) @@>)
                                      (<@@ (%%(Expr.FieldGet(args.[0], groupPathField)): File) @@>) ]
                                )
                    )
                    |> group.AddMember

                    for { Name = cpn; Type = cpty } in c.Properties do
                        ProvidedProperty(
                            propertyName = cpn,
                            propertyType = cpty,
                            getterCode =
                                fun args ->
                                    <@@ Channel.getPropertyValue
                                            cpn
                                            (%%(Expr.FieldGet(args.[0], channelField)): Channel) @@>
                        )
                        |> channel.AddMember

                    let ty' =
                        match c.RawDataBlocks with
                        | Some (StringRawDataBlocks _) -> Some typeof<string>
                        | Some (PrimitiveRawDataBlocks (ty', _)) -> Some ty'
                        | None -> None

                    Option.iter
                        (fun (ty: Type) ->
                            ProvidedProperty(
                                propertyName = "Data",
                                propertyType = ty.MakeArrayType(),
                                getterCode =
                                    fun args ->
                                        <@@ rawData ty gn cn (%%(Expr.FieldGet(args.[0], channelPathField)): File) @@>
                            )
                            |> channel.AddMember)
                        ty'

        asm.AddTypes([ root ])
        root

    let providerType =
        let tdmsProvider =
            ProvidedTypeDefinition(execAsm, ns, "TdmsProvider", baseType = Some typeof<obj>, isErased = false)

        tdmsProvider.DefineStaticParameters(
            staticParameters,
            fun typeName args -> generatedType typeName (args.[0] :?> string) (args.[1] :?> bool)
        )

        tdmsProvider

    do this.AddNamespace(ns, [ providerType ])

do ()

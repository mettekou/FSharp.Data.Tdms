namespace FSharp.Data.Tdms

open System
open System.Buffers
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open FSharp.Collections

type File =
    { Path: string
      Properties: Property seq
      Groups: FSharp.Data.Tdms.Group seq }

module File =

    [<Literal>]
    let LeadInLength = 28

    let ofObjects path objects =
        let groups =
            objects
            |> Seq.choose
                (fun ({ Name = groupName
                        Properties = properties }: FSharp.Data.Tdms.Object) ->
                    if Regex.IsMatch(groupName, @"^\/'[^\/']+'$") then
                        Some
                            { Name = groupName.Substring(2, String.length groupName - 3)
                              Properties = properties
                              Channels =
                                  objects
                                  |> Seq.filter
                                      (fun { Name = channelName } ->
                                          channelName.StartsWith groupName
                                          && String.length channelName > String.length groupName)
                                  |> Seq.map (Object.toChannel path) }
                    else
                        None)

        { Path = path
          Properties =
              objects
              |> Seq.tryFind (fun ({ Name = name }: FSharp.Data.Tdms.Object) -> name = "/")
              |> Option.map (fun { Properties = properties } -> properties)
              |> Option.toList
              |> Seq.concat
          Groups = groups }

    /// <summary>
    /// Opens a <see cref="File" />, reads the index from it, and closes it.
    /// </summary>
    /// <param name="path">the path to the <see cref="File" /> to read.</param>
    /// <param name="writeIndex">Whether to write the TDMS index file if it does not exist.</param>
    let read path writeIndex =
        let indexPath =
            Path.ChangeExtension(path, ".tdms_index")

        let indexExists = File.Exists indexPath

        use stream =
            new FileStream(
                (if indexExists then indexPath else path),
                FileMode.Open,
                FileAccess.Read,
                FileShare.Read,
                65_536,
                if indexExists then
                    FileOptions.SequentialScan
                else
                    FileOptions.None
            )

        use indexStream =
            if not indexExists && writeIndex then
                new FileStream(indexPath, FileMode.Create, FileAccess.Write, FileShare.None, 8_192, false)
            else
                null

        let mutable buffer = ArrayPool<byte>.Shared.Rent LeadInLength
        let objects = ResizeArray()
        let mutable offset = 0uL

        while stream.Position < stream.Length do
            stream.Read(buffer, 0, LeadInLength)
            |> ignore

            if not indexExists && writeIndex then
                indexStream.Write(Segment.tdsh, 0, 4)
                indexStream.Write(buffer, 4, 24)

            let mutable leadInSpan = ReadOnlySpan buffer

            let { TableOfContents = tableOfContents
                  NextSegmentOffset = nextSegmentOffset
                  RawDataOffset = rawDataOffset } =
                Segment.readLeadIn &leadInSpan

            let metaDataStart = offset + 28uL

            if tableOfContents.HasFlag(TableOfContents.ContainsMetaData) then
                let remainingLength = int rawDataOffset

                if remainingLength > buffer.Length then
                    ArrayPool<byte>.Shared.Return (buffer, false)
                    buffer <- ArrayPool<byte>.Shared.Rent remainingLength

                stream.Read(buffer, 0, remainingLength) |> ignore

                if writeIndex then
                    indexStream.Write(buffer, 0, remainingLength)

                let mutable metaDataSpan = ReadOnlySpan buffer

                Segment.readMetaData
                    objects
                    (metaDataStart + rawDataOffset)
                    (metaDataStart + nextSegmentOffset)
                    &metaDataSpan
                    (tableOfContents.HasFlag(TableOfContents.ContainsBigEndianData))
                    (tableOfContents.HasFlag(TableOfContents.ContainsInterleavedData))

            offset <- metaDataStart + nextSegmentOffset

            stream.Seek(int64 offset, SeekOrigin.Begin)
            |> ignore

        ArrayPool<byte>.Shared.Return (buffer, false)
        ofObjects path objects


    let readAsyncCt ct path writeIndex =
        backgroundTask {
            let indexPath =
                Path.ChangeExtension(path, ".tdms_index")

            let indexExists = File.Exists(indexPath)

            use stream =
                new FileStream(
                    (if indexExists then indexPath else path),
                    FileMode.Open,
                    FileAccess.Read,
                    FileShare.Read,
                    65_536,
                    if indexExists then
                        FileOptions.SequentialScan
                        ||| FileOptions.Asynchronous
                    else
                        FileOptions.Asynchronous
                )

            use indexStream =
                if not indexExists && writeIndex then
                    new FileStream(indexPath, FileMode.Create, FileAccess.Write, FileShare.None, 1_048_576, true)
                else
                    null

            let mutable buffer = ArrayPool<byte>.Shared.Rent LeadInLength
            let objects = ResizeArray()
            let mutable offset = 0uL

            while stream.Position < stream.Length do
                let! _ = stream.ReadAsync(buffer, 0, LeadInLength, ct)

                if not indexExists && writeIndex then
                    do! indexStream.WriteAsync(Segment.tdsh, 0, 4, ct)
                    do! indexStream.WriteAsync(buffer, 4, 24, ct)

                let mutable leadInSpan = ReadOnlySpan buffer

                let { TableOfContents = tableOfContents
                      NextSegmentOffset = nextSegmentOffset
                      RawDataOffset = rawDataOffset } =
                    Segment.readLeadIn &leadInSpan

                let metaDataStart = offset + 28uL

                if tableOfContents.HasFlag(TableOfContents.ContainsMetaData) then
                    let remainingLength = int rawDataOffset

                    if remainingLength > buffer.Length then
                        ArrayPool<byte>.Shared.Return (buffer, false)
                        buffer <- ArrayPool<byte>.Shared.Rent remainingLength

                    let! _ = stream.ReadAsync(buffer, 0, remainingLength, ct)

                    if writeIndex then
                        do! indexStream.WriteAsync(buffer, 0, remainingLength, ct)

                    let mutable metaDataSpan = ReadOnlySpan buffer

                    Segment.readMetaData
                        objects
                        (metaDataStart + rawDataOffset)
                        (metaDataStart + nextSegmentOffset)
                        &metaDataSpan
                        (tableOfContents.HasFlag(TableOfContents.ContainsBigEndianData))
                        (tableOfContents.HasFlag(TableOfContents.ContainsInterleavedData))

                offset <- metaDataStart + nextSegmentOffset

                stream.Seek(int64 offset, SeekOrigin.Begin)
                |> ignore

            ArrayPool<byte>.Shared.Return (buffer, false)
            return ofObjects path objects
        }

    /// <summary>
    /// Asynchronously opens a <see cref="File" />, reads the index from it, and closes it.
    /// </summary>
    /// <param name="path">the path to the <see cref="File" /> to read.</param>
    /// <param name="writeIndex">Whether to write the index file if it does not exist.</param>
    let readAsync = readAsyncCt CancellationToken.None

    let tryGetPropertyValue<'t> propertyName ({ Properties = properties }: File) =
        properties
        |> Seq.tryFind (fun { Name = propertyName' } -> propertyName' = propertyName)
        |> Option.bind Property.tryGet<'t>

    let getPropertyValue<'t> propertyName =
        tryGetPropertyValue<'t> propertyName >> Option.get

    /// <summary>Returns all groups within the <see cref="File" />.</summary>
    let getGroups { Groups = groups } = groups
    
    /// <summary>Returns the <see cref="Group" /> with the given name within the <see cref="File" />. Returns None if it does not exist.</summary>
    /// <param name="groupName">the name of the <see cref="Group" /> to find.</param>
    let tryFindGroup groupName =
        getGroups >> Seq.tryFind (fun { Name = groupName' } -> groupName' = groupName)

    /// <summary>Returns the <see cref="Group" /> with the given name within the <see cref="File" />.</summary>
    /// <param name="groupName">the name of the <see cref="Group" /> to find.</param>
    let findGroup groupName = tryFindGroup groupName >> Option.get
    
    /// <summary>Returns the <see cref="Channel" /> with the given name within the <see cref="Group" /> with the given name within the <see cref="File" />. Returns None if it does not exist.</summary>
    /// <param name="groupName">the name of the <see cref="Group" /> to find the <see cref="Channel" /> in.</param>
    /// <param name="channelName">the name of the <see cref="Channel" /> to find.</param>
    let tryFindChannel groupName channelName =
        tryFindGroup groupName
        >> Option.bind (Group.tryFindChannel channelName)

    /// <summary>Returns the <see cref="Channel" /> with the given name within the <see cref="Group" /> with the given name.</summary>
    /// <param name="groupName">the name of the <see cref="Group" /> to find the <see cref="Channel" /> in.</param>
    /// <param name="channelName">the name of the <see cref="Channel" /> to find.</param>
    let findChannel groupName channelName =
        tryFindChannel groupName channelName >> Option.get

    /// <summary>Returns the raw data for a <see cref="Channel" />. Returns None if the <see cref="Channel" /> does not exist, if it does not have any raw data, or if its raw data is not of the given type.</summary>
    /// <param name="groupName">the name of the <see cref="Group" /> the <see cref="Channel" /> is in.</param>
    /// <param name="channelName">the name of the <see cref="Channel" /> to get raw data for.</param>
    /// <param name="file">the TDMS file to read from.</param>
    let tryGetRawData<'t> groupName channelName file =
        tryFindChannel groupName channelName file
        |> Option.bind Channel.tryGetRawData<'t>

    /// <summary>Asynchronously returns the raw data for a <see cref="Channel" />. Returns None if the <see cref="Channel" /> does not exist, if it does not have any raw data, or if its raw data is not of the given type.</summary>
    /// <param name="groupName">the name of the <see cref="Group" /> the <see cref="Channel" /> is in.</param>
    /// <param name="channelName">the name of the <see cref="Channel" /> to get raw data for.</param>
    /// <param name="file">the TDMS file to read from.</param>
    let tryGetRawDataAsyncCt<'t> ct groupName channelName file =
        tryFindChannel groupName channelName file
        |> Option.map (Channel.tryGetRawDataAsyncCt<'t> ct)
        |> Option.defaultValue (Task.FromResult None)

    let tryGetRawDataAsync<'t> = tryGetRawDataAsyncCt<'t> CancellationToken.None

type File with

    /// <summary>
    /// Opens a TDMS file, reads the index from it, and closes it.
    /// </summary>
    /// <param name="path"> The path to the TDMS file to read.</param>
    /// <param name="writeIndex"> Whether to write the TDMS index file.</param>
    static member Read(path, writeIndex) = File.read path writeIndex

    /// <summary>
    /// Asynchronously opens a TDMS file, reads the index from it, and closes it.
    /// </summary>
    /// <param name="path"> The path to the TDMS file to read.</param>
    /// <param name="writeIndex"> Whether to write the TDMS index file.</param>
    static member ReadAsync(path, writeIndex, [<Optional; DefaultParameterValue(CancellationToken())>] ct) = File.readAsyncCt ct path writeIndex

    /// <summary>
    /// Tries to get the raw data for the given channel, belonging to the given group in the given TDMS file.
    /// </summary>
    member file.TryGetRawData<'T>(groupName, channelName, [<Out>] rawData: byref<'T []>) =
        match File.tryGetRawData<'T> groupName channelName file with
        | None -> false
        | Some rd ->
            rawData <- rd
            true

    /// <summary>
    /// Asynchronously gets the raw data for the given channel, belonging to the given group in the given TDMS file.
    /// </summary>
    member file.GetRawDataAsync<'t>(groupName, channelName, [<Optional; DefaultParameterValue(CancellationToken())>] ct) =
        backgroundTask {
            match! File.tryGetRawDataAsyncCt<'t> ct groupName channelName file with
            | None -> return null
            | Some rd -> return rd
        }

    /// <summary>
    /// Tries to get a property value for the given TDMS file.
    /// </summary>
    member file.TryGetPropertyValue<'T>(propertyName, [<Out>] propertyValue: byref<'T>) =
        match File.tryGetPropertyValue<'T> propertyName file with
        | None -> false
        | Some pv ->
            propertyValue <- pv
            true

    /// <summary>
    /// Tries to get a property value for the given group in the given TDMS file.
    /// </summary>
    member file.TryGetPropertyValue<'T>(propertyName, groupName, [<Out>] propertyValue: byref<'T>) =
        match File.tryFindGroup groupName file
              |> Option.bind (Group.tryGetPropertyValue<'T> propertyName) with
        | None -> false
        | Some pv ->
            propertyValue <- pv
            true

    /// <summary>
    /// Tries to get a property value for the given channel, belonging to the given group in the given TDMS file.
    /// </summary>
    member file.TryGetPropertyValue<'T>(propertyName, groupName, channelName, [<Out>] propertyValue: byref<'T>) =
        match File.tryGetPropertyValue<'T> ("/" + groupName + "/" + channelName) file with
        | None -> false
        | Some pv ->
            propertyValue <- pv
            true

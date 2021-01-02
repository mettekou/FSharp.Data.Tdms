namespace FSharp.Data.Tdms

open System.Runtime.InteropServices
open FSharp.Control.Tasks.NonAffine

type File = {
    Path: string
    Index : Index
}

module File =

    open System.IO

    /// <summary>
    /// Opens a TDMS file, reads the index from it, and closes it.
    /// </summary>
    /// <param name="path"> The path to the TDMS file to read.</param>
    let read path writeIndex =
        let indexPath = Path.ChangeExtension(path, ".tdms_index")
        let indexExists = File.Exists(indexPath)
        { Path = path; Index = Index.read indexExists (not indexExists && writeIndex) (if indexExists then indexPath else path) indexPath }
    
    let readAsync path writeIndex =
        let indexPath = Path.ChangeExtension(path, ".tdms_index")
        let indexExists = File.Exists(indexPath)
        task {
            let! index = Index.readAsync indexExists (not indexExists && writeIndex) (if indexExists then indexPath else path) indexPath
            return { Path = path; Index = index }
        }

    /// <summary>
    /// Tries to get the raw data for a <c>channel</c> within a <c>group</c>.
    /// </summary>
    let tryRawData<'T> group channel { Path = path; Index = index } =
        Index.tryRawData<'T> path group channel index

    let tryRawDataAsync<'t> group channel { Path = path; Index = index } =
        Index.tryRawDataAsync<'t> path group channel index

    /// <summary>
    /// Gets the value of a property associated with the file.
    /// </summary>
    let tryPropertyValue<'T> propertyName { Index = index } =
        Index.tryPropertyValue<'T> propertyName index
    
    let tryGroup groupName { Index = index } =
        Index.tryGroup groupName index

    let tryChannel groupName channelName { Index = index } =
        Index.tryChannel groupName channelName index

type File with

    /// <summary>
    /// Opens a TDMS file, reads the index from it, and closes it.
    /// </summary>
    /// <param name="path"> The path to the TDMS file to read.</param>
    /// <param name="writeIndex"> Whether to write the TDMS index file.</param>
    static member Read (path, writeIndex) = File.read path writeIndex

    /// <summary>
    /// Asynchronously opens a TDMS file, reads the index from it, and closes it.
    /// </summary>
    /// <param name="path"> The path to the TDMS file to read.</param>
    /// <param name="writeIndex"> Whether to write the TDMS index file.</param>
    static member ReadAsync (path, writeIndex) = File.readAsync path writeIndex

    /// <summary>
    /// Tries to get the raw data for the given channel, belonging to the given group in the given TDMS file.
    /// </summary>
    member file.TryGetRawData<'T> (groupName, channelName, [<Out>] rawData : byref<'T []>) =
        match File.tryRawData<'T> groupName channelName file with
            | None -> false
            | Some rd ->
                rawData <- rd
                true

    /// <summary>
    /// Asynchronously gets the raw data for the given channel, belonging to the given group in the given TDMS file.
    /// </summary>
    member file.GetRawDataAsync<'t> (groupName, channelName) =
        task {
            match! File.tryRawDataAsync<'t> groupName channelName file with
            | None -> return null
            | Some rd -> return rd
        }

    /// <summary>
    /// Tries to get a property value for the given TDMS file.
    /// </summary>
    member file.TryGetPropertyValue<'T> (propertyName, [<Out>] propertyValue : byref<'T>) =
        match File.tryPropertyValue<'T> propertyName file with
            | None -> false
            | Some pv ->
                propertyValue <- pv
                true

    /// <summary>
    /// Tries to get a property value for the given group in the given TDMS file.
    /// </summary>
    member file.TryGetPropertyValue<'T> (propertyName, groupName, [<Out>] propertyValue : byref<'T>) =
        match Index.tryGroup groupName file.Index |> Option.bind (Object.tryPropertyValue<'T> propertyName) with
            | None -> false
            | Some pv ->
                propertyValue <- pv
                true

    /// <summary>
    /// Tries to get a property value for the given channel, belonging to the given group in the given TDMS file.
    /// </summary>
    member file.TryGetPropertyValue<'T> (propertyName, groupName, channelName, [<Out>] propertyValue : byref<'T>) =
        match Index.tryPropertyValue<'T> ("/" + groupName + "/" + channelName) file.Index with
            | None -> false
            | Some pv ->
                propertyValue <- pv
                true
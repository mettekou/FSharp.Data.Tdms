namespace FSharp.Data.Tdms

type Group =
    { Name: string
      Properties: Property seq
      Channels: Channel seq }

module Group =

    let tryGetPropertyValue<'t> propertyName ({ Properties = properties }: Group) =
        properties
        |> Seq.tryFind (fun { Name = propertyName' } -> propertyName' = propertyName)
        |> Option.bind Property.tryGet<'t>

    let getPropertyValue<'t> propertyName =
        tryGetPropertyValue<'t> propertyName >> Option.get

    /// <summary>Finds the <see cref="Channel" /> with the given name within the <see cref="Group" />. Returns None if it does not exist.</summary>
    /// <param name="channelName">the name of the <see cref="Channel" /> to get.</param>
    let tryFindChannel channelName { Channels = channels } =
        channels
        |> Seq.tryFind (fun { Name = channelName' } -> channelName = channelName')

    /// <summary>Finds the <see cref="Channel" /> with the given name within the <see cref="Group" />.</summary>
    /// <param name="channelName">the name of the <see cref="Channel" /> to get.</param>
    let findChannel channelName =
        tryFindChannel channelName >> Option.get

    /// <summary>Returns all channels within the <see cref="Group" />.</summary>
    let getChannels { Channels = channels } = channels

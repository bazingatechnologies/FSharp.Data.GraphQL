#nowarn "40"

namespace FSharp.Data.GraphQL.Samples.GiraffeServer

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middlewares
open FSharp.Data.GraphQL.Server.Middlewares.AspNetCore

type Episode =
    | NewHope = 1
    | Empire = 2
    | Jedi = 3

type Human =
    { Id : string
      Name : string option
      Friends : string list
      AppearsIn : Episode list
      HomePlanet : string option }

type Droid =
    { Id : string
      Name : string option
      Friends : string list
      AppearsIn : Episode list
      PrimaryFunction : string option }

type Planet =
    { Id : string
      Name : string option
      mutable IsMoon : bool option }
    member x.SetMoon b =
        x.IsMoon <- b
        x

type Root =
    { ClientId: string }

type IThing =
    abstract member Shape : string
    abstract member Id : string

type Ball() =
    member __.Id = "1"
    member __.Shape = "Sphere"
    interface IThing with
        member this.Shape = this.Shape
        member this.Id = this.Id

type Box() =
    member __.Shape = "Cube"
    member __.Id = "2"
    interface IThing with
        member this.Shape = this.Shape
        member this.Id = this.Id

type Character =
    | Human of Human
    | Droid of Droid

type UploadedFile =
    { Content : string
      ContentType : string
      FileName : string }

module Schema =
    let humans =
        [ { Id = "1000"
            Name = Some "Luke Skywalker"
            Friends = [ "1002"; "1003"; "2000"; "2001" ]
            AppearsIn = [ Episode.NewHope; Episode.Empire; Episode.Jedi ]
            HomePlanet = Some "Tatooine" }
          { Id = "1001"
            Name = Some "Darth Vader"
            Friends = [ "1004" ]
            AppearsIn = [ Episode.NewHope; Episode.Empire; Episode.Jedi ]
            HomePlanet = Some "Tatooine" }
          { Id = "1002"
            Name = Some "Han Solo"
            Friends = [ "1000"; "1003"; "2001" ]
            AppearsIn = [ Episode.NewHope; Episode.Empire; Episode.Jedi ]
            HomePlanet = None }
          { Id = "1003"
            Name = Some "Leia Organa"
            Friends = [ "1000"; "1002"; "2000"; "2001" ]
            AppearsIn = [ Episode.NewHope; Episode.Empire; Episode.Jedi ]
            HomePlanet = Some "Alderaan" }
          { Id = "1004"
            Name = Some "Wilhuff Tarkin"
            Friends = [ "1001" ]
            AppearsIn = [ Episode.NewHope ]
            HomePlanet = None } ]

    let droids =
        [ { Id = "2000"
            Name = Some "C-3PO"
            Friends = [ "1000"; "1002"; "1003"; "2001" ]
            AppearsIn = [ Episode.NewHope; Episode.Empire; Episode.Jedi ]
            PrimaryFunction = Some "Protocol" }
          { Id = "2001"
            Name = Some "R2-D2"
            Friends = [ "1000"; "1002"; "1003" ]
            AppearsIn = [ Episode.NewHope; Episode.Empire; Episode.Jedi ]
            PrimaryFunction = Some "Astromech" } ]

    let planets =
        [ { Id = "1"
            Name = Some "Tatooine"
            IsMoon = Some false}
          { Id = "2"
            Name = Some "Endor"
            IsMoon = Some true}
          { Id = "3"
            Name = Some "Death Star"
            IsMoon = Some false}]

    let things : IThing list =
        [ Ball(); Box() ]

    let getHuman id =
        humans |> List.tryFind (fun h -> h.Id = id)

    let getDroid id =
        droids |> List.tryFind (fun d -> d.Id = id)

    let getPlanet id =
        planets |> List.tryFind (fun p -> p.Id = id)

    let characters =
        (humans |> List.map Human) @ (droids |> List.map Droid)

    let matchesId id = function
        | Human h -> h.Id = id
        | Droid d -> d.Id = id

    let getCharacter id =
        characters |> List.tryFind (matchesId id)

    let ThingType =
        Define.Interface(
            name = "Thing",
            description = "Gets the shape of the thing.",
            fieldsFn = fun () ->
            [
                Define.Field("format", String, "The format of the shape", fun _ (t : IThing) -> t.Shape)
                Define.Field("id", String, "The ID of the shape", fun _ (t : IThing) -> t.Id)
            ])

    let BallType =
        Define.Object<Ball>(
            name = "Ball",
            description = "A ball.",
            interfaces = [ ThingType ],
            isTypeOf = (fun o -> o :? Ball),
            fieldsFn = fun () ->
            [
                Define.Field("format", String, "The format of the ball", fun _ (b : Ball) -> b.Shape)
                Define.Field("id", String, "The ID of the ball", fun _ (t : Ball) -> t.Id)
            ]
        )

    let BoxType =
        Define.Object<Box>(
            name = "Box",
            description = "A box.",
            interfaces = [ ThingType ],
            isTypeOf = (fun o -> o :? Box),
            fieldsFn = fun () ->
            [
                Define.Field("format", String, "The format of the box", fun _ (t : Box) -> t.Shape)
                Define.Field("id", String, "The ID of the box", fun _ (t : Box) -> t.Id)
            ]
        )

    let UploadedFileType : ObjectDef<UploadedFile> =
        Define.Object<UploadedFile>(
            name = "UploadedFile",
            description = "A file upload operation.",
            isTypeOf = (fun o -> o :? UploadedFile),
            fieldsFn = fun () ->
            [
                Define.Field("fileName", String, "The name of the file.", fun _ u -> u.FileName)
                Define.Field("contentType", String, "The MIME type of the file.", fun _ u -> u.ContentType)
                Define.Field("content", String, "The content of the file as text.", fun _ u -> u.Content)
            ])

    let EpisodeType =
        Define.Enum(
            name = "Episode",
            description = "One of the films in the Star Wars Trilogy",
            options = [
                Define.EnumValue("NewHope", Episode.NewHope, "Released in 1977.")
                Define.EnumValue("Empire", Episode.Empire, "Released in 1980.")
                Define.EnumValue("Jedi", Episode.Jedi, "Released in 1983.") ])

    let rec CharacterType =
        Define.Union(
            name = "Character",
            description = "A character in the Star Wars Trilogy",
            options = [ HumanType; DroidType ],
            resolveValue = (fun o ->
                match o with
                | Human h -> box h
                | Droid d -> upcast d),
            resolveType = (fun o ->
                match o with
                | Human _ -> upcast HumanType
                | Droid _ -> upcast DroidType))

    and HumanType : ObjectDef<Human> =
        Define.Object<Human>(
            name = "Human",
            description = "A humanoid creature in the Star Wars universe.",
            isTypeOf = (fun o -> o :? Human),
            fieldsFn = fun () ->
            [
                Define.Field("id", String, "The id of the human.", fun _ (h : Human) -> h.Id)
                Define.Field("name", Nullable String, "The name of the human.", fun _ (h : Human) -> h.Name)
                Define.Field("friends", ListOf (Nullable CharacterType), "The friends of the human, or an empty list if they have none.",
                    fun _ (h : Human) -> h.Friends |> List.map getCharacter |> List.toSeq).WithQueryWeight(0.5)
                Define.Field("appearsIn", ListOf EpisodeType, "Which movies they appear in.", fun _ (h : Human) -> h.AppearsIn)
                Define.Field("homePlanet", Nullable String, "The home planet of the human, or null if unknown.", fun _ h -> h.HomePlanet)
            ])

    and DroidType =
        Define.Object<Droid>(
            name = "Droid",
            description = "A mechanical creature in the Star Wars universe.",
            isTypeOf = (fun o -> o :? Droid),
            fieldsFn = fun () ->
            [
                Define.Field("id", String, "The id of the droid.", fun _ (d : Droid) -> d.Id)
                Define.Field("name", Nullable String, "The name of the Droid.", fun _ (d : Droid) -> d.Name)
                Define.Field("friends", ListOf (Nullable CharacterType), "The friends of the Droid, or an empty list if they have none.",
                    fun _ (d : Droid) -> d.Friends |> List.map getCharacter |> List.toSeq).WithQueryWeight(0.5)
                Define.Field("appearsIn", ListOf EpisodeType, "Which movies they appear in.", fun _ d -> d.AppearsIn)
                Define.Field("primaryFunction", Nullable String, "The primary function of the droid.", fun _ d -> d.PrimaryFunction)
            ])

    and PlanetType =
        Define.Object<Planet>(
            name = "Planet",
            description = "A planet in the Star Wars universe.",
            isTypeOf = (fun o -> o :? Planet),
            fieldsFn = fun () ->
            [
                Define.Field("id", String, "The id of the planet", fun _ p -> p.Id)
                Define.Field("name", Nullable String, "The name of the planet.", fun _ p -> p.Name)
                Define.Field("ismoon", Nullable Boolean, "Is that a moon?", fun _ p -> p.IsMoon)
            ])

    and RootType =
        Define.Object<Root>(
            name = "Root",
            description = "The Root type to be passed to all our resolvers",
            isTypeOf = (fun o -> o :? Root),
            fieldsFn = fun () ->
            [
                Define.Field("clientid", String, "The ID of the client", fun _ r -> r.ClientId)
            ])

    let Query =
        Define.Object<Root>(
            name = "Query",
            fields = [
                Define.Field("hero", Nullable HumanType, "Gets human hero", [ Define.Input("id", String) ], fun ctx _ -> getHuman (ctx.Arg("id")))
                Define.Field("droid", Nullable DroidType, "Gets droid", [ Define.Input("id", String) ], fun ctx _ -> getDroid (ctx.Arg("id")))
                Define.Field("planet", Nullable PlanetType, "Gets planet", [ Define.Input("id", String) ], fun ctx _ -> getPlanet (ctx.Arg("id")))
                Define.Field("things", ListOf ThingType, "Gets things", fun _ _ -> things).WithQueryWeight(1.0) ])

    let Subscription =
        Define.SubscriptionObject<Root>(
            name = "Subscription",
            fields = [
                Define.SubscriptionField(
                    "watchMoon",
                    RootType,
                    PlanetType,
                    "Watches to see if a planet is a moon",
                    [ Define.Input("id", String) ],
                    (fun ctx _ p -> if ctx.Arg("id") = p.Id then Some p else None)) ])

    let schemaConfig =
        { SchemaConfig.Default with Types = [ BallType; BoxType ] }

    let Mutation =
        Define.Object<Root>(
            name = "Mutation",
            fields = [
                Define.Field(
                    "setMoon",
                    Nullable PlanetType,
                    "Sets a moon status",
                    [ Define.Input("id", String); Define.Input("ismoon", Boolean) ],
                    fun ctx _ ->
                        getPlanet (ctx.Arg("id"))
                        |> Option.map (fun x ->
                            x.SetMoon(Some(ctx.Arg("ismoon"))) |> ignore
                            schemaConfig.SubscriptionProvider.Publish<Planet> "watchMoon" x
                            schemaConfig.LiveFieldSubscriptionProvider.Publish<Planet> "Planet" "ismoon" x
                            x))
                Define.Field(
                    "uploadFile",
                    UploadedFileType,
                    "Uploads a file to the server and send its content back as text",
                    [ Define.Input("file", Upload) ],
                    fun ctx _ ->
                        let file = ctx.Arg<File>("file")
                        if file.Content.Position <> 0L then file.Content.Position <- 0L
                        use reader = new System.IO.StreamReader(file.Content)
                        { FileName = file.Name; ContentType = file.ContentType; Content = reader.ReadToEnd() }) ])

    let schema = Schema(Query, Mutation, Subscription, schemaConfig)

    let middlewares = 
        [ Define.QueryWeightMiddleware(2.0, true)
          Define.ObjectListFilterMiddleware<Human, Character option>(true)
          Define.ObjectListFilterMiddleware<Droid, Character option>(true)
          Define.LiveQueryMiddleware() ]

    let executor = Executor(schema, middlewares)

    let root = { ClientId = "5" }
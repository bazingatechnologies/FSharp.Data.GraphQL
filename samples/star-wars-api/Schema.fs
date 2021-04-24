namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middleware
open type FSharp.Data.GraphQL.Types.SchemaDefinitions.Define

#nowarn "40"

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
    { RequestId: string }

type Character =
    | Human of Human
    | Droid of Droid

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

    let EpisodeType =
        Enum(
            name = "Episode",
            description = "One of the films in the Star Wars Trilogy.",
            options = [
                EnumValue("NewHope", Episode.NewHope, "Released in 1977.")
                EnumValue("Empire", Episode.Empire, "Released in 1980.")
                EnumValue("Jedi", Episode.Jedi, "Released in 1983.") ])

    let rec CharacterType =
        Union(
            name = "Character",
            description = "A character in the Star Wars Trilogy.",
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
        Object<Human>(
            name = "Human",
            description = "A humanoid creature in the Star Wars universe.",
            isTypeOf = (fun o -> o :? Human),
            fieldsFn = fun () ->
            [
                Field("id", String, "The id of the human.", fun _ (h : Human) -> h.Id)
                Field("name", Nullable String, "The name of the human.", fun _ (h : Human) -> h.Name)
                Field("friends", SeqOf (Nullable CharacterType), "The friends of the human, or an empty list if they have none.",
                    fun _ (h : Human) -> h.Friends |> List.map getCharacter |> List.toSeq).WithQueryWeight(0.5)
                Field("appearsIn", ListOf EpisodeType, "Which movies they appear in.", fun _ (h : Human) -> h.AppearsIn)
                Field("homePlanet", Nullable String, "The home planet of the human, or null if unknown.", fun _ h -> h.HomePlanet)
            ])

    and DroidType =
        Object<Droid>(
            name = "Droid",
            description = "A mechanical creature in the Star Wars universe.",
            isTypeOf = (fun o -> o :? Droid),
            fieldsFn = fun () ->
            [
                Field("id", String, "The id of the droid.", fun _ (d : Droid) -> d.Id)
                Field("name", Nullable String, "The name of the Droid.", fun _ (d : Droid) -> d.Name)
                Field("friends", SeqOf (Nullable CharacterType), "The friends of the Droid, or an empty list if they have none.",
                    fun _ (d : Droid) -> d.Friends |> List.map getCharacter |> List.toSeq).WithQueryWeight(0.5)
                Field("appearsIn", ListOf EpisodeType, "Which movies they appear in.", fun _ d -> d.AppearsIn)
                Field("primaryFunction", Nullable String, "The primary function of the droid.", fun _ d -> d.PrimaryFunction)
            ])

    and PlanetType =
        Object<Planet>(
            name = "Planet",
            description = "A planet in the Star Wars universe.",
            isTypeOf = (fun o -> o :? Planet),
            fieldsFn = fun () ->
            [
                Field("id", String, "The id of the planet", fun _ p -> p.Id)
                Field("name", Nullable String, "The name of the planet.", fun _ p -> p.Name)
                Field("isMoon", Nullable Boolean, "Is that a moon?", fun _ p -> p.IsMoon)
            ])

    and RootType =
        Object<Root>(
            name = "Root",
            description = "The Root type to be passed to all our resolvers.",
            isTypeOf = (fun o -> o :? Root),
            fieldsFn = fun () ->
            [
                Field("requestId", String, "The ID of the client.", fun _ (r : Root) -> r.RequestId)
            ])

    let Query =
        Object<Root>(
            name = "Query",
            fields = [
                Field("hero", Nullable HumanType, "Gets human hero", [ Input("id", String) ], fun ctx _ -> getHuman (ctx.Arg("id")))
                Field("droid", Nullable DroidType, "Gets droid", [ Input("id", String) ], fun ctx _ -> getDroid (ctx.Arg("id")))
                Field("planet", Nullable PlanetType, "Gets planet", [ Input("id", String) ], fun ctx _ -> getPlanet (ctx.Arg("id")))
                Field("characters", ListOf CharacterType, "Gets characters", fun _ _ -> characters) ])

    let Subscription =
        SubscriptionObject<Root>(
            name = "Subscription",
            fields = [
                SubscriptionField(
                    "watchMoon",
                    RootType,
                    PlanetType,
                    "Watches to see if a planet is a moon.",
                    [ Input("id", String) ],
                    (fun ctx _ p -> if ctx.Arg("id") = p.Id then Some p else None)) ])

    let schemaConfig = SchemaConfig.Default

    let Mutation =
        Object<Root>(
            name = "Mutation",
            fields = [
                Field(
                    "setMoon",
                    Nullable PlanetType,
                    "Defines if a planet is actually a moon or not.",
                    [ Input("id", String); Input("isMoon", Boolean) ],
                    fun ctx _ ->
                        getPlanet (ctx.Arg("id"))
                        |> Option.map (fun x ->
                            x.SetMoon(Some(ctx.Arg("isMoon"))) |> ignore
                            schemaConfig.SubscriptionProvider.Publish<Planet> "watchMoon" x
                            schemaConfig.LiveFieldSubscriptionProvider.Publish<Planet> "Planet" "isMoon" x
                            x))])
    let middlewares =
        [ QueryWeightMiddleware(2.0, true)
          ObjectListFilterMiddleware<Human, Character option>(true)
          ObjectListFilterMiddleware<Droid, Character option>(true)
          LiveQueryMiddleware() ]
#r "../../packages/Suave/lib/net40/Suave.dll"
#r "../../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "../../packages/System.Runtime/lib/net462/System.Runtime.dll"
#r "../../bin/FSharp.Data.GraphQL.Server/FSharp.Data.GraphQL.Shared.dll"
#r "../../bin/FSharp.Data.GraphQL.Server/FSharp.Data.GraphQL.Server.dll"

open System

// Data
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

let getHuman id = humans |> List.tryFind (fun h -> h.Id = id)
let getDroid id = droids |> List.tryFind (fun d -> d.Id = id)

type Character =
    | Human of Human
    | Droid of Droid

let characters = (humans |> List.map Human) @ (droids |> List.map Droid)

let matchesId id = function
    | Human h -> h.Id = id
    | Droid d -> d.Id = id

let getCharacter id = characters |> List.tryFind (matchesId id)

// Schema definition
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

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
    fieldsFn = fun () -> [
        Define.Field("id", String, "The id of the human.", fun _ h -> h.Id)
        Define.Field("name", Nullable String, "The name of the human.", fun _ h -> h.Name)
        Define.Field("friends", ListOf (Nullable CharacterType), "The friends of the human, or an empty list if they have none.",
            fun _ h -> 
                h.Friends
                |> List.map getCharacter 
                |> List.toSeq)
        Define.Field("appearsIn", ListOf EpisodeType, "Which movies they appear in.", fun _ h -> h.AppearsIn)
        Define.Field("homePlanet", Nullable String, "The home planet of the human, or null if unknown.", fun _ h -> h.HomePlanet) ])
        
and DroidType =
  Define.Object<Droid>(
    name = "Droid",
    description = "A mechanical creature in the Star Wars universe.",
    isTypeOf = (fun o -> o :? Droid),
    fieldsFn = fun () -> [
        Define.Field("id", String, "The id of the droid.", fun _ d -> d.Id)
        Define.Field("name", Nullable String, "The name of the Droid.", fun _ d -> d.Name)
        Define.Field("friends", ListOf (Nullable CharacterType), "The friends of the Droid, or an empty list if they have none.", 
            fun ctx d -> d.Friends |> List.map getCharacter |> List.toSeq)
        Define.Field("appearsIn", ListOf EpisodeType, "Which movies they appear in.", fun _ d -> d.AppearsIn)
        Define.Field("primaryFunction", Nullable String, "The primary function of the droid.", fun _ d -> d.PrimaryFunction) ])

let Query =
  Define.Object(
    name = "Query",
    fields = [
        Define.Field("hero", Nullable HumanType, "Gets human hero", [ Define.Input("id", String) ], fun ctx () -> getHuman (ctx.Arg("id")))
        Define.Field("droid", Nullable DroidType, "Gets droid", [ Define.Input("id", String) ], fun ctx () -> getDroid (ctx.Arg("id"))) ])

let schema = Schema(Query)

// server initialization
open Suave
open Suave.Operators
open Newtonsoft.Json

type OptionConverter() =
    inherit JsonConverter()
    
    override x.CanConvert(t) = 
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

    override x.WriteJson(writer, value, serializer) =
        let value = 
            if value = null then null
            else 
                let _,fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, value.GetType())
                fields.[0]  
        serializer.Serialize(writer, value)

    override x.ReadJson(reader, t, existingValue, serializer) = failwith "Not supported"
    
let settings = JsonSerializerSettings()
settings.Converters <- [| OptionConverter() :> JsonConverter |]
settings.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
let json o = JsonConvert.SerializeObject(o, settings)
let ex = Executor(schema)
    
let tryParse fieldName data =
    let raw = Text.Encoding.UTF8.GetString data
    if raw <> null && raw <> ""
    then
        let map = JsonConvert.DeserializeObject<Map<string,string>>(raw)
        Map.tryFind fieldName map
    else None

let graphiql : WebPart =
    fun http ->
        async {
            match tryParse "query" http.request.rawForm with
            | Some query ->
                printfn "Received query: %s" query
                // at the moment parser is not parsing new lines correctly, so we need to get rid of them
                let q = query.Trim().Replace("\r\n", " ")
                let! result = ex.AsyncExecute(q)   
                return! http |> Successful.OK (json result)
            | None ->
                let! schemaResult = ex.AsyncExecute(Introspection.introspectionQuery)
                return! http |> Successful.OK (json schemaResult)
        }

let setCorsHeaders = 
    Writers.setHeader  "Access-Control-Allow-Origin" "*"
    >=> Writers.setHeader "Access-Control-Allow-Headers" "content-type"


startWebServer defaultConfig (setCorsHeaders >=> graphiql >=> Writers.setMimeType "application/json")
// Example:
// curl --form 'query={ hero(id: "1000") { id, name, appearsIn, friends { id,name } } }' http://localhost:8083/
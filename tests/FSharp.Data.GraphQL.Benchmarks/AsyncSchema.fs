namespace FSharp.Data.GraphQL.Benchmarks

#nowarn "40"

open System.Threading

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middleware

[<RequireQualifiedAccess>]
module AsyncSchemaDefinition =
  let humans =
      [ { Id = "1000"
          Name = Some "Luke Skywalker"
          Friends = [ "1002"; "1003" ]
          HomePlanet = Some "Tatooine" }
        { Id = "1001"
          Name = Some "Darth Vader"
          Friends = [ "1004" ]
          HomePlanet = Some "Tatooine" }
        { Id = "1002"
          Name = Some "Han Solo"
          Friends = [ "1000"; "1003" ]
          HomePlanet = None }
        { Id = "1003"
          Name = Some "Leia Organa"
          Friends = [ "1000"; "1002" ]
          HomePlanet = Some "Alderaan" }
        { Id = "1004"
          Name = Some "Wilhuff Tarkin"
          Friends = [ "1001" ]
          HomePlanet = None } ]

  let getPerson id = humans |> List.tryFind (fun h -> h.Id = id)

  let delay value = async {
          do Thread.Sleep(10)
          return value
      }

  let rec Person =
      Define.Object(name = "Person", isTypeOf = (fun o -> o :? Person),
                    fieldsFn = fun () ->
                        [ Define.AsyncField("id", String, resolve = fun _ person -> delay person.Id)
                          Define.AsyncField("name", Nullable String, resolve = fun _ person -> delay person.Name)
                          Define.AsyncField("friends", Nullable(SeqOf(Nullable Person)),
                                       resolve = fun _ person ->
                                           person.Friends
                                           |> List.map getPerson
                                           |> List.toSeq
                                           |> Some
                                           |> delay).WithQueryWeight(1.0)
                          Define.Field("homePlanet", String) ])

  let Query =
      Define.Object
          (name = "Query",
           fields = [ Define.Field
                          ("hero", Nullable Person, "Retrieves a person by provided id", [ Define.Input("id", String) ],
                           fun ctx () -> getPerson (ctx.Arg("id"))) ])


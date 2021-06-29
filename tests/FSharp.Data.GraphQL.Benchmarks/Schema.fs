namespace FSharp.Data.GraphQL.Benchmarks

#nowarn "40"

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middleware

type Person =
    { Id : string
      Name : string option
      Friends : string list
      HomePlanet : string option }

module SchemaDefinition =
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

  let rec Person =
      Define.Object(name = "Person", isTypeOf = (fun o -> o :? Person),
                    fieldsFn = fun () ->
                        [ Define.Field("id", String, resolve = fun _ person -> person.Id)
                          Define.Field("name", Nullable String, resolve = fun _ person -> person.Name)
                          Define.Field("friends", Nullable(SeqOf(Nullable Person)),
                                       resolve = fun _ person ->
                                           person.Friends
                                           |> List.map getPerson
                                           |> List.toSeq
                                           |> Some).WithQueryWeight(1.0)
                          Define.Field("homePlanet", String) ])

  let Query =
      Define.Object
          (name = "Query",
           fields = [ Define.Field
                          ("hero", Nullable Person, "Retrieves a person by provided id", [ Define.Input("id", String) ],
                           fun ctx () -> getPerson (ctx.Arg("id"))) ])

module QueryStrings =
    let simple = """{
        hero(id: "1000") {
            id
        }
    }"""
    let flat = """{
        hero(id: "1000") {
            id,
            name,
            homePlanet
        }
    }"""
    let nested = """{
        hero(id: "1000") {
            id,
            name,
            friends {
                id,
                name,
                friends {
                    id,
                    name,
                    friends {
                        id,
                        name
                    }
                }
            }
        }
    }"""
    let filtered = """{
        hero(id: "1000") {
            id,
            name,
            friends (filter : { id : "1002" }) {
                id,
                name
            }
        }
    }"""

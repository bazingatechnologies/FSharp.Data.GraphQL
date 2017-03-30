/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.Relay.ConnectionTests

#nowarn "25"

open System
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Relay
open FSharp.Data.GraphQL.Execution

type Pet =
    | Cat of name:string * barks:bool
    | Dog of name:string * meows:bool

type Human = { Name: string; Pets: Pet list }

let people = [
    { Name = "Abigail"; Pets = [] }
    { Name = "Tom"; Pets = [] }
    { Name = "Jane"; Pets = [] }
    { Name = "Chris"; Pets = [ Cat("Felix", true); Dog("Max", false) ] }
    { Name = "Jack"; Pets = [] }]

let humanName { Name = n; Pets = _ } = n

let inline toConnection cursor slice all =
    { Edges = 
        slice 
        |> List.map (fun s -> { Node = s; Cursor = cursor s}) 
        |> List.toSeq
      PageInfo = 
        { HasNextPage = slice.Tail <> (all |> List.tail)
          HasPreviousPage = slice.Head <> (all.Head)
          StartCursor = Some (cursor all.Head)
          EndCursor = Some (all |> List.last |> cursor) }
      TotalCount = Some (all.Length) }

let resolveSlice (cursor: 't -> string) (values: 't list) (SliceInfo slice) () : Connection<'t> =
    match slice with
    | Forward(first, after) -> 
        let idx = 
            match after with
            | Some a -> 1 + (values |> List.findIndex (fun x -> (cursor x) = a))
            | None -> 0
        let slice = 
            values 
            |> List.splitAt idx
            |> snd
            |> List.take first
        toConnection cursor slice values
    | Backward(last, before) ->
        let idx = 
            match before with
            | Some a -> values |> List.findIndexBack (fun x -> (cursor x) = a)
            | None -> values.Length
        let slice = 
            values 
            |> List.splitAt idx
            |> fst
            |> List.rev
            |> List.take last
            |> List.rev
        toConnection cursor slice values

let petName = function
    | Cat (name, _) -> name
    | Dog (name, _) -> name

let Cat =
  Define.Object(
   "Cat", [
    Define.Field("name", String, fun _ (Cat(name, _)) -> name)
    Define.Field("meows", Boolean, fun _ (Cat(_, meows)) -> meows) ])

let Dog =
  Define.Object(
   "Dog", [
    Define.Field("name", String, fun _ (Dog(name, _)) -> name)
    Define.Field("barks", Boolean, fun _ (Dog(_, barks)) -> barks) ])

let Pet = Define.Union("Pet", [ Dog; Cat ], id<Pet>, fun pet ->
    match pet with
    | Cat _ -> upcast Cat
    | Dog _ -> upcast Dog)

let Human =
  Define.Object(
   "Human", [
    Define.Field("name", String, fun _ human -> human.Name)
    Define.Field("pets",  ConnectionOf Pet, "", Connection.forwardArgs, fun ctx human -> resolveSlice petName (human.Pets) ctx ()) ])
    
let strings = ["one"; "two"; "three"; "four"; "five"]
let Query =
  Define.Object(
   "Query", [
    Define.Field(
        name = "strings", 
        description = "",
        typedef = ConnectionOf String, 
        args = Connection.allArgs, 
        resolve = resolveSlice id strings)
    Define.Field(
        name = "people", 
        description = "",
        typedef = ConnectionOf Human, 
        args = Connection.forwardArgs, 
        resolve = resolveSlice humanName people) ])

let schema = Schema(Query, config = { SchemaConfig.Default with Types = [ Pet; Human ]})

open Xunit

[<Fact>]
let ``Connection definition includes connection and edge fields for simple cases`` () =
    let query = """query QueryStrings {
        strings(last: 1) {
            edges {
                node
            }
        }
    }"""
    let result = sync <| Executor(schema).AsyncExecute(query)
    let expected =
      NameValueLookup.ofList [
        "strings", upcast NameValueLookup.ofList [
            "edges", upcast [
                box <| NameValueLookup.ofList [
                    "node", upcast "five" ]]]]
    noErrors result
    result.["data"] |> equals (upcast expected)

[<Fact>]
let ``Connection definition includes connection and edge fields for complex cases`` () =
    let query = """query QueryStrings {
        people(first: 1, after:"Jane") {
            edges {
                node {
                    name,
                    pets(first:2) {
                        edges {
                            node {
                                ... on Dog {
                                    name
                                    barks
                                }
                                ... on Cat {
                                    name
                                    meows
                                }
                            }
                        }
                    }
                }
            }
        }
    }"""
    let result = sync <| Executor(schema).AsyncExecute(query)
    let expected =
      NameValueLookup.ofList [
        "people", upcast NameValueLookup.ofList [
            "edges", upcast [
                box <| NameValueLookup.ofList [
                    "node", upcast NameValueLookup.ofList [
                        "name", upcast "Chris"
                        "pets", upcast NameValueLookup.ofList [
                            "edges", upcast [
                                box <| NameValueLookup.ofList [
                                    "node", upcast NameValueLookup.ofList [
                                        "name", upcast "Felix"
                                        "meows", upcast true]]
                                upcast NameValueLookup.ofList [
                                    "node", upcast NameValueLookup.ofList [
                                        "name", upcast "Max"
                                        "barks", upcast false]]]]]]]]]
    noErrors result
    result.["data"] |> equals (upcast expected)


[<Fact>]
let ``Connection doesn't allow to use List node type`` () = 
    throws<Exception>(fun () -> ConnectionOf (ListOf String) |> ignore)
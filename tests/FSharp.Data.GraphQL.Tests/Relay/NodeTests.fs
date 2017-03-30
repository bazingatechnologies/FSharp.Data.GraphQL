/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.Relay.NodeTests

#nowarn "40"

open System
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Relay
open FSharp.Data.GraphQL.Execution

type Person = { Id: string; Name: string; Age: int } 
type Car = { Id: string; Model: string; } 

let people = [
    { Id = "1"; Name = "Alice"; Age = 18 }
    { Id = "2"; Name = "Bob"; Age = 23 }
    { Id = "3"; Name = "Susan"; Age = 37 }]
    
let cars = [
    { Id = "1"; Model = "Tesla S" }
    { Id = "2"; Model = "Shelby GT500" }]

let rec Person =
  Define.Object<Person>(
    name = "Person",
    interfaces = [ Node ],
    fields = [
        Define.Field("id", ID, fun _ person -> toGlobalId "person" person.Id)
        Define.Field("name", String, fun _ person -> person.Name)
        Define.Field("age", Int, fun _ person -> person.Age) ])

and Car =
  Define.Object<Car>(
    name = "Car",
    interfaces = [ Node ],
    fields = [
        Define.Field("id", ID, fun _ car -> toGlobalId "car" car.Id)
        Define.Field("model", String, fun _ car -> car.Model) ])

and resolve _ _ id =
    match id with
    | GlobalId("person", id) -> people |> List.tryFind (fun person -> person.Id = id) |> Option.map box
    | GlobalId("car", id) -> cars |> List.tryFind (fun car -> car.Id = id) |> Option.map box
    | _ -> None

and Node = Define.Node (fun () -> [ Person; Car ])

let schema = Schema<unit>(Define.Object("Query", [ Define.NodeField(Node, resolve) ]), config = { SchemaConfig.Default with Types = [ Person; Car ] })

open Xunit

let execAndValidateNode (query: string) expected =
    let result = sync <| Executor(schema).AsyncExecute(query)
    noErrors result
    result.["data"] |> equals (upcast NameValueLookup.ofList ["node", upcast expected])
   
[<Fact>]
let ``Node with global ID gets correct record`` () =
    let query1 = """query ExampleQuery {
        node(id: "cGVyc29uOjE=") {
            name,
            age
        }
    }"""
    let expected1 =
      NameValueLookup.ofList [
        "name", upcast "Alice"
        "age", upcast 18]
    execAndValidateNode query1 expected1

    let query2 = """query ExampleQuery {
        node(id: "Y2FyOjE=") {
            model
        }
    }"""
    let expected2 =    
      NameValueLookup.ofList [
        "model", upcast "Tesla S" ]
    execAndValidateNode query1 expected1

[<Fact>]
let ``Node with global ID gets correct type`` () =
    execAndValidateNode """{ node(id: "cGVyc29uOjI=") { id, __typename } }""" (NameValueLookup.ofList ["id", upcast "cGVyc29uOjI="; "__typename", upcast "Person" ])
    execAndValidateNode """{ node(id: "Y2FyOjI=") { id, __typename } }""" (NameValueLookup.ofList ["id", upcast "Y2FyOjI="; "__typename", upcast "Car" ])
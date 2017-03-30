/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.UnionInterfaceTests

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type INamed =
    interface
        abstract Name : string
    end 

type Dog = 
    { Name: string; Barks: bool }
    interface INamed with
        member x.Name = x.Name

type Cat = 
    { Name: string; Meows: bool }
    interface INamed with
        member x.Name = x.Name
        
type Pet =
    | Cat of Cat
    | Dog of Dog

type Person = 
    { Name: string; Pets: Pet list; Friends: INamed list }
    interface INamed with
        member x.Name = x.Name
        
let NamedType =
  Define.Interface<INamed>(
    name = "Named",
    fields = [ Define.Field("name", String) ])

let DogType =
  Define.Object<Dog>(
    name = "Dog",
    isTypeOf = is<Dog>,
    interfaces = [ NamedType ],
    fields = [
        Define.AutoField("name", String)
        Define.AutoField("barks", Boolean)
    ])
    
let CatType =
  Define.Object<Cat>(
    name = "Cat",
    isTypeOf = is<Cat>,
    interfaces = [ NamedType ],
    fields = [
        Define.AutoField("name", String)
        Define.AutoField("meows", Boolean)
    ])

let PetType =
  Define.Union(
    name = "Pet",
    options = [ CatType; DogType ],
    resolveType = (fun pet ->
        match pet with
        | Cat _ -> upcast CatType
        | Dog _ -> upcast DogType),
    resolveValue = (fun pet ->
        match pet with
        | Cat cat -> box cat
        | Dog dog -> box dog))

let PersonType =
  Define.Object(
    name = "Person",
    isTypeOf = is<Person>,
    interfaces = [ NamedType ],
    fields = [
        Define.AutoField("name", String)
        Define.Field("pets", ListOf PetType, fun _ person -> person.Pets)
        Define.AutoField("friends", ListOf NamedType)
    ])

let schema = Schema(query = PersonType, config = { SchemaConfig.Default with Types = [ PetType ] })

let garfield = { Name = "Garfield"; Meows = false }
let odie = { Name = "Odie"; Barks = true }
let liz = { Name = "Liz"; Pets = []; Friends = [] }
let john = { Name = "John"; Pets = [ Cat garfield; Dog odie ]; Friends = [ liz; odie ] }

[<Fact>]
let ``Execute can introspect on union and intersection types`` () =
    let ast = parse """{
        Named: __type(name: "Named") {
          kind
          name
          fields { name }
          interfaces { name }
          possibleTypes { name }
          enumValues { name }
          inputFields { name }
        }
        Pet: __type(name: "Pet") {
          kind
          name
          fields { name }
          interfaces { name }
          possibleTypes { name }
          enumValues { name }
          inputFields { name }
        }
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected =
      NameValueLookup.ofList [
        "Named", upcast NameValueLookup.ofList [
            "kind", box "INTERFACE"
            "name", upcast "Named"
            "fields", upcast [
                box (NameValueLookup.ofList [ "name", box "name"])]
            "interfaces", null
            "possibleTypes", upcast [
                box (NameValueLookup.ofList ["name", box "Person"])
                upcast NameValueLookup.ofList ["name", box "Dog"]
                upcast NameValueLookup.ofList ["name", box "Cat"]]
            "enumValues", null
            "inputFields", null]
        "Pet", upcast NameValueLookup.ofList [
            "kind", box "UNION"
            "name", upcast "Pet"
            "fields", null
            "interfaces", null
            "possibleTypes", upcast [
                box (NameValueLookup.ofList ["name", box "Cat"])
                upcast NameValueLookup.ofList ["name", box "Dog"]]
            "enumValues", null
            "inputFields", null]]
    noErrors actual
    actual.["data"] |> equals (upcast expected)

[<Fact>]
let ``Executes union types`` () =
    // NOTE: This is an *invalid* query, but it should be an *executable* query.
    let ast = parse """{
        __typename
        name
        pets {
          __typename
          name
          barks
          meows
        }
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, john)
    let expected =
      NameValueLookup.ofList [
        "__typename", box "Person"
        "name", upcast "John"
        "pets", upcast [
            box <| NameValueLookup.ofList [
                "__typename", box "Cat"
                "name", upcast "Garfield"
                "meows", upcast false]
            upcast NameValueLookup.ofList [
                "__typename", box "Dog"
                "name", upcast "Odie"
                "barks", upcast true]]]
    noErrors actual
    actual.["data"] |> equals (upcast expected)
    
[<Fact>]
let ``Executes union types with inline fragments`` () =
    // This is the valid version of the query in the above test.
    let ast = parse """{
        __typename
        name
        pets {
          __typename
          ... on Dog {
            name
            barks
          }
          ... on Cat {
            name
            meows
          }
        }
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, john)
    let expected =
      NameValueLookup.ofList [
        "__typename", box "Person"
        "name", upcast "John"
        "pets", upcast [
            box <| NameValueLookup.ofList [
                "__typename", box "Cat"
                "name", upcast "Garfield"
                "meows", upcast false]
            upcast NameValueLookup.ofList [
                "__typename", box "Dog"
                "name", upcast "Odie"
                "barks", upcast true]]]
    noErrors actual
    actual.["data"] |> equals (upcast expected)
    
[<Fact>]
let ``Executes interface types`` () =
    // NOTE: This is an *invalid* query, but it should be an *executable* query.
    let ast = parse """{
        __typename
        name
        friends {
          __typename
          name
          barks
          meows
        }
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, john)
    let expected =
      NameValueLookup.ofList [
        "__typename", box "Person"
        "name", upcast "John"
        "friends", upcast [
            box <| NameValueLookup.ofList [
                "__typename", box "Person"
                "name", upcast "Liz" ]
            upcast NameValueLookup.ofList [
                "__typename", box "Dog"
                "name", upcast "Odie"
                "barks", upcast true ]]]
    noErrors actual
    actual.["data"] |> equals (upcast expected)
    
[<Fact>]
let ``Executes interface types with inline fragments`` () =
    // This is the valid version of the query in the above test.
    let ast = parse """{
        __typename
        name
        friends {
          __typename
          name
          ... on Dog {
            barks
          }
          ... on Cat {
            meows
          }
        }
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, john)
    let expected =
      NameValueLookup.ofList [
        "__typename", box "Person"
        "name", upcast "John"
        "friends", upcast [
            box <| NameValueLookup.ofList [
                "__typename", box "Person"
                "name", upcast "Liz"]
            upcast NameValueLookup.ofList [
                "__typename", box "Dog"
                "name", upcast "Odie"
                "barks", upcast true]]]
    noErrors actual
    actual.["data"] |> equals (upcast expected)

[<Fact>]
let ``Execute allows fragment conditions to be abstract types`` () =
    let ast = parse """{
        __typename
        name
        pets { ...PetFields }
        friends { ...FriendFields }
      }

      fragment PetFields on Pet {
        __typename
        ... on Dog {
          name
          barks
        }
        ... on Cat {
          name
          meows
        }
      }

      fragment FriendFields on Named {
        __typename
        name
        ... on Dog {
          barks
        }
        ... on Cat {
          meows
        }
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, john)
    let expected =
      NameValueLookup.ofList [
        "__typename", box "Person"
        "name", upcast "John"
        "pets", upcast [
            box <| NameValueLookup.ofList [
                "__typename", box "Cat"
                "name", upcast "Garfield"
                "meows", upcast false]
            upcast NameValueLookup.ofList [
                "__typename", box "Dog"
                "name", upcast "Odie"
                "barks", upcast true]]
        "friends", upcast [
            box <| NameValueLookup.ofList [
                "__typename", box "Person"
                "name", upcast "Liz"]
            upcast NameValueLookup.ofList [
                "__typename", box "Dog"
                "name", upcast "Odie"
                "barks", upcast true]]]
    noErrors actual
    actual.["data"] |> equals (upcast expected)
    
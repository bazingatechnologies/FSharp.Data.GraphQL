/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.PlanningTests

#nowarn "25"
#nowarn "40"

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Planning
open FSharp.Data.GraphQL.Execution

type Person = 
    { firstName : string
      lastName : string
      age : int }

type Animal = 
    { name : string
      species : string }

type Named = 
    | Animal of Animal
    | Person of Person

let people = 
    [ { firstName = "John"
        lastName = "Doe"
        age = 21 } ]

let animals = 
    [ { name = "Max"
        species = "Dog" } ]

let rec Person = 
    Define.Object(name = "Person", 
                  fieldsFn = (fun () -> 
                  [ Define.Field("firstName", String, fun _ person -> person.firstName)
                    Define.Field("lastName", String, fun _ person -> person.lastName)
                    Define.Field("age", Int, fun _ person -> person.age)
                    Define.Field("name", String, fun _ person -> person.firstName + " " + person.lastName)
                    Define.Field("friends", ListOf Person, fun _ _ -> []) ]), interfaces = [ INamed ])

and Animal = 
    Define.Object(name = "Animal", 
                  fields = [ Define.Field("name", String, fun _ animal -> animal.name)
                             Define.Field("species", String, fun _ animal -> animal.species) ], interfaces = [ INamed ])

and INamed = Define.Interface<obj>("INamed", [ Define.Field("name", String) ])

and UNamed = 
    Define.Union("UNamed", [ Person; Animal ], 
                 function 
                 | Animal a -> box a
                 | Person p -> upcast p)

[<Fact>]
let ``Planning should retain correct types for leafs``() = 
    let schema = Schema(Person)
    let schemaProcessor = Executor(schema)
    let query = """{
        firstName
        lastName
        age
    }"""
    let plan = schemaProcessor.CreateExecutionPlan(query)
    plan.RootDef |> equals (upcast Person)
    equals 3 plan.Fields.Length
    plan.Fields
    |> List.map (fun info -> (info.Identifier, info.ParentDef, info.ReturnDef))
    |> equals [ ("firstName", upcast Person, upcast String)
                ("lastName", upcast Person, upcast String)
                ("age", upcast Person, upcast Int) ]

[<Fact>]
let ``Planning should work with fragments``() = 
    let schema = Schema(Person)
    let schemaProcessor = Executor(schema)
    let query = """query Example {
        ...named
        age
    }
    fragment named on Person {
        firstName
        lastName
    }"""
    let plan = schemaProcessor.CreateExecutionPlan(query)
    plan.RootDef |> equals (upcast Person)
    equals 3 plan.Fields.Length
    plan.Fields
    |> List.map (fun info -> (info.Identifier, info.ParentDef, info.ReturnDef))
    |> equals [ ("firstName", upcast Person, upcast String)
                ("lastName", upcast Person, upcast String)
                ("age", upcast Person, upcast Int) ]

[<Fact>]
let ``Planning should work with parallel fragments``() = 
    let schema = Schema(Person)
    let schemaProcessor = Executor(schema)
    let query = """query Example {
        ...fnamed
        ...lnamed
        age
    }
    fragment fnamed on Person {
        firstName
    }
    fragment lnamed on Person {
        lastName
    }
    """
    let plan = schemaProcessor.CreateExecutionPlan(query)
    plan.RootDef |> equals (upcast Person)
    equals 3 plan.Fields.Length
    plan.Fields
    |> List.map (fun info -> (info.Identifier, info.ParentDef, info.ReturnDef))
    |> equals [ ("firstName", upcast Person, upcast String)
                ("lastName", upcast Person, upcast String)
                ("age", upcast Person, upcast Int) ]

[<Fact>]
let ``Planning should retain correct types for lists``() = 
    let Query = Define.Object("Query", [ Define.Field("people", ListOf Person, fun _ () -> people) ])
    let schema = Schema(Query)
    let schemaProcessor = Executor(schema)
    let query = """{
        people {
            firstName
            lastName
            friends
        }
    }"""
    let PersonList : ListOfDef<Person, Person list> = ListOf Person
    let plan = schemaProcessor.CreateExecutionPlan(query)
    equals 1 plan.Fields.Length
    let listInfo = plan.Fields.Head
    listInfo.Identifier |> equals "people"
    listInfo.ReturnDef |> equals (upcast PersonList)
    let (ResolveCollection(info)) = listInfo.Kind
    info.Identifier |> equals "people"
    info.ParentDef |> equals (upcast PersonList)
    info.ReturnDef |> equals (upcast Person)
    let (SelectFields(innerFields)) = info.Kind
    equals 3 innerFields.Length
    innerFields
    |> List.map (fun i -> (i.Identifier, i.ParentDef, i.ReturnDef))
    |> equals [ ("firstName", upcast Person, upcast String)
                ("lastName", upcast Person, upcast String)
                ("friends", upcast Person, upcast PersonList) ]
    let (ResolveCollection(friendInfo)) = (innerFields |> List.find (fun i -> i.Identifier = "friends")).Kind
    friendInfo.Identifier |> equals "friends"
    friendInfo.ParentDef |> equals (upcast PersonList)
    friendInfo.ReturnDef |> equals (upcast Person)

[<Fact>]
let ``Planning should work with interfaces``() = 
    let Query = Define.Object("Query", [ Define.Field("names", ListOf INamed, fun _ () -> []) ])
    let schema = Schema(query = Query, config = { SchemaConfig.Default with Types = [ Person; Animal ] })
    let schemaProcessor = Executor(schema)
    let query = """query Example {
        names {
            name
            ... on Animal {
                species
            }
            ...ageFragment
        }
    }
    fragment ageFragment on Person {
        age
    }"""
    let plan = schemaProcessor.CreateExecutionPlan(query)
    equals 1 plan.Fields.Length
    let INamedList : ListOfDef<obj, obj list> = ListOf INamed
    let listInfo = plan.Fields.Head
    listInfo.Identifier |> equals "names"
    listInfo.ReturnDef |> equals (upcast INamedList)
    let (ResolveCollection(info)) = listInfo.Kind
    info.Identifier |> equals "names"
    info.ParentDef |> equals (upcast INamedList)
    info.ReturnDef |> equals (upcast INamed)
    let (ResolveAbstraction(innerFields)) = info.Kind
    innerFields
    |> Map.map (fun typeName fields -> fields |> List.map (fun i -> (i.Identifier, i.ParentDef, i.ReturnDef)))
    |> equals (Map.ofList [ "Person", 
                            [ ("name", upcast INamed, upcast String)
                              ("age", upcast INamed, upcast Int) ]
                            "Animal", 
                            [ ("name", upcast INamed, upcast String)
                              ("species", upcast INamed, upcast String) ] ])

[<Fact>]
let ``Planning should work with unions``() = 
    let Query = Define.Object("Query", [ Define.Field("names", ListOf UNamed, fun _ () -> []) ])
    let schema = Schema(Query)
    let schemaProcessor = Executor(schema)
    let query = """query Example {
        names {
            ... on Animal {
                name
                species
            }
            ... on Person {
                name
                age
            }
        }
    }"""
    let plan = schemaProcessor.CreateExecutionPlan(query)
    equals 1 plan.Fields.Length
    let listInfo = plan.Fields.Head
    let UNamedList : ListOfDef<Named, Named list> = ListOf UNamed
    listInfo.Identifier |> equals "names"
    listInfo.ReturnDef |> equals (upcast UNamedList)
    let (ResolveCollection(info)) = listInfo.Kind
    info.Identifier |> equals "names"
    info.ParentDef |> equals (upcast UNamedList)
    info.ReturnDef |> equals (upcast UNamed)
    let (ResolveAbstraction(innerFields)) = info.Kind
    innerFields
    |> Map.map (fun typeName fields -> fields |> List.map (fun i -> (i.Identifier, i.ParentDef, i.ReturnDef)))
    |> equals (Map.ofList [ "Animal", 
                            [ ("name", upcast UNamed, upcast String)
                              ("species", upcast UNamed, upcast String) ]
                            "Person", 
                            [ ("name", upcast UNamed, upcast String)
                              ("age", upcast UNamed, upcast Int) ] ])

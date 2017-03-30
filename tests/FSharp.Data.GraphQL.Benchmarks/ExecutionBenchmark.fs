/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.ExecutionBenchmark

open System
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type Person = 
    { Id : string
      Name : string option
      Friends : string list
      HomePlanet : string option }

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
                        Define.Field("friends", Nullable(ListOf(Nullable Person)), 
                                     resolve = fun _ person -> 
                                         person.Friends
                                         |> List.map getPerson
                                         |> List.toSeq
                                         |> Some)
                        Define.Field("homePlanet", String) ])

let Query = 
    Define.Object
        (name = "Query", 
         fields = [ Define.Field
                        ("hero", Nullable Person, "Retrieves a person by provided id", [ Define.Input("id", String) ], 
                         fun ctx () -> getPerson (ctx.Arg("id"))) ])

open BenchmarkDotNet.Attributes

[<Config(typeof<GraphQLBenchConfig>)>]
type SimpleExecutionBenchmark() = 
    let simpleQueryString = """{ 
        hero(id: "1000") { 
            id
        } 
    }"""
    let flatQueryString = """{ 
        hero(id: "1000") { 
            id,
            name, 
            homePlanet
        } 
    }"""
    let nestedQueryString = """{ 
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

    let mutable schema : Schema<unit> = Unchecked.defaultof<Schema<unit>>
    let mutable schemaProcessor : Executor = Unchecked.defaultof<Executor>
    let mutable simpleAst : Ast.Document = Unchecked.defaultof<Ast.Document>
    let mutable flatAst : Ast.Document = Unchecked.defaultof<Ast.Document>
    let mutable nestedAst : Ast.Document = Unchecked.defaultof<Ast.Document>
    let mutable simpleExecutionPlan : ExecutionPlan = Unchecked.defaultof<ExecutionPlan>
    let mutable flatExecutionPlan : ExecutionPlan = Unchecked.defaultof<ExecutionPlan>
    let mutable nestedExecutionPlan : ExecutionPlan = Unchecked.defaultof<ExecutionPlan>
    
    [<Setup>]
    member x.Setup() = 
        schema <- Schema(Query)
        schemaProcessor <- Executor(schema)
        simpleAst <- parse simpleQueryString
        flatAst <- parse flatQueryString
        nestedAst <- parse nestedQueryString
        simpleExecutionPlan <- schemaProcessor.CreateExecutionPlan(simpleAst)
        flatExecutionPlan <- schemaProcessor.CreateExecutionPlan(flatAst)
        nestedExecutionPlan <- schemaProcessor.CreateExecutionPlan(nestedAst)
    
    [<Benchmark>]
    member x.BenchmarkSimpleQueryUnparsed() = schemaProcessor.AsyncExecute(simpleQueryString) |> Async.RunSynchronously
    
    [<Benchmark>]
    member x.BenchmarkSimpleQueryParsed() = schemaProcessor.AsyncExecute(simpleAst) |> Async.RunSynchronously
    
    [<Benchmark>]
    member x.BenchmarkSimpleQueryPlanned() = schemaProcessor.AsyncExecute(simpleExecutionPlan) |> Async.RunSynchronously
    
    [<Benchmark>]
    member x.BenchmarkFlatQueryUnparsed() = schemaProcessor.AsyncExecute(flatQueryString) |> Async.RunSynchronously
    
    [<Benchmark>]
    member x.BenchmarkFlatQueryParsed() = schemaProcessor.AsyncExecute(flatAst) |> Async.RunSynchronously
    
    [<Benchmark>]
    member x.BenchmarkFlatQueryPlanned() = schemaProcessor.AsyncExecute(flatExecutionPlan) |> Async.RunSynchronously
    
    [<Benchmark>]
    member x.BenchmarkNestedQueryUnparsed() = schemaProcessor.AsyncExecute(nestedQueryString) |> Async.RunSynchronously
    
    [<Benchmark>]
    member x.BenchmarkNestedQueryParsed() = schemaProcessor.AsyncExecute(nestedAst) |> Async.RunSynchronously
    
    [<Benchmark>]
    member x.BenchmarkNestedQueryPlanned() = schemaProcessor.AsyncExecute(nestedExecutionPlan) |> Async.RunSynchronously


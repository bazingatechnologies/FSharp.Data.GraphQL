/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.MiddlewareBenchmark
open BenchmarkDotNet.Jobs

#nowarn "40"

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open BenchmarkDotNet.Attributes
open FSharp.Data.GraphQL.Benchmarks
open FSharp.Data.GraphQL.Server.Middleware

[<Config(typeof<GraphQLBenchConfig>); MonoJob; SimpleJob(RuntimeMoniker.NetCoreApp21)>]
type SimpleExecutionWithMiddlewareBenchmark() = 
    let mutable schema : Schema<unit> = Unchecked.defaultof<Schema<unit>>
    let mutable middlewares : IExecutorMiddleware list = []
    let mutable schemaProcessor : Executor<unit> = Unchecked.defaultof<Executor<unit>>
    let mutable simpleAst : Ast.Document = Unchecked.defaultof<Ast.Document>
    let mutable flatAst : Ast.Document = Unchecked.defaultof<Ast.Document>
    let mutable nestedAst : Ast.Document = Unchecked.defaultof<Ast.Document>
    let mutable simpleExecutionPlan : ExecutionPlan = Unchecked.defaultof<ExecutionPlan>
    let mutable flatExecutionPlan : ExecutionPlan = Unchecked.defaultof<ExecutionPlan>
    let mutable nestedExecutionPlan : ExecutionPlan = Unchecked.defaultof<ExecutionPlan>
    let mutable filteredExecutionPlan : ExecutionPlan = Unchecked.defaultof<ExecutionPlan>
    let mutable filteredAst : Ast.Document = Unchecked.defaultof<Ast.Document>
    
    [<GlobalSetup>]
    member __.Setup() = 
        schema <- Schema(SchemaDefinition.Query)
        middlewares <- [ Define.QueryWeightMiddleware(20.0); Define.ObjectListFilterMiddleware<Person, Person option>() ]
        schemaProcessor <- Executor(schema, middlewares)
        simpleAst <- parse QueryStrings.simple
        flatAst <- parse QueryStrings.flat
        nestedAst <- parse QueryStrings.nested
        simpleExecutionPlan <- schemaProcessor.CreateExecutionPlan(simpleAst)
        flatExecutionPlan <- schemaProcessor.CreateExecutionPlan(flatAst)
        nestedExecutionPlan <- schemaProcessor.CreateExecutionPlan(nestedAst)
        filteredAst <- parse QueryStrings.filtered
        filteredExecutionPlan <- schemaProcessor.CreateExecutionPlan(filteredAst)
    
    [<Benchmark>]
    member __.BenchmarkSimpleQueryUnparsed() = schemaProcessor.AsyncExecute(QueryStrings.simple) |> Async.RunSynchronously
    
    [<Benchmark>]
    member __.BenchmarkSimpleQueryParsed() = schemaProcessor.AsyncExecute(simpleAst) |> Async.RunSynchronously
    
    [<Benchmark>]
    member __.BenchmarkSimpleQueryPlanned() = schemaProcessor.AsyncExecute(simpleExecutionPlan) |> Async.RunSynchronously
    
    [<Benchmark>]
    member __.BenchmarkFlatQueryUnparsed() = schemaProcessor.AsyncExecute(QueryStrings.flat) |> Async.RunSynchronously
    
    [<Benchmark>]
    member __.BenchmarkFlatQueryParsed() = schemaProcessor.AsyncExecute(flatAst) |> Async.RunSynchronously
    
    [<Benchmark>]
    member __.BenchmarkFlatQueryPlanned() = schemaProcessor.AsyncExecute(flatExecutionPlan) |> Async.RunSynchronously
    
    [<Benchmark>]
    member __.BenchmarkNestedQueryUnparsed() = schemaProcessor.AsyncExecute(QueryStrings.nested) |> Async.RunSynchronously
    
    [<Benchmark>]
    member __.BenchmarkNestedQueryParsed() = schemaProcessor.AsyncExecute(nestedAst) |> Async.RunSynchronously
    
    [<Benchmark>]
    member __.BenchmarkNestedQueryPlanned() = schemaProcessor.AsyncExecute(nestedExecutionPlan) |> Async.RunSynchronously

    [<Benchmark>]
    member __.BenchmarkFilteredQueryUnparsed() = schemaProcessor.AsyncExecute(QueryStrings.filtered) |> Async.RunSynchronously

    [<Benchmark>]
    member __.BenchmarkFilteredQueryParsed() = schemaProcessor.AsyncExecute(filteredAst) |> Async.RunSynchronously

    [<Benchmark>]
    member __.BenchmarkFilteredQueryPlanned() = schemaProcessor.AsyncExecute(filteredExecutionPlan) |> Async.RunSynchronously

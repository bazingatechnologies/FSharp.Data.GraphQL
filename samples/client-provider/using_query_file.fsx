#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/netstandard2.0/Microsoft.Extensions.Http.dll"
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"http://localhost:8086">

// If you pass a query file, it will load the query from it.
let operation = MyProvider.Operation<"operation.graphql">()

let result = operation.Run()

printfn "Data: %A\n" result.Data
printfn "Errors: %A\n" result.Errors
printfn "Custom data: %A\n" result.CustomData
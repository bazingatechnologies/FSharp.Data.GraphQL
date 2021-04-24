#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/netstandard2.0/Microsoft.Extensions.Http.dll"
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"http://localhost:8086">

let operation =
    MyProvider.Operation<"""mutation m {
      setMoon (id: "1", isMoon: true) {
        id
        name
        isMoon
      }
    }""">()

let result = operation.Run()

// Mutations are also supported.
// Subscriptions are not fully supported at the moment.
// Only direct responses will be received - we don't have a socket base connection.
printfn "Mutation result: %A\n" result.Data
printfn "Mutation errors: %A\n" result.Errors
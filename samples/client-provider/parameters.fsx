#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/netstandard2.0/Microsoft.Extensions.Http.dll"
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"http://localhost:8086">

let operation =
    MyProvider.Operation<"""query q($id: String!) {
      hero(id: $id) {
        name
        friends {
          ... on Human {
            id
            homePlanet
          }
          ... on Droid {
            id
            primaryFunction
          }
        }
      }
    }""">()

let result = operation.Run(id = "1000")

printfn "Result: %A" result.Data
printfn "Errors: %A" result.Errors
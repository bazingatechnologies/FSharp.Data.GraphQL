// Uncomment those to use build script client assembly
//#r "../../bin/FSharp.Data.GraphQL.Client/net47/FSharp.Data.GraphQL.Client.dll"
//#r "../../bin/FSharp.Data.GraphQL.Shared/net47/FSharp.Data.GraphQL.Shared.dll"

// Uncomment those to use build script client assembly using netstandard2.0
//#r "../../bin/FSharp.Data.GraphQL.Shared/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
//#r "../../bin/FSharp.Data.GraphQL.Client/netstandard2.0/netstandard.dll"
//#r "../../bin/FSharp.Data.GraphQL.Client/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

// Uncomment those to use dotnet build command for the client assembly
// #r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/net47/FSharp.Data.GraphQL.Shared.dll"
// #r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/net47/FSharp.Data.GraphQL.Client.dll"

//Uncomment those to use dotnet build command for the client assembly using netstandard2.0
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/netstandard.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/Microsoft.Extensions.Http.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"http://localhost:8084">

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

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

// If your server needs custom headers for the introspection query,
// you can provider it as an HTTP header file, or an HTTP header string.
// The format is similar to how headers are encoded in an HTTP request (one header per line, names and values separated by a comma).
// Those headers will go on to the query run methods as well. Unless you provide different headers when acutally calling Operation.Run.
// Other headers can be provided in the same format as here.
//type MyProvider = GraphQLProvider<"http://localhost:8084", "http_headers1.headerfile">
type MyProvider = GraphQLProvider<"http://localhost:8084", "UserData: 45883115-db2f-4ccc-ae6f-21ec17d4a7a1">

let operation =
    MyProvider.Operation<"""query q {
      hero (id: "1000") {
        name
        appearsIn
        homePlanet
        friends {
          ... on Human {
            name
            homePlanet
          }
          ... on Droid {
            name
            primaryFunction
          }
        }
      }
    }""">()

let result = operation.Run()

// If you need different user data from the introspection, you can provide here on the run method.
//let userData = Seq.ofArray [|"UserData", "45e7ca6f-4384-4da7-ad97-963133e6f0fb"|]
//let userData = HttpHeaders.ofFile "http_headers2.headerfile"
//let runtimeContext = MyProvider.GetContext(httpHeaders = userData)

//let result = operation.Run(runtimeContext)
//let result = operation.AsyncRun(runtimeContext) |> Async.RunSynchronously

printfn "Data: %A\n" result.Data
printfn "Errors: %A\n" result.Errors
printfn "Custom data: %A\n" result.CustomData
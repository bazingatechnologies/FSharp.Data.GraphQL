#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/netstandard2.0/Microsoft.Extensions.Http.dll"
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"swapi_schema.json">

let operation =
    MyProvider.Operation<"""{
      hero(id: "1000") {
        name
      }
    }""">()

let runtimeContext = MyProvider.GetContext(serverUrl = "http://localhost:8086")

let result = operation.Run(runtimeContext)

// Query result objects have pretty-printing and structural equality.
printfn "Data: %A\n" result.Data
printfn "Errors: %A\n" result.Errors
printfn "Custom data: %A\n" result.CustomData
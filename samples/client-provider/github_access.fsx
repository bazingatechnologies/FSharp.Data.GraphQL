#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/netstandard2.0/Microsoft.Extensions.Http.dll"
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL

// Some GraphQL API's gives access to their schema via GET method, whithout need to anthenticate via headers.
// The provider automatically tries to get the schema via GET method first. If it does not work,
// The classical way via POST is done.
let [<Literal>] Schema = __SOURCE_DIRECTORY__ + "/github_schema.json"
type MyProvider = GraphQLProvider<Schema>

let operation = MyProvider.Operation<"""query q { viewer { login } }""">()

let headers = HttpHeaders.ofFile (__SOURCE_DIRECTORY__ + "/github_authorization_headers.headerfile")

let run () =
    // Dispose runtime context after using it.
    use runtimeContext = MyProvider.GetContext(serverUrl = "https://api.github.com/graphql", httpHeaders = headers)
    let result = operation.Run(runtimeContext)
    printfn "Data: %A\n" result.Data

run ()
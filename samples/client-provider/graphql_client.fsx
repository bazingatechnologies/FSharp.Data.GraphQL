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

let run () =
      // Dispose the connection after using it.
      use connection = new GraphQLClientConnection()
      let request : GraphQLRequest =
          { Query = """query q { viewer { login } }"""
            Variables = [||]
            ServerUrl = "https://api.github.com/graphql"
            HttpHeaders =
                  [| "Authorization", "bearer [your bearer token here]"
                     "User-Agent", "[your github username here]" |]
            OperationName = Some "q" }
      let response = GraphQLClient.sendRequest connection request
      printfn "%s" response

run ()
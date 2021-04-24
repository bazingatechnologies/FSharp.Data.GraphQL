#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/netstandard2.0/Microsoft.Extensions.Http.dll"
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
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
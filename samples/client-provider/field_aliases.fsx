#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/netstandard2.0/Microsoft.Extensions.Http.dll"
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"swapi_schema.json">

let operation =
    MyProvider.Operation<"""query TestQuery {
      myHero: hero(id: "1000") {
        hisName: name
        appearsIn
        homePlanet
        hisFriends: friends {
          ... on Human {
            humanName: name
            appearsIn
            homePlanet
          }
          ... on Droid {
            droidName: name
            appearsIn
            primaryFunction
          }
        }
      }
    }""">()

let ctx = MyProvider.GetContext(serverUrl = "http://localhost:8086")

let result = operation.Run(ctx)

let hisName = result.Data.Value.MyHero.Value.HisName.Value

let hisFriends = result.Data.Value.MyHero.Value.HisFriends |> Array.choose id

let humanFriendNames =
  hisFriends |> Array.choose (fun f -> f.TryAsHuman()) |> Array.map (fun h -> h.HumanName)

let droidFriendNames =
  hisFriends |> Array.choose (fun f -> f.TryAsDroid()) |> Array.map (fun h -> h.DroidName)

printfn "His name: %s" hisName

printfn "Human friend names: %A" humanFriendNames

printfn "Droid friend names: %A" droidFriendNames

printfn "Data: %A" result.Data
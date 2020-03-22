module FSharp.Data.GraphQL.Tests.ResponseJsonTests

open System
open Xunit
open Helpers
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client

// Local provider should be able to be created from local introspection json file.
// This schema is adjusted to have some less common type names in order to test them.
type Provider = GraphQLProvider<"introspection-altered-types.json">


[<Fact>]
let ``Should be able to parse nullable ID``() =
    let op = Provider.Operation<"""query TestQuery {
      hero(id:"1000") {
          id,
          name,
          appearsIn,
          homePlanet,
          friends {
            ... on Human {
              name,
              id
            }
            ... on Droid {
              name,
              id
            }
          }
        }
      }""">
    let xop = op()
    let result1 = xop.ParseResult("""{
       "documentId": 2018203290,
       "data": {
         "hero": {
          "id": "1000",
          "__typename": "Human",
          "name": "Luke Skywalker",
          "appearsIn": [
            "NewHope",
            "Empire",
            "Jedi"
          ],
          "homePlanet": "Tatooine",
          "friends": [
            {
              "name": "Han Solo",
              "id": "1002",
              "__typename": "Human"
            },
            {
              "name": "Leia Organa",
              "id": "1003",
              "__typename": "Human"
            },
            {
              "name": "C-3PO",
              "id": "2000",
              "__typename": "Droid"
            },
            {
              "name": "R2-D2",
              "id": "2001",
              "__typename": "Droid"
            }
          ]
        }
       }
    }""")
    ()

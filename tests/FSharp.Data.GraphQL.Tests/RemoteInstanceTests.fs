module FSharp.Data.GraphQL.Tests.RemoteInstanceTests

open System
open System.Collections.Generic
open Xunit
open FsCheck
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Relay
open FSharp.Data.GraphQL.RemoteSchema
open Newtonsoft.Json

type IPet =
    interface
        abstract Name : string
        abstract Weight: int
        abstract Friends: string[]
    end 

type Dog = 
    { Name: string; Woofs: bool; Weight: int; Friends: string[] }
    interface IPet with
        member x.Name = x.Name
        member x.Weight = x.Weight
        member x.Friends = x.Friends

type Cat = 
    { Name: string; Meows: bool; Weight: int; Friends: string[] }
    interface IPet with
        member x.Name = x.Name
        member x.Weight = x.Weight
        member x.Friends = x.Friends

type Pet =
    | DogCase of Dog
    | CatCase of Cat


type Friend = 
    {
        Name: string
        Weight: int
    }

let FriendType =Define.Object<Friend>(
        name = "Friend",
        isTypeOf = is<Friend>,
        fields = [
            Define.Field("name", String, fun _ d -> d.Name)
            Define.Field("weight", Int, fun _ d -> d.Weight)
        ])

let remoteSchema =
    Schema<obj>(
        query = Define.Object("Query", fun () ->
        [
            Define.Field("friends", ListOf FriendType, (fun _ _ -> [{ Name = "friend 1"; Weight=10; } :> Friend ; upcast { Name = "friend 2"; Weight=20; } ]))
        ]), 
        config = { SchemaConfig.Default with Types = [FriendType] })


let rsExecutor = RequestSchemaExecutor()
rsExecutor.RegisterSchema("friends", remoteSchema)

let serializer rsd = JsonConvert.SerializeObject(rsd) :> obj
let transport (data : obj) : IDictionary<string, obj> =
    let unserializedData = JsonConvert.DeserializeObject<RemoteSchemaData>(data.ToString())
    rsExecutor.Execute(unserializedData)


[<Fact>]
let ``Execute subschema`` () = 
    let PetType = Define.Interface("Pet", fun () -> [ Define.Field("name", String); Define.Field("friends", Obj) ])
    let DogType =
      Define.Object<Dog>(
        name = "Dog", 
        isTypeOf = is<Dog>,
        interfaces = [ PetType ],
        fields = [
            Define.Field("name", String, fun _ d -> d.Name)
            Define.Field("woofs", Boolean, fun _ d -> d.Woofs)
            Define.Field("weight", Int, fun _ d -> d.Weight)
            Define.RemoteSchemaField("friends", serializer, transport)
        ])
    let CatType =
      Define.Object<Cat>(
        name = "Cat", 
        isTypeOf = is<Cat>,
        interfaces = [ PetType ],
        fields = [
            Define.Field("name", String, fun _ c -> c.Name)
            Define.Field("meows", Boolean, fun _ c -> c.Meows)
            Define.Field("weight", Int, fun _ d -> d.Weight)
            Define.Field("friends", Obj, fun _ d -> d.Friends :> obj)
        ])
    let schema =
      Schema(
        query = Define.Object("Query", fun () ->
        [
            Define.Field("pets", ListOf PetType, (fun _ _ -> [ { Name = "Odie"; Woofs = true; Weight=10; Friends=[|"bb"|] } :> IPet ; upcast { Name = "Garfield"; Meows = false; Weight=20; Friends=[||] } ]))
        ]), 
        config = { SchemaConfig.Default with Types = [CatType; DogType] })
    let schemaProcessor = Executor(schema)
    let query = """{
      pets {
        name
        ... on Dog {
          woofs
        }
        ... on Cat {
          meows
        }
        friends {
            name
            weight
        }
      }
    }"""
    let result = sync <| schemaProcessor.AsyncExecute(parse query)
    let expected =
      NameValueLookup.ofList [
          "pets", upcast [
            NameValueLookup.ofList [
                "name", "Odie" :> obj
                "woofs", upcast true
                "friends", upcast [
                    NameValueLookup.ofList [
                        "name", upcast "friend 1"
                        "weight", upcast 10
                    ]
                    NameValueLookup.ofList [
                        "name", upcast "friend 2"
                        "weight", upcast 20
                    ]
                        ] ] :> obj
            upcast NameValueLookup.ofList [
                "name", "Garfield" :> obj
                "meows", upcast false
                "friends", upcast []]]]
    noErrors result
    result.["data"] |> equals (upcast expected)

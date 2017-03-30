/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.AbstractionTests

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Relay

type IPet =
    interface
        abstract Name : string
    end 

type Dog = 
    { Name: string; Woofs: bool }
    interface IPet with
        member x.Name = x.Name

type Cat = 
    { Name: string; Meows: bool }
    interface IPet with
        member x.Name = x.Name

type Human = { Name: string; }

type Pet =
    | DogCase of Dog
    | CatCase of Cat

let resolvePet = function
    | DogCase d -> box d
    | CatCase c -> upcast c

[<Fact>]
let ``Execute handles execution of abstract types: isTypeOf is used to resolve runtime type for Interface`` () = 
    let PetType = Define.Interface("Pet", fun () -> [ Define.Field("name", String) ])
    let DogType =
      Define.Object<Dog>(
        name = "Dog", 
        isTypeOf = is<Dog>,
        interfaces = [ PetType ],
        fields = [
            Define.Field("name", String, fun _ d -> d.Name)
            Define.Field("woofs", Boolean, fun _ d -> d.Woofs)
        ])
    let CatType =
      Define.Object<Cat>(
        name = "Cat", 
        isTypeOf = is<Cat>,
        interfaces = [ PetType ],
        fields = [
            Define.Field("name", String, fun _ c -> c.Name)
            Define.Field("meows", Boolean, fun _ c -> c.Meows)
        ])
    let schema =
      Schema(
        query = Define.Object("Query", fun () ->
        [
            Define.Field("pets", ListOf PetType, fun _ _ -> [ { Name = "Odie"; Woofs = true } :> IPet ; upcast { Name = "Garfield"; Meows = false } ])
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
      }
    }"""
    let result = sync <| schemaProcessor.AsyncExecute(parse query)
    let expected =
      NameValueLookup.ofList [
          "pets", upcast [
            NameValueLookup.ofList [
                "name", "Odie" :> obj
                "woofs", upcast true ] :> obj
            upcast NameValueLookup.ofList [
                "name", "Garfield" :> obj
                "meows", upcast false]]]
    noErrors result
    result.["data"] |> equals (upcast expected)
    
[<Fact>]
let ``Execute handles execution of abstract types: isTypeOf is used to resolve runtime type for Union`` () = 
    let DogType =
      Define.Object<Dog>(
        name = "Dog", 
        isTypeOf = is<Dog>,
        fields = [
            Define.AutoField("name", String)
            Define.AutoField("woofs", Boolean)
        ])
    let CatType =
      Define.Object<Cat>(
        name = "Cat", 
        isTypeOf = is<Cat>,
        fields = [
            Define.AutoField("name", String)
            Define.AutoField("meows", Boolean)
        ])
    let PetType = Define.Union("Pet", [ DogType; CatType ], resolvePet)
    let schema =
      Schema(
        query = Define.Object("Query", fun () ->
        [
            Define.Field("pets", ListOf PetType, fun _ _ -> [ DogCase { Name = "Odie"; Woofs = true }; CatCase { Name = "Garfield"; Meows = false } ])
        ]))
    let schemaProcessor = Executor(schema)
    let query = """{
      pets {
        ... on Dog {
          name
          woofs
        }
        ... on Cat {
          name
          meows
        }
      }
    }"""
    let result = sync <| schemaProcessor.AsyncExecute(parse query)
    let expected =
      NameValueLookup.ofList [
        "pets", upcast [
            NameValueLookup.ofList [
                "name", "Odie" :> obj
                "woofs", upcast true ] :> obj
            upcast NameValueLookup.ofList [
                "name", "Garfield" :> obj
                "meows", upcast false]]]
    noErrors result
    result.["data"] |> equals (upcast expected)



type Widget = 
   { Id: string;
       Name: string }

type User = 
   { Id: string;
       Name: string;
       Widgets: Widget list }


[<Fact>]
let ``inner types `` () = 
   let viewer = {
       Id = "1"
       Name = "Anonymous"
       Widgets = [
           { Id = "1"; Name = "What's it"}
           { Id = "2"; Name = "Who's it"}
           { Id = "3"; Name = "How's it"} ]}

   let getUser id = if viewer.Id = id then Some viewer else None
   let getWidget id = viewer.Widgets |> List.tryFind (fun w -> w.Id = id)

   let rec Widget = Define.Object<Widget>(
       name = "Widget",
       description = "A shiny widget",
       interfaces = [ Node ],
       fields = [
           Define.GlobalIdField(fun _ w -> w.Id)
           Define.Field("name", String, fun _ w -> w.Name)
           ])

   //and WidgetConnection = ConnectionOf Widget
   and WidgetsField name (getUser: ResolveFieldContext -> 'a -> User) =
       let resolve ctx xx =
           let user = getUser ctx xx 
           let widgets = user.Widgets |> List.toArray
           Connection.ofArray widgets
    
       Define.Field(name, ConnectionOf Widget, "A person's collection of widgets", Connection.allArgs, resolve)


   and User = Define.Object<User>(
       name = "User",
       description = "A person who uses our app",
       interfaces = [ Node ],
       fields = [
           Define.GlobalIdField(fun _ w -> w.Id)
           Define.Field("name", String, fun _ w -> w.Name)
           WidgetsField "widgets" (fun _ user -> user)
       ])


   and Node = Define.Node<obj>(fun () -> [ User; Widget ])

   let Query =
       Define.Object("Query",
           [
           Define.NodeField(Node, fun ctx () id -> 
               match id with
               | GlobalId("User", i) -> getUser i |> Option.map box
               | GlobalId("Widget", i) -> getWidget i |> Option.map box
               | _ -> None)
           Define.Field("viewer", User, fun _ () -> viewer)

           // >>>>>>>>> uncomment the following line and notice that field won't be accessible
           WidgetsField "widgets" (fun _ () -> viewer)
       ])

   let schema = Schema(query = Query, config = { SchemaConfig.Default with Types = [ User; Widget ]})
   let schemaProcessor = Executor(schema)
    
   let query = "{
                   viewer {name}, widgets { edges }
                }"
   let q = query.Trim().Replace("\r\n", " ")

   let result = sync <| schemaProcessor.AsyncExecute(parse query)
    
   noErrors result  
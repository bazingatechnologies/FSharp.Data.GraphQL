/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.SchemaTests

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

[<Fact>]
let ``Object type should be able to merge fields with matching signatures from different interfaces`` () = 
    let MovableType = Define.Interface("Movable", [Define.Field("speed", Int)])
    let Movable2Type =
      Define.Interface(
          "Movable2", [
            Define.Field("speed", Int)
            Define.Field("acceleration", Int)
        ])
    let PersonType =
      Define.Object(
        name = "Person",
        interfaces = [ MovableType; Movable2Type ],
        fields = [
            Define.Field("name", String)
            Define.Field("speed", Int)
            Define.Field("acceleration", Int) ])
    equals [ MovableType :> InterfaceDef; upcast Movable2Type ] (PersonType.Implements |> Array.toList )
    let expected = 
        //NOTE: under normal conditions field order shouldn't matter in object definitions
        [ Define.Field("acceleration", Int) :> FieldDef
          upcast Define.Field("name", String) 
          upcast Define.Field("speed", Int)  ]
    equals expected (( PersonType :> ObjectDef).Fields |> Map.toList |> List.map snd)

[<Fact>]
let ``Schema config should be able to override default error handling`` () =
    let conf = { SchemaConfig.Default with ParseErrors = Array.mapi (fun idx _ -> string idx) }
    let TestType = 
        Define.Object<obj>("TestType", [
            Define.Field("passing", String, fun _ _ -> "ok")
            Define.Field("failing1", Nullable String, fun _ _ -> failwith "not ok" )
            Define.Field("failing2", Nullable String, fun _ _ -> failwith "not ok" ) ])
    let schema = Schema(Define.Object("Query", [ Define.Field("test", TestType, fun _ () -> obj())]), config = conf)
    let query = """
    {
        test {
            failing1
            passing
            failing2
        }
    }
    """
    let actual = sync <| Executor(schema).AsyncExecute query
    let expected = 
         NameValueLookup.ofList [
            "test", box <| NameValueLookup.ofList [
                "failing1", null
                "passing", box "ok"
                "failing2", null ]]
    actual.["data"] |> equals (upcast expected)
    let e = actual.["errors"] :?> string seq
    e |> Seq.toList |> equals ["0"; "1"]
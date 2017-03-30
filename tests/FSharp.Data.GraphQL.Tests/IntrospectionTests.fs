/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.IntrospectionTests

#nowarn "25"

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Types.Introspection

type IntrospectionResult = {
    __schema: IntrospectionSchema
}

type IntrospectionData = {
    Data:  IntrospectionResult
}


[<Fact(Skip="FIXME: investigate reason of failure")>]
let ``Introspection schema should be serializable back and forth using json`` () =
    let root = Define.Object("Query", [ Define.Field("onlyField", String) ])
    let schema = Schema(root)
    let introResult = Executor(schema).AsyncExecute(Introspection.introspectionQuery) |> sync
    let json = toJson introResult
    let deserialized:IntrospectionData = Helpers.fromJson json
    deserialized.Data.__schema |> equals (schema :> ISchema).Introspected

[<Fact>]
let ``Core type definitions are considered nullable`` () =
    let root = Define.Object("Query", [ Define.Field("onlyField", String) ])
    let schema = Schema(root)
    let query = """{ __type(name: "String") {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
          }
        }
      }
    } }"""
    let result = sync <| Executor(schema).AsyncExecute(query)
    let expected =
      NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "kind", upcast "SCALAR"
            "name", upcast "String"
            "ofType", null]]
    noErrors result
    result.["data"] |> equals (upcast expected)

type User = { FirstName: string; LastName: string }
type UserInput = { Name: string }

[<Fact>]
let ``Introspection works with query and mutation sharing same generic param`` =
    let User = Define.Object<User>("User", [
        Define.AutoField("firstName", String)
        Define.AutoField("lastName", String) ])
    let UserInput = Define.InputObject<UserInput>("UserInput", [
        Define.Input("name", String) ])
    let Query = Define.Object<User list>("Query", [
        Define.Field("users", ListOf User, fun _ u -> u) ])
    let Mutation = Define.Object<User list>("Mutation", [
        Define.Field("addUser", User, fun _ u -> u |> List.head)])
    let schema = Schema(Query, Mutation)
    let introResult = Executor(schema).AsyncExecute(Introspection.introspectionQuery) |> sync
    ()
    
[<Fact>]
let ``Default field type definitions are considered non-null`` () =
    let root = Define.Object("Query", [ Define.Field("onlyField", String) ])
    let schema = Schema(root)
    let query = """{ __type(name: "Query") {
      fields {
        name
        type {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
              }
            }
          }
        }
      }
    } }"""
    let result = sync <| Executor(schema).AsyncExecute(query)
    let expected =
      NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "fields", upcast [
                box <| NameValueLookup.ofList [
                    "name", upcast "onlyField"
                    "type", upcast NameValueLookup.ofList [
                        "kind", upcast "NON_NULL"
                        "name", null
                        "ofType", upcast NameValueLookup.ofList [
                            "kind", upcast "SCALAR"
                            "name", upcast "String"
                            "ofType", null]]]]]]
    noErrors result
    result.["data"] |> equals (upcast expected)
    
[<Fact>]
let ``Nullabe field type definitions are considered nullable`` () =
    let root = Define.Object("Query", [ Define.Field("onlyField", Nullable String) ])
    let schema = Schema(root)
    let query = """{ __type(name: "Query") {
      fields {
        name
        type {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
              }
            }
          }
        }
      }
    } }"""
    let result = sync <| Executor(schema).AsyncExecute(query)
    let expected =
      NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "fields", upcast [
                box <| NameValueLookup.ofList [
                    "name", upcast "onlyField"
                    "type", upcast NameValueLookup.ofList [
                        "kind", upcast "SCALAR"
                        "name", upcast "String"
                        "ofType", null]]]]]
    noErrors result
    result.["data"] |> equals (upcast expected) 
    
[<Fact>]
let ``Default field args type definitions are considered non-null`` () =
    let root = Define.Object("Query", [ Define.Field("onlyField", String, "", [ Define.Input("onlyArg", Int) ], fun _ () -> null) ])
    let schema = Schema(root)
    let query = """{ __type(name: "Query") {
      fields {
        args {
          name
          type {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                }
              }
            }
          }
        }
      }
    } }"""
    let result = sync <| Executor(schema).AsyncExecute(query)
    let expected =
      NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "fields", upcast [
                box <| NameValueLookup.ofList [
                    "args", upcast [
                        box <| NameValueLookup.ofList [
                            "name", upcast "onlyArg"
                            "type", upcast NameValueLookup.ofList [
                                "kind", upcast "NON_NULL"
                                "name", null
                                "ofType", upcast NameValueLookup.ofList [
                                    "kind", upcast "SCALAR"
                                    "name", upcast "Int"
                                    "ofType", null]]]]]]]]
    noErrors result
    result.["data"] |> equals (upcast expected) 
    
[<Fact>]
let ``Nullable field args type definitions are considered nullable`` () =
    let root = Define.Object("Query", [ Define.Field("onlyField", String, "", [ Define.Input("onlyArg", Nullable Int) ], fun _ () -> null) ])
    let schema = Schema(root)
    let query = """{ __type(name: "Query") {
      fields {
        args {
          name
          type {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                }
              }
            }
          }
        }
      }
    } }"""
    let result = sync <| Executor(schema).AsyncExecute(query)
    let expected =
      NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "fields", upcast [
                box <| NameValueLookup.ofList [
                    "args", upcast [
                        box <| NameValueLookup.ofList [
                            "name", upcast "onlyArg"
                            "type", upcast NameValueLookup.ofList [
                                "kind", upcast "SCALAR"
                                "name", upcast "Int"
                                "ofType", null ]]]]]]]
    noErrors result
    result.["data"] |> equals (upcast expected) 

[<Fact(Skip = "Investigate and fix the test, introspeciton is already working")>]
let ``Introspection executes an introspection query`` () =
    let root = Define.Object("QueryRoot", [ Define.Field("onlyField", String) ])
    let schema = Schema(root)
    let (Patterns.Object raw) = root
    let result = sync <| Executor(schema).AsyncExecute(parse Introspection.introspectionQuery, raw)
    noErrors result
    let expected =
      NameValueLookup.ofList [
        "__schema", upcast NameValueLookup.ofList [
            "mutationType", null
            "subscriptionType", null
            "queryType", upcast NameValueLookup.ofList [
                    "name", upcast "QueryRoot"]
            "types", upcast [
                    box <| NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "QueryRoot"
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Schema"
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "types"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "LIST"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "NON_NULL"
                                                              "name", null
                                                              "ofType", upcast NameValueLookup.ofList [
                                                                      "kind", upcast "OBJECT"
                                                                      "name", upcast "__Type"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "queryType"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "OBJECT"
                                                      "name", upcast "__Type"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "mutationType"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "OBJECT"
                                              "name", upcast "__Type"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "subscriptionType"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "OBJECT"
                                              "name", upcast "__Type"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "directives"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "LIST"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "NON_NULL"
                                                              "name", null
                                                              "ofType", upcast NameValueLookup.ofList [
                                                                      "kind", upcast "OBJECT"
                                                                      "name", upcast "__Directive"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Type"
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "kind"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "ENUM"
                                                      "name", upcast "__TypeKind"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "fields"
                                      "args", upcast [
                                              box <| NameValueLookup.ofList [
                                                   "name", upcast "includeDeprecated"
                                                   "type", upcast NameValueLookup.ofList [
                                                           "kind", upcast "SCALAR"
                                                           "name", upcast "Boolean"
                                                           "ofType", null]
                                                   "defaultValue", upcast "false"];]
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "LIST"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "NON_NULL"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "OBJECT"
                                                              "name", upcast "__Field"
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "interfaces"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "LIST"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "NON_NULL"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "OBJECT"
                                                              "name", upcast "__Type"
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "possibleTypes"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "LIST"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "NON_NULL"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "OBJECT"
                                                              "name", upcast "__Type"
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "enumValues"
                                      "args", upcast [
                                              box <| NameValueLookup.ofList [
                                                   "name", upcast "includeDeprecated"
                                                   "type", upcast NameValueLookup.ofList [
                                                           "kind", upcast "SCALAR"
                                                           "name", upcast "Boolean"
                                                           "ofType", null]
                                                   "defaultValue", upcast "false"];]
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "LIST"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "NON_NULL"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "OBJECT"
                                                              "name", upcast "__EnumValue"
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "inputFields"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "LIST"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "NON_NULL"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "OBJECT"
                                                              "name", upcast "__InputValue"
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "ofType"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "OBJECT"
                                              "name", upcast "__Type"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "ENUM"
                         "name", upcast "__TypeKind"
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "SCALAR"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "OBJECT"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "INTERFACE"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "UNION"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "ENUM"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "INPUT_OBJECT"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "LIST"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "NON_NULL"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "SCALAR"
                         "name", upcast "String"
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "SCALAR"
                         "name", upcast "Boolean"
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Field"
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "String"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "args"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "LIST"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "NON_NULL"
                                                              "name", null
                                                              "ofType", upcast NameValueLookup.ofList [
                                                                      "kind", upcast "OBJECT"
                                                                      "name", upcast "__InputValue"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "type"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "OBJECT"
                                                      "name", upcast "__Type"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "isDeprecated"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "deprecationReason"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__InputValue"
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "String"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "type"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "OBJECT"
                                                      "name", upcast "__Type"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "defaultValue"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__EnumValue"
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "String"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "isDeprecated"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "deprecationReason"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Directive"
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "String"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "locations"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "LIST"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "NON_NULL"
                                                              "name", null
                                                              "ofType", upcast NameValueLookup.ofList [
                                                                      "kind", upcast "ENUM"
                                                                      "name", upcast "__DirectiveLocation"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "args"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "LIST"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "NON_NULL"
                                                              "name", null
                                                              "ofType", upcast NameValueLookup.ofList [
                                                                      "kind", upcast "OBJECT"
                                                                      "name", upcast "__InputValue"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "onOperation"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast true
                                      "deprecationReason", upcast "Use `locations`."];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "onFragment"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast true
                                      "deprecationReason", upcast "Use `locations`."];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "onField"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast true
                                      "deprecationReason", upcast "Use `locations`."];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "ENUM"
                         "name", upcast "__DirectiveLocation"
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "QUERY"
                                      "isDeprecated", upcast false];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "MUTATION"
                                      "isDeprecated", upcast false];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "SUBSCRIPTION"
                                      "isDeprecated", upcast false];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "FIELD"
                                      "isDeprecated", upcast false];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "FRAGMENT_DEFINITION"
                                      "isDeprecated", upcast false];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "FRAGMENT_SPREAD"
                                      "isDeprecated", upcast false];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "INLINE_FRAGMENT"
                                      "isDeprecated", upcast false];]
                         "possibleTypes", null];]
            "directives", upcast [
                    box <| NameValueLookup.ofList [
                         "name", upcast "include"
                         "locations", upcast [
                                 box <| "FIELD";
                                 upcast "FRAGMENT_SPREAD";
                                 upcast "INLINE_FRAGMENT";]
                         "args", upcast [
                                 box <| NameValueLookup.ofList [
                                      "defaultValue", null
                                      "name", upcast "if"
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]];]];
                    upcast NameValueLookup.ofList [
                         "name", upcast "skip"
                         "locations", upcast [
                                 box <| "FIELD";
                                 upcast "FRAGMENT_SPREAD";
                                 upcast "INLINE_FRAGMENT";]
                         "args", upcast [
                                 box <| NameValueLookup.ofList [
                                      "defaultValue", null
                                      "name", upcast "if"
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]];]];]]]

    result.["data"]|> equals (upcast expected)
/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.VariablesTests

#nowarn "25"

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

let TestComplexScalar =
  Define.Scalar(
    name = "ComplexScalar",
    coerceInput = (fun (StringValue value) -> if value = "SerializedValue" then Some "DeserializedValue" else None),
    coerceValue = (fun value -> if value = upcast "DeserializedValue" then Some "SerializedValue" else None))

type TestInput = {
    a: string option
    b: string option list option
    c: string
    d: string option
}
let TestInputObject =
  Define.InputObject<TestInput>(
    name = "TestInputObject",
    fields = [
        Define.Input("a", Nullable String)
        Define.Input("b", Nullable(ListOf (Nullable String)))
        Define.Input("c", String)
        Define.Input("d", Nullable TestComplexScalar)
    ])

type TestNestedInput = {
    na: TestInput option
    nb: string
}
let TestNestedInputObject =
  Define.InputObject<TestNestedInput>(
    name = "TestNestedInputObject",
    fields = [
        Define.Input("na", Nullable TestInputObject)
        Define.Input("nb", String)
    ])

let stringifyArg name (ctx: ResolveFieldContext) () =
    let arg = ctx.TryArg name |> Option.toObj
    toJson arg

let stringifyInput = stringifyArg "input"

type EnumTestType = Foo | Bar

let EnumTestType =
  Define.Enum
    ("EnumTestType"
      , [ Define.EnumValue("Foo", EnumTestType.Foo)
          Define.EnumValue("Bar", EnumTestType.Bar)
        ]
      , "Test enum"
    )

let TestType =
  Define.Object<unit>(
    name = "TestType",
    fields = [
        Define.Field("fieldWithObjectInput", String, "", [ Define.Input("input", Nullable TestInputObject) ], stringifyInput)
        Define.Field("fieldWithNullableStringInput", String, "", [ Define.Input("input", Nullable String) ], stringifyInput)
        Define.Field("fieldWithNonNullableStringInput", String, "", [ Define.Input("input", String) ], stringifyInput)
        Define.Field("fieldWithDefaultArgumentValue", String, "", [ Define.Input("input", Nullable String, Some "hello world") ], stringifyInput)
        Define.Field("fieldWithNestedInputObject", String, "", [ Define.Input("input", TestNestedInputObject, { na = None; nb = "hello world"}) ], stringifyInput)
        Define.Field("fieldWithEnumInput", String, "", [ Define.Input("input", EnumTestType) ], stringifyInput)
        Define.Field("list", String, "", [ Define.Input("input", Nullable(SeqOf (Nullable String))) ], stringifyInput)
        Define.Field("nnList", String, "", [ Define.Input("input", ListOf (Nullable String)) ], stringifyInput)
        Define.Field("listNN", String, "", [ Define.Input("input", Nullable (SeqOf String)) ], stringifyInput)
        Define.Field("nnListNN", String, "", [ Define.Input("input", SeqOf String) ], stringifyInput)
    ])

let schema = Schema(TestType)

[<Fact>]
let ``Execute handles objects and nullability using inline structs with complex input`` () =
    let ast = parse """{ fieldWithObjectInput(input: {a: "foo", b: ["bar"], c: "baz"}) }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast """{"a":"foo","b":["bar"],"c":"baz","d":null}""" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact(Skip = "This test does not pass anymore since the literal representation of the argument does not match the input type.")>]
let ``Execute handles objects and nullability using inline structs and properly parses single value to list`` () =
    let ast = parse """{ fieldWithObjectInput(input: {a: "foo", b: "bar", c: "baz"}) }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast """{"a":"foo","b":["bar"],"c":"baz","d":null}""" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact(Skip = "This test can not pass anymore since validation does not allow inputs of incorrect type.")>]
let ``Execute handles objects and nullability using inline structs and doesn't use incorrect value`` () =
    let ast = parse """{ fieldWithObjectInput(input: ["foo", "bar", "baz"]) }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast "null" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles objects and nullability using inline structs and proprely coerces complex scalar types`` () =
    let ast = parse """{ fieldWithObjectInput(input: {c: "foo", d: "SerializedValue"}) }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast """{"a":null,"b":null,"c":"foo","d":"DeserializedValue"}"""]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables with complex inputs`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let params' : Map<string, obj> =
        Map.ofList ["input", upcast { a = Some "foo"; b = Some (upcast [ Some "bar"]) ; c = "baz"; d = None }]
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast """{"a":"foo","b":["bar"],"c":"baz","d":null}""" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables with default value when no value was provided`` () =
    let ast = parse """query q($input: TestInputObject = {a: "foo", b: ["bar"], c: "baz"}) {
            fieldWithObjectInput(input: $input)
          }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast """{"a":"foo","b":["bar"],"c":"baz","d":null}""" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and errors on null for nested non-nulls`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let params' : Map<string, obj> =
      Map.ofList [
        "input", upcast Map.ofList [
            "a", "foo" :> obj
            "b", upcast ["bar"]
            "c", null]]
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    match actual with
    | Direct(data, errors) ->
        hasError "Variable '$input': in input object 'TestInputObject': in field 'c': expected value of type String but got None" errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and errors on incorrect type`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let params' : Map<string, obj> = Map.ofList ["input", upcast "foo bar"]
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let errMsg = sprintf " Variable '$input': value of type %O is not assignable from %O" typeof<TestInput> typeof<string>
    match actual with
    | Direct(data, errors) ->
        hasError errMsg errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and errors on omission of nested non-nulls`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let params' : Map<string, obj> =
      Map.ofList [
        "input", upcast Map.ofList [
            "a", "foo" :> obj
            "b", upcast ["bar"]]]
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    match actual with
    | Direct(data, errors) ->
        List.length errors |> equals 1
        hasError "Variable '$input': in input object 'TestInputObject': in field 'c': expected value of type String but got None" errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be omitted`` () =
    let ast = parse """{ fieldWithNullableStringInput }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast "null" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be omitted in a variable`` () =
    let ast = parse """query SetsNullable($value: String) {
        fieldWithNullableStringInput(input: $value)
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast "null" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact(Skip = "This test does not pass anymore, since validation requires variables to be defined in the operation.")>]
let ``Execute handles variables and allows nullable inputs to be omitted in an unlisted variable`` () =
    let ast = parse """query SetsNullable {
        fieldWithNullableStringInput(input: $value)
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast "null" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be set to null in a variable`` () =
    let ast = parse """query SetsNullable($value: String) {
        fieldWithNullableStringInput(input: $value)
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "value", null ])
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast "null" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be set to a value in a variable`` () =
    let ast = parse """query SetsNullable($value: String) {
        fieldWithNullableStringInput(input: $value)
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "value", "a" :> obj ])
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast "\"a\"" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be set to a value directly`` () =
    let ast = parse """{ fieldWithNullableStringInput(input: "a") }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast "\"a\"" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles non-nullable scalars and does not allow non-nullable inputs to be omitted in a variable`` () =
    let ast = parse """query SetsNonNullable($value: String!) {
          fieldWithNonNullableStringInput(input: $value)
        }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "value", null ])
    match actual with
    | Direct(data, errors) ->
        hasError "Variable '$value': expected value of type String but got None" errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles non-nullable scalars and allows non-nullable inputs to be set to a value in a variable`` () =
    let ast = parse """query SetsNonNullable($value: String!) {
          fieldWithNonNullableStringInput(input: $value)
        }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "value", "a" :> obj ])
    let expected = NameValueLookup.ofList [ "fieldWithNonNullableStringInput", upcast "\"a\"" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles non-nullable scalars and allows non-nullable inputs to be set to a value directly`` () =
    let ast = parse """{ fieldWithNonNullableStringInput(input: "a") }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithNonNullableStringInput", upcast "\"a\"" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact(Skip = "This test can not pass anymore, since validation does not allow to omit non nullable arguments with no default value.")>]
let ``Execute handles non-nullable scalars and passes along null for non-nullable inputs if explcitly set in the query`` () =
    let ast = parse """{ fieldWithNonNullableStringInput }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithNonNullableStringInput", upcast "null" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists to be null`` () =
    let ast = parse """query q($input: [String]) {
          list(input: $input)
        }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "input", null ])
    let expected = NameValueLookup.ofList [ "list", upcast "null" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists to contain values`` () =
    let ast = parse """query q($input: [String]) {
          list(input: $input)
        }"""
    let variables =  Map.ofList [ "input", box [ "A" ] ]
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = variables)
    let expected = NameValueLookup.ofList [ "list", upcast "[\"A\"]" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists to contain null`` () =
    let ast = parse """query q($input: [String]) {
          list(input: $input)
        }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "input", [ "A":> obj; null; "B" :> obj ] :> obj ])
    let expected = NameValueLookup.ofList [ "list", upcast "[\"A\",null,\"B\"]" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists to be null`` () =
    let ast = parse """query q($input: [String]!) {
          nnList(input: $input)
        }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "input", null ])
    match actual with
    | Direct(data, errors) ->
        hasError "Variable '$input': expected value of type [String]!, but no value was found" errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows non-null lists to contain values`` () =
    let ast = parse """query q($input: [String]!) {
          nnList(input: $input)
        }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "input", [ "A" ] :> obj ])
    let expected = NameValueLookup.ofList [ "nnList", upcast "[\"A\"]" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows non-null lists to contain null`` () =
    let ast = parse """query q($input: [String]!) {
          nnList(input: $input)
        }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "input", [ "A":> obj; null; "B" :> obj ] :> obj ])
    let expected = NameValueLookup.ofList [ "nnList", upcast "[\"A\",null,\"B\"]" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists of non-nulls to be null`` () =
    let ast = parse """query q($input: [String!]) {
          listNN(input: $input)
        }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "input", null ])
    let expected = NameValueLookup.ofList [ "listNN", upcast "null" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists of non-nulls to contain values`` () =
    let ast = parse """query q($input: [String!]) {
          listNN(input: $input)
        }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "input", [ "A" ] :> obj ])
    let expected = NameValueLookup.ofList [ "listNN", upcast "[\"A\"]" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow lists of non-nulls to contain null`` () =
    let ast = parse """query q($input: [String!]) {
          listNN(input: $input)
        }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "input", [ "A":> obj; null; "B" :> obj ] :> obj ])
    match actual with
    | Direct(data, errors) ->
        hasError "Variable '$input': list element expected value of type String but got None" errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists of non-nulls to be null`` () =
    let ast = parse """query q($input: [String!]!) {
          nnListNN(input: $input)
        }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "input", null ])
    match actual with
    | Direct(data, errors) ->
        hasError "Variable '$input': expected value of type [String!]!, but no value was found" errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists of non-nulls to contain values`` () =
    let ast = parse """query q($input: [String!]!) {
          nnListNN(input: $input)
        }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "input", [ "A" ] :> obj ])
    let expected = NameValueLookup.ofList [ "nnListNN", upcast "[\"A\"]" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists of non-nulls to contain null`` () =
    let ast = parse """query q($input: [String!]!) {
          nnListNN(input: $input)
        }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "input", [ "A":> obj; null; "B" :> obj ] :> obj ])
    match actual with
    | Direct(data, errors) ->
        hasError "Variable '$input': list element expected value of type String but got None" errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow invalid types to be used as values`` () =
    let ast = parse """query q($input: TestType!) {
          fieldWithObjectInput(input: $input)
        }"""
    // as that kind of an error inside of a query is guaranteed to fail in every call, we're gonna to fail noisy here
    let e = throws<MalformedQueryException> (fun () ->
        Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "input", [ "A":> obj, "B" :> obj ] :> obj ])
        |> sync
        |> ignore)
    e.Message |> equals "GraphQL query defined variable '$input' of type 'TestType!' which is not an input type definition"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow unknown types to be used as values`` () =
    let ast = parse """query q($input: UnknownType!) {
          fieldWithObjectInput(input: $input)
        }"""
    // as that kind of an error inside of a query is guaranteed to fail in every call, we're gonna to fail noisy here
    let e = throws<MalformedQueryException> (fun () ->
        Executor(schema).AsyncExecute(ast, variables = Map.ofList [ "input", "whoknows" :> obj ])
        |> sync
        |> ignore)
    e.Message |> equals "GraphQL query defined variable '$input' of type 'UnknownType!' which is not known in the current schema"

[<Fact>]
let ``Execute uses argument default value when no argument was provided`` () =
    let ast = parse """{ fieldWithDefaultArgumentValue }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithDefaultArgumentValue", upcast "\"hello world\"" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute uses argument default value when nullable variable provided`` () =
    let ast = parse """query optionalVariable($optional: String) {
        fieldWithDefaultArgumentValue(input: $optional)
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList ["optional", null ])
    let expected = NameValueLookup.ofList [ "fieldWithDefaultArgumentValue", upcast "\"hello world\"" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact(Skip = "This test does not pass anymore since validation requires that literal representations of input types match their types.")>]
let ``Execute uses argument default value when argument provided cannot be parsed`` () =
    let ast = parse """{ fieldWithDefaultArgumentValue(input: WRONG_TYPE) }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithDefaultArgumentValue", upcast "\"hello world\"" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"


[<Fact>]
let ``Execute handles enum input as variable`` () =
    let ast = parse """query fieldWithEnumValue($enumVar: EnumTestType!) {
        fieldWithEnumInput(input: $enumVar)
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = Map.ofList ["enumVar", "Foo" :> obj ])
    let expected = NameValueLookup.ofList [ "fieldWithEnumInput", upcast "{\"case\":\"Foo\"}" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"


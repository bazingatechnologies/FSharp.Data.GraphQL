module FSharp.Data.GraphQL.Tests.MiddlewareTests

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middleware
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Ast

#nowarn "40"

type Root = {
    clientId : int
}

and Subject =
    | A of A
    | B of B

and A = {
    id : int
    value : string
    subjects : int list
}

and B = {
    id : int
    value : string
    subjects : int list
}

let executor =
    let a1 : A = { id = 1; value = "A1"; subjects = [ 2; 6 ] }
    let a2 : A = { id = 2; value = "A2"; subjects = [ 1; 3; 5 ] }
    let a3 : A = { id = 3; value = "A3"; subjects = [ 1; 2; 4 ] }
    let b1 = { id = 4; value = "1000"; subjects = [ 1; 5 ] }
    let b2 = { id = 5; value = "2000"; subjects = [ 3; 4 ; 6] }
    let b3 = { id = 6; value = "3000"; subjects = [ 1; 3; 5 ] }
    let al = [ a1; a2; a3 ]
    let bl = [ b1; b2; b3 ]
    let getA id = al |> List.tryFind (fun a -> a.id = id)
    let getB id = bl |> List.tryFind (fun b -> b.id = id)
    let subjects = (al |> List.map A) @ (bl |> List.map B)
    let getSubject id =
        let matchesId id = function A a -> a.id = id | B b -> b.id = id
        subjects |> List.tryFind (matchesId id)
    let rec SubjectType =
        Define.Union(
            name = "Subject",
            options = [ AType; BType ],
            resolveValue = (fun u -> match u with A a -> box a | B b -> box b),
            resolveType = (fun u -> match u with A _ -> upcast AType | B _ -> upcast BType))
    and AType =
        Define.Object<A>(
            name = "A",
            isTypeOf = (fun o -> o :? A),
            fieldsFn = fun () ->
                [ Define.Field("id", Int, resolve = fun _ a -> a.id)
                  Define.Field("value", String, resolve = fun _ a -> a.value)
                  Define.Field("subjects", Nullable (SeqOf (Nullable SubjectType)),
                    resolve = fun _ (a : A) -> a.subjects |> List.map getSubject |> List.toSeq |> Some)
                    .WithQueryWeight(1.0) ])
    and BType =
        Define.Object<B>(
            name = "B",
            isTypeOf = (fun o -> o :? B),
            fieldsFn = fun () ->
                [ Define.Field("id", Int, resolve = fun _ b -> b.id)
                  Define.Field("value", String, resolve = fun _ b -> b.value)
                  Define.Field("subjects", Nullable (SeqOf (Nullable SubjectType)),
                    resolve = fun _ (b : B) -> b.subjects |> List.map getSubject |> List.toSeq |> Some)
                    .WithQueryWeight(1.0) ])
    let Query =
        Define.Object<Root>(
            name = "Query",
            fields =
                [ Define.Field("A", Nullable AType, "A Field", [ Define.Input("id", Int) ], resolve = fun ctx _ -> getA (ctx.Arg("id")))
                  Define.Field("B", Nullable BType, "B Field", [ Define.Input("id", Int) ], resolve = fun ctx _ -> getB (ctx.Arg("id"))) ])
    let schema = Schema(Query)
    let middleware =
        [ Define.QueryWeightMiddleware(2.0, true)
          Define.ObjectListFilterMiddleware<A, Subject option>(true)
          Define.ObjectListFilterMiddleware<B, Subject option>(true) ]
    Executor(schema, middleware)

let execute (query : Document) =
    executor.AsyncExecute(query) |> sync

let expectedErrors : Error list =
    [ "Query complexity exceeds maximum threshold. Please reduce query complexity and try again.", [] ]

[<Fact>]
let ``Simple query: Should pass when below threshold``() =
    let query =
        parse """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects { ...Value }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A", upcast NameValueLookup.ofList [
                "id", upcast 1
                "value", upcast "A1"
                "subjects", upcast [
                    NameValueLookup.ofList [
                        "id", upcast 2
                        "value", upcast "A2"
                    ]
                    NameValueLookup.ofList [
                        "id", upcast 6
                        "value", upcast "3000"
                    ]
                ]
            ]
        ]
    let result = execute query
    match result with
    | Direct (data, errors) ->
        empty errors
        data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQLResponse"
    result.Metadata.TryFind<float>("queryWeightThreshold") |> equals (Some 2.0)
    result.Metadata.TryFind<float>("queryWeight") |> equals (Some 1.0)

[<Fact>]
let ``Simple query: Should not pass when above threshold``() =
    let query =
        parse """query testQuery {
                    A (id : 1) {
                        id
                        value
                        subjects { ...All }
                    }
        }

        fragment Value on Subject {
                    ...on A {
                        id
                        value
                    }
                    ...on B {
                        id
                        value
                    }
        }

        fragment Inner on Subject {
                    ...on A {
                        id
                        value
                        subjects { ...Value }
                    }
                    ...on B {
                        id
                        value
                        subjects { ...Value }
                    }
        }

        fragment AllA on A {
                    id
                    value
                    subjects { ...Inner }
        }

        fragment AllB on B {
                    id
                    value
                    subjects { ...Inner }
        }

        fragment All on Subject {
                    ...on A { ...AllA }
                    ...on B { ...AllB }
        }"""
    let result = execute query
    match result with
    | Direct (data, errors) ->
        errors |> equals expectedErrors
        data.["data"] |> isNameValueDict |> empty
    | _ -> fail "Expected Direct GQLResponse"
    result.Metadata.TryFind<float>("queryWeightThreshold") |> equals (Some 2.0)
    result.Metadata.TryFind<float>("queryWeight") |> equals (Some 3.0)

[<Fact>]
let ``Deferred queries : Should pass when below threshold``() =
    let query =
        parse """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects @defer { ...Value }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A", upcast NameValueLookup.ofList [
                "id", upcast 1
                "value", upcast "A1"
                "subjects", upcast null
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast [
                NameValueLookup.ofList [
                    "id", upcast 2
                    "value", upcast "A2"
                ]
                NameValueLookup.ofList [
                    "id", upcast 6
                    "value", upcast "3000"
                ]
            ]
            "path", upcast [ "A" :> obj; "subjects" :> obj ]
        ]
    let result = execute query
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data.["data"] |> equals (upcast expected)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received |> single |> equals (upcast expectedDeferred)
    result.Metadata.TryFind<float>("queryWeightThreshold") |> equals (Some 2.0)
    result.Metadata.TryFind<float>("queryWeight") |> equals (Some 2.0)

[<Fact>]
let ``Streamed queries : Should pass when below threshold``() =
    let query =
        parse """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects @stream { ...Value }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A", upcast NameValueLookup.ofList [
                "id", upcast 1
                "value", upcast "A1"
                "subjects", upcast []
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", upcast [
                NameValueLookup.ofList [
                    "id", upcast 2
                    "value", upcast "A2"
                ]
            ]
            "path", upcast [ "A" :> obj; "subjects" :> obj; 0 :> obj ]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", upcast [
                NameValueLookup.ofList [
                    "id", upcast 6
                    "value", upcast "3000"
                ]
            ]
            "path", upcast [ "A" :> obj; "subjects" :> obj; 1 :> obj ]
        ]
    let result = execute query
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data.["data"] |> equals (upcast expected)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        sub.Received
        |> Seq.cast<NameValueLookup>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> ignore
    result.Metadata.TryFind<float>("queryWeightThreshold") |> equals (Some 2.0)
    result.Metadata.TryFind<float>("queryWeight") |> equals (Some 2.0)

[<Fact>]
let ``Deferred and Streamed queries : Should not pass when above threshold``() =
    let query =
        sprintf """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects @%s { ...All }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }

        fragment Inner on Subject {
                ...on A {
                    id
                    value
                    subjects { ...Value }
                }
                ...on B {
                    id
                    value
                    subjects { ...Value }
                }
        }

        fragment AllA on A {
                id
                value
                subjects { ...Inner }
        }

        fragment AllB on B {
                id
                value
                subjects { ...Inner }
        }

        fragment All on Subject {
                ...on A { ...AllA }
                ...on B { ...AllB }
        }"""
    asts query
    |> Seq.map execute
    |> Seq.iter (fun result ->
        ensureDirect result <| fun data errors ->
            errors |> equals expectedErrors
            data.["data"] |> isNameValueDict |> empty
        result.Metadata.TryFind<float>("queryWeightThreshold") |> equals (Some 2.0)
        result.Metadata.TryFind<float>("queryWeight") |> equals (Some 3.0))

[<Fact>]
let ``Inline fragment query : Should pass when below threshold``() =
    let query =
        parse """query testQuery {
            A (id : 1) {
                id
                value
                subjects {
                    ... on A {
                        id
                        value
                    }
                    ... on B {
                        id
                    }
                }
            }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A", upcast NameValueLookup.ofList [
                "id", upcast 1
                "value", upcast "A1"
                "subjects", upcast [
                    NameValueLookup.ofList [
                        "id", upcast 2
                        "value", upcast "A2"
                    ]
                    NameValueLookup.ofList [
                        "id", upcast 6
                    ]
                ]
            ]
        ]
    let result = execute query
    ensureDirect result <| fun data errors ->
        empty errors
        data.["data"] |> equals (upcast expected)
    result.Metadata.TryFind<float>("queryWeightThreshold") |> equals (Some 2.0)
    result.Metadata.TryFind<float>("queryWeight") |> equals (Some 1.0)

[<Fact>]
let ``Inline fragment query : Should not pass when above threshold``() =
    let query =
        parse """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects {
                        ... on A {
                            id
                            value
                        }
                        ... on B {
                            id
                            subjects { ...Inner }
                        }
                    }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }

        fragment Inner on Subject {
                ...on A {
                    id
                    value
                    subjects { ... Value }
                }
                ... on B {
                    id
                    value
                    subjects { ...Value }
                }
        }"""
    let result = execute query
    ensureDirect result <| fun data errors ->
        errors |> equals expectedErrors
        data.["data"] |> isNameValueDict |> empty
    result.Metadata.TryFind<float>("queryWeightThreshold") |> equals (Some 2.0)
    result.Metadata.TryFind<float>("queryWeight") |> equals (Some 3.0)

[<Fact>]
let ``Object list filter: should return filter information in Metadata``() =
    let query =
        parse """query testQuery {
                A (id : 1) {
                    id
                    value
                    subjects (filter : { value_starts_with: "A", id : 2 }) { ...Value }
                }
        }

        fragment Value on Subject {
                ...on A {
                    id
                    value
                }
                ...on B {
                    id
                    value
                }
        }"""
    let expected =
        NameValueLookup.ofList [
            "A", upcast NameValueLookup.ofList [
                "id", upcast 1
                "value", upcast "A1"
                "subjects", upcast [
                    NameValueLookup.ofList [
                        "id", upcast 2
                        "value", upcast "A2"
                    ]
                    NameValueLookup.ofList [
                        "id", upcast 6
                        "value", upcast "3000"
                    ]
                ]
            ]
        ]
    let expectedFilter =
        "subjects", And (Equals { FieldName = "id"; Value = 2L }, StartsWith { FieldName = "value"; Value = "A" })
    let result = execute query
    ensureDirect result <| fun data errors ->
        empty errors
        data.["data"] |> equals (upcast expected)
    result.Metadata.TryFind<float>("queryWeightThreshold") |> equals (Some 2.0)
    result.Metadata.TryFind<float>("queryWeight") |> equals (Some 1.0)
    result.Metadata.TryFind<(string * ObjectListFilter) list>("filters") |> equals (Some [ expectedFilter ])

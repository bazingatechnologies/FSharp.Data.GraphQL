/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.DirectivesTests

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type Data = { A: string; B: string }
let data = { A = "a"; B = "b" }

let schema = 
    Schema(Define.Object("TestType", [ Define.AutoField("a", String); Define.AutoField("b", String) ])) :> Schema<Data>

let private execAndCompare query expected =
    let actual = sync <| Executor(schema).AsyncExecute(parse query, data)
    noErrors actual
    actual.["data"] |> equals (upcast expected)
    
[<Fact>]
let ``Execute works without directives``() = 
    execAndCompare "{ a, b }" (NameValueLookup.ofList [("a", "a" :> obj); ("b", upcast "b")])
    
// SCALARS

[<Fact>]
let ``Execute with include works on scalars: if true, includes scalar``() = 
    execAndCompare "{ a, b @include(if: true) }" (NameValueLookup.ofList [("a", "a" :> obj); ("b", upcast "b")])

[<Fact>]
let ``Execute with include works on scalars: if false, excludes scalar``() = 
    execAndCompare "{ a, b @include(if: false) }" (NameValueLookup.ofList [("a", "a" :> obj)])
    
[<Fact>]
let ``Execute with skip works on scalars: if false, includes scalar``() = 
    execAndCompare "{ a, b @skip(if: false) }" (NameValueLookup.ofList [("a", "a" :> obj); ("b", upcast "b")])
    
[<Fact>]
let ``Execute with skip works on scalars: if true, excludes scalar``() = 
    execAndCompare "{ a, b @skip(if: true) }" (NameValueLookup.ofList [("a", "a" :> obj)])
    
// FRAGMENT SPREADS

[<Fact>]
let ``Execute with include works on fragment spreads: if true, includes fragment spread``() = 
    execAndCompare 
        """query Q {
          a
          ...Frag @include(if: true)
        }
        fragment Frag on TestType {
          b
        }""" 
        (NameValueLookup.ofList [("a", "a" :> obj); ("b", upcast "b")])

[<Fact>]
let ``Execute with include works on fragment spreads: if false, excludes fragment spread``() = 
    execAndCompare 
        """query Q {
          a
          ...Frag @include(if: false)
        }
        fragment Frag on TestType {
          b
        }""" 
        (NameValueLookup.ofList [("a", "a" :> obj)])
    
[<Fact>]
let ``Execute with skip works on fragment spreads: if false, includes fragment spread``() = 
    execAndCompare 
        """query Q {
          a
          ...Frag @skip(if: false)
        }
        fragment Frag on TestType {
          b
        }""" 
        (NameValueLookup.ofList [("a", "a" :> obj); ("b", upcast "b")])
    
[<Fact>]
let ``Execute with skip works on fragment spreads: if true, excludes fragment spread``() = 
    execAndCompare 
        """query Q {
          a
          ...Frag @skip(if: true)
        }
        fragment Frag on TestType {
          b
        }""" 
        (NameValueLookup.ofList [("a", "a" :> obj)])
    
// INLINE FRAGMENTS

[<Fact>]
let ``Execute with include works on inline fragments: if true, includes inline fragment``() = 
    execAndCompare 
        """query Q {
          a
          ... on TestType @include(if: true) {
            b
          }
        }
        fragment Frag on TestType {
          b
        }""" 
        (NameValueLookup.ofList [("a", "a" :> obj); ("b", upcast "b")])

[<Fact>]
let ``Execute with include works on inline fragments: if false, excludes inline fragment``() = 
    execAndCompare 
        """query Q {
          a
          ... on TestType @include(if: false) {
            b
          }
        }
        fragment Frag on TestType {
          b
        }""" 
        (NameValueLookup.ofList [("a", "a" :> obj)])
    
[<Fact>]
let ``Execute with skip works on inline fragments: if false, includes inline fragment``() = 
    execAndCompare 
        """query Q {
          a
          ... on TestType @skip(if: false) {
            b
          }
        }
        fragment Frag on TestType {
          b
        }""" 
        (NameValueLookup.ofList [("a", "a" :> obj); ("b", upcast "b")])
    
[<Fact>]
let ``Execute with skip works on inline fragments: if true, excludes inline fragment``() = 
    execAndCompare 
        """query Q {
          a
          ... on TestType @skip(if: true) {
            b
          }
        }
        fragment Frag on TestType {
          b
        }""" 
        (NameValueLookup.ofList [("a", "a" :> obj)])
    
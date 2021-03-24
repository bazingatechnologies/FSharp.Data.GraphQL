namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Text
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Serialization
open System.Collections.Generic

[<AutoOpen>]
module Helpers =
    let tee f x =
        f x
        x

[<AutoOpen>]
module StringHelpers =
    let utf8String (bytes : byte seq) =
        bytes
        |> Seq.filter (fun i -> i > 0uy)
        |> Array.ofSeq
        |> Encoding.UTF8.GetString

    let utf8Bytes (str : string) =
        str |> Encoding.UTF8.GetBytes

    let isNullOrWhiteSpace (str : string) =
        String.IsNullOrWhiteSpace(str)

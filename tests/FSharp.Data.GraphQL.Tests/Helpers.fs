/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
[<AutoOpen>]
module Helpers

open System
open System.Collections.Generic
open Xunit
open FSharp.Data.GraphQL.Execution
open System.Threading

let isType<'a> actual = Assert.IsAssignableFrom<'a>(actual)
let isSeq<'a> actual = isType<'a seq> actual
let isDict<'k, 'v> actual = isSeq<KeyValuePair<'k, 'v>> actual
let isNameValueDict actual = isDict<string, obj> actual
let equals (expected : 'x) (actual : 'x) =
    Assert.True((actual = expected), sprintf "expected %+A\nbut got %+A" expected actual)
let notEquals (expected : 'x) (actual : 'x) =
    Assert.True((actual <> expected), sprintf "unexpected %+A" expected)
let noErrors (result: IDictionary<string, obj>) =
    match result.TryGetValue("errors") with
    | true, errors -> failwithf "expected ExecutionResult to have no errors but got %+A" errors
    | false, _ -> ()
let empty (xs: 'a seq) =
    Assert.True(Seq.isEmpty xs, sprintf "expected empty sequence, but got %A" xs)
let fail (message: string) =
    Assert.True(false, message)
let single (xs : 'a seq) =
    let length = Seq.length xs
    if length <> 1
    then fail <| sprintf "Expected single item in sequence, but found %i items.\n%A" length xs
    Seq.head xs
let throws<'e when 'e :> exn> (action : unit -> unit) = Assert.Throws<'e>(action)
let sync = Async.RunSynchronously
let is<'t> (o: obj) = o :? 't
let hasError errMsg (errors: Error seq) =
    let containsMessage = errors |> Seq.exists (fun (message, _) -> message.Contains(errMsg))
    Assert.True (containsMessage, sprintf "Expected to contain message '%s', but no such message was found. Messages found: %A" errMsg errors)
let (<??) opt other =
    match opt with
    | None -> Some other
    | _ -> opt
let undefined (value: 't) =
    Assert.True((value = Unchecked.defaultof<'t>), sprintf "Expected value to be undefined, but was: %A" value)
let contains (expected : 'a) (xs : 'a seq) =
    Assert.Contains(expected, xs); xs
let itemEquals (index : int) (expected : 'a) (xs : 'a seq) =
    match xs |> Seq.tryItem index with
    | Some item -> item |> equals expected
    | None -> fail <| sprintf "Expected sequence to contain item at index %i, but sequence does not contain enough elements" index
    xs

let greaterThanOrEqual expected actual =
    Assert.True(actual >= expected, sprintf "Expected value to be greather than or equal to %A, but was: %A" expected actual)

open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Microsoft.FSharp.Reflection
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Server.Middlewares.AspNetCore

type internal OptionConverter() =
    inherit JsonConverter()

    override __.CanConvert(t) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>
    override __.WriteJson(writer, value, serializer) =
        let value =
            if isNull value then null
            else
                let _,fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, value.GetType())
                fields.[0]
        serializer.Serialize(writer, value)

    override __.ReadJson(reader, t, _, serializer) =
        let innerType = t.GetGenericArguments().[0]
        let innerType =
            if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
            else innerType
        let value = serializer.Deserialize(reader, innerType)
        let cases = FSharpType.GetUnionCases(t)
        if isNull value then FSharpValue.MakeUnion(cases.[0], [||])
        else FSharpValue.MakeUnion(cases.[1], [|value|])

let internal settings = JsonSerializerSettings()
settings.Converters <- [| OptionConverter() |]
settings.ContractResolver <- CamelCasePropertyNamesContractResolver()

let toJson (o: 't) : string = JsonConvert.SerializeObject(o, settings)

let fromJson<'t> (json: string) : 't = JsonConvert.DeserializeObject<'t>(json, settings)

let asts query =
    ["defer"; "stream"]
    |> Seq.map (query >> parse)

let set (mre : ManualResetEvent) =
    mre.Set() |> ignore

let reset (mre : ManualResetEvent) =
    mre.Reset() |> ignore

let wait (mre : ManualResetEvent) errorMsg =
    if TimeSpan.FromSeconds(float 30) |> mre.WaitOne |> not
    then fail errorMsg

let rec waitFor (condition : unit -> bool) (times : int) errorMsg =
    Thread.Sleep(100) // Wait a bit before checking condition
    if not (condition ())
    then
        if times = 0
        then fail errorMsg
        else waitFor condition (times - 1) errorMsg

let queryEquals expected (operation : Operation) =
    operation.Query |> equals expected; operation

let containsOperation expectedQuery (operations : Operation seq) =
    match operations |> Seq.tryFind (fun op -> op.Query = expectedQuery) with
    | Some op -> op
    | None -> failwithf "There is no operation having query `%s` on the operation list." expectedQuery

let containsFile varName (operation : Operation) =
    match operation.Variables |> Map.tryFind varName with
    | Some (:? File as f) -> f
    | _ -> failwithf "There is no file in the request using variable name `%s`." varName

let containsFiles varName (operation : Operation) =
    match operation.Variables |> Map.tryFind varName with
    | Some (:? seq<obj> as fs) -> fs |> Seq.cast<File>
    | _ -> failwithf "There is no file sequence in the request using variable name `%s`." varName

let hasName expected (file : File) =
    file.Name |> equals expected; file

let hasContentType expected (file : File) =
    file.ContentType |> equals expected; file
    
let hasContent expected (file : File) =
    if file.Content.Position <> 0L then file.Content.Position <- 0L
    use reader = new System.IO.StreamReader(file.Content)
    reader.ReadToEnd() |> equals expected

let hasContentTypes expected (files : File seq) =
    files |> Seq.mapi (fun ix f -> hasContentType (expected |> Seq.item ix) f)

let hasNames expected (files : File seq) =
    files |> Seq.mapi (fun ix f -> hasName (expected |> Seq.item ix) f)

let hasContents expected (files : File seq) =
    files |> Seq.mapi (fun ix f -> hasContent (expected |> Seq.item ix) f)

let rec ensureThat (condition : unit -> bool) (times : int) errorMsg =
    Thread.Sleep(100) // Wait a bit before checking condition
    if not (condition ())
    then fail errorMsg
    elif times > 0
    then ensureThat condition (times - 1) errorMsg
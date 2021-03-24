namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open Microsoft.AspNetCore.Http
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Control.Tasks
open Giraffe

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

module HttpHandlers =

    let private jsonOptions =
        NameValueLookupConverter() :> JsonConverter
        |> Array.singleton
        |> Json.getSerializerOptions

    let internalServerError : HttpHandler = setStatusCode 500

    let okWithStr str : HttpHandler = setStatusCode 200 >=> text str

    let setCorsHeaders : HttpHandler =
        setHttpHeader "Access-Control-Allow-Origin" "*"
        >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"

    let setContentTypeAsJson : HttpHandler =
        setHttpHeader "Content-Type" "application/json"

    let private graphQL (next : HttpFunc) (ctx : HttpContext) = task {

        let serialize d = JsonSerializer.Serialize(d, jsonOptions)

        let json =
            function
            | Direct (data, _) ->
                JsonSerializer.Serialize(data, jsonOptions)
            | Deferred (data, _, deferred) ->
                deferred |> Observable.add(fun d -> printfn "Deferred: %s" (serialize d))
                JsonSerializer.Serialize(data, jsonOptions)
            | Stream data ->
                data |> Observable.add(fun d -> printfn "Subscription data: %s" (serialize d))
                "{}"

        let removeWhitespacesAndLineBreaks (str : string) = str.Trim().Replace("\r\n", " ")

        //let asycnReadStream (s : Stream) = async {
        //    let ms = new MemoryStream(4096)
        //    do! s.CopyToAsync(ms) |> Async.AwaitTask
        //     ms.Seek(0L, SeekOrigin.Begin) |> ignore
        //    return ms
        //}

        //use! stream = asycnReadStream ctx.Request.Body
        //let! data = async {
        //    if stream.Length > 0L
        //    then
        //        let! document = JsonDocument.ParseAsync(stream) |> Async.AwaitTask
        //        return document.RootElement |> ValueSome
        //    else
        //        return ValueNone
        //}

        let readStream (s : Stream) =
            let ms = new MemoryStream(4096)
            do s.CopyTo(ms)
            ms.Seek(0L, SeekOrigin.Begin) |> ignore
            ms

        use stream = readStream ctx.Request.Body
        let data =
            if stream.Length > 0L
            then
                let document = JsonDocument.Parse(stream)
                document.RootElement |> ValueSome
            else
                ValueNone

        let query =
            data |> ValueOption.bind (fun data ->
                match data.TryGetProperty FIELD_Query with
                | true, query -> query.GetString() |> ValueSome
                | _ -> ValueNone)

        let variables =
            data |> ValueOption.map (fun data ->
                match data.TryGetProperty FIELD_Variables with
                | true, variables when variables.ValueKind <> JsonValueKind.Null ->
                    variables.EnumerateObject () |> Seq.map (fun v -> v.Name, v.Value) |> Map.ofSeq
                | _ -> Map.empty) |> ValueOption.defaultValue Map.empty

        match query with
        | ValueSome query ->
            printfn "Received query: %s" query
            if not variables.IsEmpty then printfn "Received variables: %A" variables
            let query = removeWhitespacesAndLineBreaks query
            let root = { RequestId = System.Guid.NewGuid().ToString() }
            let plan = Schema.executor.CreateExecutionPlan(query)
            let variables =
                plan.Variables
                |> Seq.choose (fun v ->
                    variables.TryFind v.Name
                    |> Option.map (fun j -> v.Name, JsonSerializer.Deserialize(j.GetRawText(), v.TypeDef.Type, jsonOptions)))
                |> Map.ofSeq
            let result = Schema.executor.AsyncExecute(plan, root, variables) |> Async.RunSynchronously
            printfn "Result metadata: %A" result.Metadata
            return! okWithStr (json result) next ctx
        | ValueNone ->
            let result = Schema.executor.AsyncExecute(Introspection.IntrospectionQuery) |> Async.RunSynchronously
            printfn "Result metadata: %A" result.Metadata
            return! okWithStr (json result) next ctx
    }

    let webApp : HttpHandler =
        setCorsHeaders
        >=> graphQL
        >=> setContentTypeAsJson

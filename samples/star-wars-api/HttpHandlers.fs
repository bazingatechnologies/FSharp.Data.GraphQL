namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System.Text
open Giraffe
open Microsoft.AspNetCore.Http
open Newtonsoft.Json
open FSharp.Data.GraphQL.Execution
open System.IO
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Control.Tasks
open Newtonsoft.Json.Linq

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

module HttpHandlers =
    let private converters : JsonConverter [] = [| OptionConverter() |]
    let private jsonSettings = jsonSerializerSettings converters

    let internalServerError : HttpHandler = setStatusCode 500

    let okWithStr str : HttpHandler = setStatusCode 200 >=> text str

    let setCorsHeaders : HttpHandler =
        setHttpHeader "Access-Control-Allow-Origin" "*"
        >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"

    let setContentTypeAsJson : HttpHandler =
        setHttpHeader "Content-Type" "application/json"

    let private graphQL (next : HttpFunc) (ctx : HttpContext) = task {
        let serialize d = JsonConvert.SerializeObject(d, jsonSettings)

        let deserialize (data : string) =
            let getMap (token : JToken) =
                let rec mapper (name : string) (token : JToken) =
                    match name, token.Type with
                    | "variables", JTokenType.Object -> token.Children<JProperty>() |> Seq.map (fun x -> x.Name, mapper x.Name x.Value) |> Map.ofSeq |> box
                    | name, JTokenType.Array -> token |> Seq.map (fun x -> mapper name x) |> Array.ofSeq |> box
                    | _ -> (token :?> JValue).Value
                token.Children<JProperty>()
                |> Seq.map (fun x -> x.Name, mapper x.Name x.Value)
                |> Map.ofSeq
            if System.String.IsNullOrWhiteSpace(data)
            then None
            else data |> JToken.Parse |> getMap |> Some

        let json =
            function
            | Direct (data, _) ->
                JsonConvert.SerializeObject(data, jsonSettings)
            | Deferred (data, _, deferred) ->
                deferred |> Observable.add(fun d -> printfn "Deferred: %s" (serialize d))
                JsonConvert.SerializeObject(data, jsonSettings)
            | Stream data ->
                data |> Observable.add(fun d -> printfn "Subscription data: %s" (serialize d))
                "{}"

        let removeWhitespacesAndLineBreaks (str : string) = str.Trim().Replace("\r\n", " ")

        let readStream (s : Stream) =
            use ms = new MemoryStream(4096)
            s.CopyTo(ms)
            ms.ToArray()

        let data = Encoding.UTF8.GetString(readStream ctx.Request.Body) |> deserialize

        let query =
            data |> Option.bind (fun data ->
                if data.ContainsKey("query")
                then
                    match data.["query"] with
                    | :? string as x -> Some x
                    | _ -> failwith "Failure deserializing repsonse. Could not read query - it is not stringified in request."
                else None)

        let variables =
            data |> Option.bind (fun data ->
                if data.ContainsKey("variables")
                then
                    match data.["variables"] with
                    | null -> None
                    | :? string as x -> deserialize x
                    | :? Map<string, obj> as x -> Some x
                    | _ -> failwith "Failure deserializing response. Could not read variables - it is not a object in the request."
                else None)

        match query, variables  with
        | Some query, Some variables ->
            printfn "Received query: %s" query
            printfn "Received variables: %A" variables
            let query = removeWhitespacesAndLineBreaks query
            let root = { RequestId = System.Guid.NewGuid().ToString(); ServiceProvider = ctx.RequestServices }
            let result = Schema.executor.AsyncExecute(query, root, variables) |> Async.RunSynchronously
            printfn "Result metadata: %A" result.Metadata
            return! okWithStr (json result) next ctx
        | Some query, None ->
            printfn "Received query: %s" query
            let query = removeWhitespacesAndLineBreaks query
            let result = Schema.executor.AsyncExecute(query) |> Async.RunSynchronously
            printfn "Result metadata: %A" result.Metadata
            return! okWithStr (json result) next ctx
        | None, _ ->
            let result = Schema.executor.AsyncExecute(Introspection.IntrospectionQuery) |> Async.RunSynchronously
            printfn "Result metadata: %A" result.Metadata
            return! okWithStr (json result) next ctx
    }

    let webApp : HttpHandler =
        setCorsHeaders
        >=> graphQL
        >=> setContentTypeAsJson

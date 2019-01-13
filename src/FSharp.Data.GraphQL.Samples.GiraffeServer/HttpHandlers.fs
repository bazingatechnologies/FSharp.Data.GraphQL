namespace FSharp.Data.GraphQL.Samples.GiraffeServer

open System.Text
open Giraffe
open Microsoft.AspNetCore.Http
open Newtonsoft.Json
open FSharp.Data.GraphQL.Execution
open System.IO
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middlewares.AspNetCore
open Microsoft.AspNetCore.WebUtilities
open Newtonsoft.Json.Linq
open Giraffe.HttpStatusCodeHandlers.RequestErrors
open Giraffe.HttpStatusCodeHandlers.Successful
open Newtonsoft.Json.Serialization

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

module HttpHandlers =
    let internalServerError : HttpHandler = setStatusCode 500

    let setCorsHeaders : HttpHandler =
        setHttpHeader "Access-Control-Allow-Origin" "*"
        >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"

    let isMultipartRequest (req : HttpRequest) =
        not (System.String.IsNullOrEmpty(req.ContentType)) && req.ContentType.Contains("multipart/form-data")

    let getMultipartRequestBoundary (req : HttpRequest) =
        req.Headers.GetCommaSeparatedValues("Content-Type")
        |> Seq.map (fun v -> v.TrimStart())
        |> Seq.tryFind (fun v -> v.Contains("boundary"))
        |> Option.map (fun v -> v.Remove(0, v.IndexOf('=') + 1))
        |> Option.map (fun v -> v.Trim('"'))

    let private graphQL : HttpHandler = 
        fun (next : HttpFunc) (ctx : HttpContext) -> task {
            let copyBodyToMemory (req : HttpRequest) =
                let ms = new MemoryStream(4096)
                req.Body.CopyTo(ms)
                ms.Position <- 0L
                ms
            let jsonSettings =
                JsonSerializerSettings(Converters = [| OptionConverter() |], ContractResolver = CamelCasePropertyNamesContractResolver())
            let serializer =
                JsonSerializer(ContractResolver = CamelCasePropertyNamesContractResolver())
                |> tee (fun s -> s.Converters.Add(OptionConverter()))
            let json =
                function
                | Direct (data, _) ->
                    JObject.FromObject(data, serializer)
                | Deferred (data, _, deferred) ->
                    deferred |> Observable.add(fun d -> printfn "Deferred: %s" (JsonConvert.SerializeObject(d, jsonSettings)))
                    JObject.FromObject(data, serializer)
                | Stream _ ->
                    JObject()
            let normalizeQuery (query : string) = 
                query.Trim().Replace("\r\n", " ")
            if isMultipartRequest ctx.Request
            then
                match getMultipartRequestBoundary ctx.Request with
                | Some boundary ->
                    use ms = copyBodyToMemory(ctx.Request)
                    let reader = MultipartReader(boundary, ms)
                    let request = MultipartRequest.read reader |> Async.AwaitTask |> Async.RunSynchronously
                    let results = 
                        request.Operations
                        |> Seq.map (fun op -> 
                            let query = op.Query |> normalizeQuery
                            Schema.executor.AsyncExecute(query, variables = op.Variables, data = Schema.root) |> Async.RunSynchronously)
                        |> Seq.map json
                        |> List.ofSeq
                    match results with
                    | [ result ] -> 
                        return! OK result next ctx
                    | results -> 
                        let result = JArray.FromObject(results)
                        return! OK result next ctx
                | None -> 
                    return! badRequest (text "Invalid multipart request header: missing boundary value.") next ctx
            else
                let tryParse fieldName data =
                    let raw = Encoding.UTF8.GetString data
                    if System.String.IsNullOrWhiteSpace(raw) |> not
                    then
                        let map = JsonConvert.DeserializeObject<Map<string,string>>(raw)
                        match Map.tryFind fieldName map with
                        | Some "" -> None
                        | s -> s
                    else None
                let mapString = JsonConvert.DeserializeObject<Map<string, obj>> |> Option.map
                let readyBody (req : HttpRequest) =
                    use ms = copyBodyToMemory(req)
                    ms.ToArray()
                let body = readyBody ctx.Request
                let query = body |> tryParse "query"
                let variables = body |> tryParse "variables" |> mapString
                match query, variables  with
                | Some query, Some variables ->
                    printfn "Received query: %s" query
                    printfn "Received variables: %A" variables
                    let query = query |> normalizeQuery
                    let result = Schema.executor.AsyncExecute(query, variables = variables, data = Schema.root) |> Async.RunSynchronously
                    printfn "Result metadata: %A" result.Metadata
                    return! OK (json result) next ctx
                | Some query, None ->
                    printfn "Received query: %s" query
                    let query = query |> normalizeQuery
                    let result = Schema.executor.AsyncExecute(query) |> Async.RunSynchronously
                    printfn "Result metadata: %A" result.Metadata
                    return! OK (json result) next ctx
                | None, _ ->
                    let result = Schema.executor.AsyncExecute(Introspection.introspectionQuery) |> Async.RunSynchronously
                    printfn "Result metadata: %A" result.Metadata
                    return! OK (json result) next ctx
        }

    let webApp : HttpHandler = 
        setCorsHeaders
        >=> graphQL

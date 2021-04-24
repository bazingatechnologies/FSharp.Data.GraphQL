namespace FSharp.Data.GraphQL.Server.AspNet

open System
open System.IO
open System.Net
open System.Text.Json
open System.Threading.Tasks
open System.Security.Cryptography

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Net.Http.Headers
open Microsoft.Extensions.Primitives
open Microsoft.AspNetCore.WebUtilities
open Microsoft.AspNetCore.Http.Extensions

[<AutoOpen>]
module private MultipartRequest =
    let getBoundy (contentType: MediaTypeHeaderValue) (lengthLimit: int) =
        let boundary = HeaderUtilities.RemoveQuotes(contentType.Boundary)
        if System.String.IsNullOrEmpty boundary.Value then
            raise <| InvalidDataException("Missing content-type boundary.")
        elif boundary.Value.Length > lengthLimit then
            raise <| InvalidDataException(sprintf "Multiparty boundary length limit %i exceeded" lengthLimit)
        else
            boundary.Value

    let isMultipartContentType (contentType: string) =
        not(System.String.IsNullOrEmpty contentType) && contentType.IndexOf("multipart/", StringComparison.OrdinalIgnoreCase) >= 0

    let (|FormContent|FileContent|NoContent|) (section: MultipartSection) =
        match ContentDispositionHeaderValue.TryParse(StringSegment section.ContentDisposition) with
        | true, contentDisposition when not(isNull contentDisposition) ->
            if contentDisposition.IsFormDisposition()
            then FormContent contentDisposition
            else FileContent contentDisposition
        | _ ->
            NoContent


module private ExecutionHandler =
    // crypto stream that doesn't close underlying stream
    type NonClosingCryptoStream(stream, transform, mode) =
        inherit CryptoStream(stream, transform, mode)
        override this.Dispose(disposing) =
            if (not this.HasFlushedFinalBlock) then
                this.FlushFinalBlock()
            base.Dispose(false)

    let bin2hex bytes =
        let byteToChar b = char(if b > 9uy then b + 0x37uy else b + 0x30uy)
        new string [| for byte in bytes do byteToChar(byte >>> 4); byteToChar(byte &&& 0xFuy) |]

    let tryReadSection (reader: MultipartReader) = async {
        let! result = reader.ReadNextSectionAsync() |> Async.AwaitTask
        return Option.ofObj result
    }

    let tryReadForm (reader: MultipartReader) = async {
        match! tryReadSection reader with
        | Some(FormContent contentDisposition & section) ->
            let form = section.AsFormDataSection()
            let! value = form.GetValueAsync() |> Async.AwaitTask
            return Some(contentDisposition.Name.Value, value)
        | _ ->
            return None
    }

    let tryReadFile (reader: MultipartReader) = async {
        match! tryReadSection reader with
        | Some(FileContent contentDisposition & section) ->
            let fileName = WebUtility.HtmlEncode(contentDisposition.FileName.Value)
            let filepath = Path.GetTempFileName()
            let cryptoHash = SHA256.Create()
            let options = FileOptions.Asynchronous ||| FileOptions.SequentialScan
            let stream = new FileStream(filepath, FileMode.Create, FileAccess.ReadWrite, FileShare.None, 4096, options)
            use hash = new NonClosingCryptoStream(stream, cryptoHash, CryptoStreamMode.Write)
            do! section.Body.CopyToAsync(hash) |> Async.AwaitTask
            hash.FlushFinalBlock()
            stream.Position <- 0L
            let file =
                { Name = fileName; ContentType =  section.ContentType; Size = stream.Length |> int
                  Path = filepath; Content = stream; Hash = bin2hex cryptoHash.Hash }
            return Some(contentDisposition.Name.Value, file)
        | _ ->
            return None
    }

    let readFiles (reader: MultipartReader) = async {
        let rec loop files = async {
            match! tryReadFile reader with
            | Some file -> return! loop (file::files)
            | None -> return Map.ofList files
        }
        return! loop []
    }

    let readMapping (json: string) (fileMapping: Map<string, FileUpload>) =
        let bytes = System.Text.Encoding.UTF8.GetBytes(json)
        let readonlySpan =  ReadOnlySpan(bytes)
        let reader = Utf8JsonReader(readonlySpan)
        let mapping = ResizeArray()
        reader.Read() |> ignore
        match reader.TokenType with
        | JsonTokenType.StartObject ->
            reader.Read() |> ignore
            while reader.TokenType <> JsonTokenType.EndObject do
                let fileKey = reader.GetString()
                match Map.tryFind fileKey fileMapping with
                | Some file ->
                    reader.Read() |> ignore
                    match reader.TokenType with
                    | JsonTokenType.StartArray ->
                        reader.Read() |> ignore
                        while reader.TokenType <> JsonTokenType.EndArray do
                            let path = reader.GetString()
                            mapping.Add (path, file)
                            reader.Read() |> ignore
                        reader.Read() |> ignore
                    | invalidToken ->
                        let message = sprintf "Expected 'StartArray' but received json token '%A'" invalidToken
                        raise <| invalidOp message
                | None ->
                    failwithf "Mapped file %s not found in request" fileKey
            Map.ofSeq mapping
        | invalidToken ->
            let message = sprintf "Expected PropertyName or EndObject for property mapping but received json token '%A'" invalidToken
            raise <| invalidOp message

    let readOperation (cache: Json.TypeReaderCache) (executor: Executor<'T>) (element: JsonElement) (replacements:  Map<string, FileUpload>) (index: int option) =
        match element.ValueKind with
        | JsonValueKind.Object ->
            let queryText =
                match element.TryGetProperty("query") with
                | true, queryElement -> queryElement.GetString()
                | false, _ -> Introspection.IntrospectionQuery
            let variablesElement =
                match element.TryGetProperty("variables") with
                | true, value when value.ValueKind <> JsonValueKind.Null -> Some value
                |  _ -> None
            let plan = executor.CreateExecutionPlan(queryText)
            let variables = Json.readVariables cache replacements index variablesElement plan.Variables
            { ExecutionPlan = plan; Variables = variables }
        | invalidElementKind ->
            let message = sprintf "Expected 'Object' element kind but received '%A'" invalidElementKind
            raise <| invalidOp message

    let readOperations (cache: Json.TypeReaderCache) (executor: Executor<'T>) (json: string) (replacements: Map<string, FileUpload>) =
        use document = JsonDocument.Parse(json)
        match document.RootElement.ValueKind with
        | JsonValueKind.Array ->
            let operations =
                document.RootElement.EnumerateArray()
                |> Seq.mapi(fun i operation -> readOperation cache executor operation replacements (Some i))
                |> Seq.toList
            Batch operations
        | _ ->
            let operation = readOperation cache executor document.RootElement replacements None
            Single operation

    let serializeResponse (ctx: HttpContext) (serializerOptions: JsonSerializerOptions) (body: 'T) = async {
        let outputStream = ctx.Response.Body
        do! JsonSerializer.SerializeAsync(outputStream, body, serializerOptions) |> Async.AwaitTask
    }

    let processMultipartQuery (cache: Json.TypeReaderCache) (executor: Executor<'T>) (root: 'T) (ctx: HttpContext) (serializerOptions: JsonSerializerOptions) = async {
        let boundary = ctx.Request.GetMultipartBoundary()
        let reader = MultipartReader(boundary, ctx.Request.Body)
        let! operationPart = tryReadForm reader
        let! mappingPart = tryReadForm reader
        let! fileParts = readFiles reader
        match mappingPart with
        | Some("map", mappingJson) ->
            match operationPart with
            | Some("operations", operationsJson) ->
                let replacements = readMapping mappingJson fileParts
                match readOperations cache executor operationsJson replacements with
                | Single request ->
                    let! response = executor.AsyncExecute(request.ExecutionPlan, data = root, variables = request.Variables)
                    do! serializeResponse ctx serializerOptions response
                | Batch requests ->
                    let hasMutation = requests |> List.exists (fun req -> req.ExecutionPlan.Operation.OperationType = Ast.OperationType.Mutation)
                    let operations = requests |> List.map(fun req -> executor.AsyncExecute(req.ExecutionPlan, data = root, variables = req.Variables))
                    let! responses = if hasMutation then Async.Sequential operations else Async.Parallel operations
                    do! serializeResponse ctx serializerOptions responses
            | _ ->
                let message = "Expected operations as first segment of multipart request"
                return raise <| invalidOp message
        | _ ->
            let message = "Expected mapping as second segment of multipart request"
            return raise <| invalidOp message
    }

    let readBody (ctx: HttpContext) = async {
        use reader = new StreamReader(ctx.Request.Body, Text.Encoding.UTF8)
        return! reader.ReadToEndAsync() |> Async.AwaitTask
    }

    let processQuery (cache: Json.TypeReaderCache) (executor: Executor<'T>) (root: 'T) (ctx: HttpContext) (serializerOptions: JsonSerializerOptions) = async {
        let! body  = readBody ctx
        let! response =
            if System.String.IsNullOrWhiteSpace body then
                executor.AsyncExecute(Introspection.IntrospectionQuery, root)
            else
                async {
                    use document = JsonDocument.Parse(body)
                    let request = readOperation cache executor document.RootElement Map.empty None
                    return! executor.AsyncExecute(request.ExecutionPlan, data = root, variables = request.Variables)
                }
        let outputStream = ctx.Response.Body
        do! JsonSerializer.SerializeAsync(outputStream, response, serializerOptions) |> Async.AwaitTask
    }

    let queryHandler (buildArguments: HttpContext -> Async<'T>) (executor: Executor<'T>) (readerCache: Json.TypeReaderCache) =
        let graphQLSerializationOptions = JsonSerializerOptions()
        graphQLSerializationOptions.Converters.Add(Json.GQLResponseConverter())
        graphQLSerializationOptions.Converters.Add(Json.NameValueLookupConverter())
        fun (ctx: HttpContext) ->
            async {
                ctx.Response.Headers.Add("Content-Type", StringValues("application/json"))
                let! root = ctx |> buildArguments
                let execute =
                    if isMultipartContentType ctx.Request.ContentType
                    then processMultipartQuery
                    else processQuery
                do! execute readerCache executor root ctx graphQLSerializationOptions
              } |> Async.StartAsTask :> Task

[<AutoOpen>]
module ApplicationBuilderExtensions =
    type IApplicationBuilder with
        member builder.UseGraphQL(executor: Executor<'T>, buildArguments: HttpContext -> Async<'T>, ?path: string) =
            let urlPath = defaultArg path "/graphql"
            let readerCache = Json.TypeReaderCache()
            let graphQLHandler = ExecutionHandler.queryHandler buildArguments executor readerCache
            builder.Map(PathString(urlPath.TrimEnd('/')), (fun (builder:IApplicationBuilder) ->
                builder.Run(RequestDelegate(graphQLHandler))))
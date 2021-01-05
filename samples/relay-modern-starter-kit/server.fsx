#r "../../packages/Suave/lib/net40/Suave.dll"
#r "../../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "../../src/FSharp.Data.GraphQL.Server/bin/Debug/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Server/bin/Debug/FSharp.Data.GraphQL.Server.dll"

open System

let address = "127.0.0.1"
let port = 8083

// Data

type Widget = 
    { Id: string;
      Name: string }

type User = 
    { Id: string;
      Name: string;
      Widgets: Widget list }

let viewer = {
    Id = "1"
    Name = "Anonymous"
    Widgets = [
        { Id = "1"; Name = "What's it"}
        { Id = "2"; Name = "Who's it"}
        { Id = "3"; Name = "How's it"} ]}

let getUser id = if viewer.Id = id then Some viewer else None
let getWidget id = viewer.Widgets |> List.tryFind (fun w -> w.Id = id)

// Schema definition
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Relay

let rec Widget = Define.Object<Widget>(
    name = "Widget",
    description = "A shiny widget",
    interfaces = [ Node ],
    fields = [
        Define.GlobalIdField(fun _ w -> w.Id)
        Define.Field("name", String, fun _ w -> w.Name)])

and User = Define.Object<User>(
    name = "User",
    description = "A person who uses our app",
    interfaces = [ Node ],
    fields = [
        Define.GlobalIdField(fun _ w -> w.Id)
        Define.Field("name", String, fun _ w -> w.Name)
        Define.Field("widgets", ConnectionOf Widget |> Nullable, "A person's collection of widgets", Connection.allArgs,
            fun ctx user -> 
                let totalCount = user.Widgets.Length
                let widgets, hasNextPage =
                    match ctx with
                    | SliceInfo(Forward(n, after)) ->
                        match after with
                        | Some (GlobalId("Widget", id)) ->
                            let i = user.Widgets |> List.indexed |> List.pick (fun (i, e) -> if e.Id = id then Some i else None)
                            user.Widgets |> List.skip (i+1) |> List.take n,
                            i+1+n < totalCount
                        | None ->
                            user.Widgets |> List.take n,
                            n < totalCount
                        | _ -> failwithf "Cursor %A is not 'Widget' global id" after
                    | _ -> user.Widgets, false
                let edges = widgets |> Seq.map (fun b -> { Cursor = toGlobalId "Widget" (string b.Id); Node = b }) |> Seq.toArray
                let headCursor = edges |> Array.tryHead |> Option.map (fun edge -> edge.Cursor)
                let pi = { HasNextPage = hasNextPage; EndCursor = headCursor; StartCursor = None; HasPreviousPage = false }
                let con = { TotalCount = Some totalCount; PageInfo = pi; Edges = edges }
                Some con
        )])

and Node = Define.Node<obj>(fun () -> [ User; Widget ])

let Query = Define.Object("Query", [
    Define.NodeField(Node, fun ctx () id -> 
        match id with
        | GlobalId("User", i) -> getUser i |> Option.map box
        | GlobalId("Widget", i) -> getWidget i |> Option.map box
        | _ -> None)
    Define.Field("viewer", User, fun _ () -> viewer)])

let schema = Schema(query = Query, config = { SchemaConfig.Default with Types = [ User; Widget ]})
let ex = Executor(schema)

// server initialization
open Suave
open Suave.Operators
open Newtonsoft.Json
open FSharp.Data.GraphQL.Execution

let settings = JsonSerializerSettings()
settings.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
let json o = JsonConvert.SerializeObject(o, settings)

let tryParse fieldName data =
    let raw = Text.Encoding.UTF8.GetString data
    if raw <> null && raw <> ""
    then
        let map = JsonConvert.DeserializeObject<Map<string,string>>(raw)
        match Map.tryFind fieldName map with
        | Some "" -> None
        | s -> s
    else None
    
let handle : WebPart =
    fun http ->
        async {
            let tryQuery = tryParse "query" http.request.rawForm
            match tryQuery with
            | Some query ->
                let tryVariables = tryParse "variables" http.request.rawForm |> Option.map (JsonConvert.DeserializeObject<Map<string, obj>>)
                match tryVariables with
                | Some variables ->
                    printfn "Received query: %s" query
                    printfn "Recieved variables: %A" variables
                    // at the moment parser is not parsing new lines correctly, so we need to get rid of them
                    let q = query.Trim().Replace("\r\n", " ")
                    let! result = ex.AsyncExecute(q, variables=variables)
                    return! http |> Successful.OK (json result)
                | None ->
                    printfn "Received query: %s" query 
                    // at the moment parser is not parsing new lines correctly, so we need to get rid of them
                    let q = query.Trim().Replace("\r\n", " ")
                    let! result = ex.AsyncExecute(q)
                    let serialized = json result
                    return! http |> Successful.OK serialized
            | None ->
                let! schemaResult = ex.AsyncExecute(Introspection.introspectionQuery)
                return! http |> Successful.OK (json schemaResult)
        }

let serverConfig =
    { defaultConfig with
        bindings = [ HttpBinding.createSimple HTTP address port]

    }

let setCorsHeaders =
    Writers.setHeader  "Access-Control-Allow-Origin" "*"
    >=> Writers.setHeader "Access-Control-Allow-Headers" "content-type"

startWebServer serverConfig (setCorsHeaders >=> handle >=> Writers.setMimeType "application/json")
    
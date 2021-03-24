/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Execution

open System
open System.Collections.Generic
open System.Linq
open FSharp.Data.GraphQL.Extensions
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Helpers

type Error = string * obj list

type Output = IReadOnlyDictionary<string, obj>

type GQLResponse =
    { Content : GQLResponseContent
      Metadata : Metadata }
    static member Direct(data, errors, meta) =
        { Content = Direct (data, errors)
          Metadata = meta }
    static member Deferred(data, errors, deferred, meta) =
        { Content = Deferred (data, errors, deferred)
          Metadata = meta }
    static member Stream(data, meta) =
        { Content = Stream data
          Metadata = meta }
    static member Empty(meta) =
        GQLResponse.Direct(Map.empty, [], meta)
    static member Error(msg, meta) =
        GQLResponse.Direct(Map.empty, [ msg, [] ], meta)
    static member Invalid(errors, meta) =
        GQLResponse.Direct(Map.empty, errors, meta)
    static member ErrorAsync(msg, meta) =
        asyncVal { return GQLResponse.Error(msg, meta) }

and GQLResponseContent =
    | Direct of data : Output * errors: Error list
    | Deferred of data : Output * errors : Error list * defer : IObservable<Output>
    | Stream of stream : IObservable<Output>

let (|Direct|Deferred|Stream|) (response : GQLResponse) =
    match response.Content with
    | Direct (data, errors) -> Direct (data, errors)
    | Deferred (data, errors, deferred) -> Deferred (data, errors, deferred)
    | Stream data -> Stream data

/// Name value lookup used as output to be serialized into JSON.
/// It has a form of a dictionary with fixed set of keys. Values under keys
/// can be set, but no new entry can be added or removed, once lookup
/// has been initialized.
/// This dicitionay implements structural equality.
type NameValueLookup(keyValues: KeyValuePair<string, obj> seq) =
    let kvals = keyValues |> Seq.distinctBy (fun kv -> kv.Key) |> Seq.toArray
    let setValue key value =
        let mutable i = 0
        while i < kvals.Length do
            if kvals.[i].Key = key then
                kvals.[i] <- KeyValuePair<string, obj>(key, value)
                i <- Int32.MaxValue
            else i <- i+1
    let getValue key = (kvals |> Array.find (fun kv -> kv.Key = key)).Value

    let (|BoxedSeq|_|) (xs : obj) =
        match xs with
        | (:? System.Collections.IEnumerable as enumerable) -> Some (Seq.cast<obj> enumerable)
        | _ -> None

    let rec structEq (x: NameValueLookup) (y: NameValueLookup) =
        if Object.ReferenceEquals(x, y) then true
        elif Object.ReferenceEquals(y, null) then false
        elif Object.ReferenceEquals(x, null) then false
        elif x.Count <> y.Count then false
        else
            x.Buffer
            |> Array.forall2 (fun (a: KeyValuePair<string, obj>) (b: KeyValuePair<string, obj>) ->
                if a.Key <> b.Key then false
                else
                    match a.Value, b.Value with
                    | (:? NameValueLookup as x), (:? NameValueLookup as y) -> structEq x y
                    | (BoxedSeq x), (BoxedSeq y) ->
                        if Seq.length x <> Seq.length y then false else Seq.forall2 (=) x y
                    | a1, b1 -> a1 = b1) y.Buffer
    let pad (sb: System.Text.StringBuilder) times =
        for _ in 0..times do sb.Append("\t") |> ignore
    let rec stringify (sb: System.Text.StringBuilder) deep (o:obj) =
        match o with
        | :? NameValueLookup as lookup ->
            if lookup.Count > 0 then
                sb.Append("{ ") |> ignore
                lookup.Buffer
                |> Array.iter (fun kv ->
                    sb.Append(kv.Key).Append(": ") |> ignore
                    stringify sb (deep+1) kv.Value
                    sb.Append(",\r\n") |> ignore
                    pad sb deep)
                sb.Remove(sb.Length - 4 - deep, 4 + deep).Append(" }") |> ignore
        | :? string as s ->
            sb.Append("\"").Append(s).Append("\"") |> ignore
        | :? System.Collections.IEnumerable as s ->
            sb.Append("[") |> ignore
            for i in s do
                stringify sb (deep + 1) i
                sb.Append(", ") |> ignore
            sb.Append("]") |> ignore
        | other ->
            if isNull other |> not
            then sb.Append(other.ToString()) |> ignore
            else sb.Append("null") |> ignore
        ()
    /// Returns raw content of the current lookup.
    member __.Buffer : KeyValuePair<string, obj> [] = kvals
    /// Return a number of entries stored in current lookup. It's fixed size.
    member __.Count = kvals.Length
    member __.Item
        with get (key) = getValue key
        and  set (key) value = setValue key value
    /// Updates an entry's value under given key. It will throw an exception
    /// if provided key cannot be found in provided lookup.
    member __.Update key value = setValue key value
    override x.Equals(other) =
        match other with
        | :? NameValueLookup as lookup -> structEq x lookup
        | _ -> false
    override __.GetHashCode() =
        let mutable hash = 0
        for kv in kvals do
            hash <- (hash*397) ^^^ (kv.Key.GetHashCode()) ^^^ (if isNull kv.Value then 0 else kv.Value.GetHashCode())
        hash
    override x.ToString() =
        let sb =Text.StringBuilder()
        stringify sb 1 x
        sb.ToString()
    interface IEquatable<NameValueLookup> with
        member x.Equals(other) = structEq x other
    interface System.Collections.IEnumerable with
        member __.GetEnumerator() = (kvals :> System.Collections.IEnumerable).GetEnumerator()
    interface IEnumerable<KeyValuePair<string, obj>> with
        member __.GetEnumerator() = (kvals :> IEnumerable<KeyValuePair<string, obj>>).GetEnumerator()
    interface IReadOnlyDictionary<string, obj> with
        member __.ContainsKey(key) = kvals |> Array.exists (fun kv -> kv.Key = key)
        member x.Count = x.Count
        member __.Item
            with get (key) = getValue key
        member __.Keys = upcast (kvals |> Array.map (fun kv -> kv.Key))
        member __.Values = upcast (kvals |> Array.map (fun kv -> kv.Value))
        member __.TryGetValue(key, value) =
            match kvals |> Array.tryFind (fun kv -> kv.Key = key) with
            | Some kv -> value <- kv.Value; true
            | None -> value <- null; false
    new(t: (string * obj) list) =
        NameValueLookup(t |> List.map (fun (k, v) -> KeyValuePair<string,obj>(k, v)) |> List.toArray)
    new(t: string []) =
        NameValueLookup(t |> Array.map (fun k -> KeyValuePair<string,obj>(k, null)))

module NameValueLookup =
    /// Create new NameValueLookup from given list of key-value tuples.
    let ofList (l: (string * obj) list) = NameValueLookup(l)

let private collectDefaultArgValue acc (argdef: InputFieldDef) =
    match argdef.DefaultValue with
    | Some defVal -> Map.add argdef.Name defVal acc
    | None -> acc

let internal argumentValue variables (argdef: InputFieldDef) (argument: Argument) =
    match argdef.ExecuteInput argument.Value variables  with
    | null -> argdef.DefaultValue
    | value -> Some value

let private getArgumentValues (argDefs: InputFieldDef []) (args: Argument list) (variables: Map<string, obj>) : Map<string, obj> =
    argDefs
    |> Array.fold (fun acc argdef ->
        match List.tryFind (fun (a: Argument) -> a.Name = argdef.Name) args with
        | Some argument ->
            match argumentValue variables argdef argument with
            | Some v -> Map.add argdef.Name v acc
            | None -> acc
        | None -> collectDefaultArgValue acc argdef
    ) Map.empty

let private getOperation = function
    | OperationDefinition odef -> Some odef
    | _ -> None

let internal findOperation doc opName =
    match doc.Definitions |> List.choose getOperation, opName with
    | [def], _ -> Some def
    | defs, name ->
        defs
        |> List.tryFind (fun def -> def.Name = name)

let private defaultResolveType possibleTypesFn abstractDef : obj -> ObjectDef =
    let possibleTypes = possibleTypesFn abstractDef
    let mapper = match abstractDef with Union u -> u.ResolveValue | _ -> id
    fun value ->
        let mapped = mapper value
        possibleTypes
        |> Array.find (fun objdef ->
            match objdef.IsTypeOf with
            | Some isTypeOf -> isTypeOf mapped
            | None -> false)

let private resolveInterfaceType possibleTypesFn (interfacedef: InterfaceDef) =
    match interfacedef.ResolveType with
    | Some resolveType -> resolveType
    | None -> defaultResolveType possibleTypesFn interfacedef

let private resolveUnionType possibleTypesFn (uniondef: UnionDef) =
    match uniondef.ResolveType with
    | Some resolveType -> resolveType
    | None -> defaultResolveType possibleTypesFn uniondef

let private createFieldContext objdef argDefs ctx (info: ExecutionInfo) =
    let fdef = info.Definition
    let args = getArgumentValues argDefs info.Ast.Arguments ctx.Variables
    { ExecutionInfo = info
      Context = ctx.Context
      ReturnType = fdef.TypeDef
      ParentType = objdef
      Schema = ctx.Schema
      Args = args
      Variables = ctx.Variables }

let private resolveField (execute: ExecuteField) (ctx: ResolveFieldContext) (parentValue: obj) =
    if ctx.ExecutionInfo.IsNullable
    then
        execute ctx parentValue
        |> AsyncVal.map(optionCast)
    else
        execute ctx parentValue
        |> AsyncVal.map(fun v -> if isNull v then None else Some v)

type ResolverResult<'T> = Result<'T * IObservable<Output> option * Error list, Error list>

module ResolverResult =
    let mapValue (f : 'T -> 'U) (r : ResolverResult<'T>) : ResolverResult<'U> =
        Result.map(fun (data, deferred, errs) -> (f data, deferred, errs)) r

type StreamOutput =
    | NonList of (KeyValuePair<string, obj> * Error list)
    | NonBufferedList of int * (KeyValuePair<string, obj> * Error list)
    | BufferedList of int list * (KeyValuePair<string, obj> * Error list) list

let private raiseError err = AsyncVal.wrap <| Error [err]

let private resolverError path ctx e = (ctx.Schema.ParseError e, List.rev path)
let private nullResolverError name path ctx = resolverError path ctx (GraphQLException <| sprintf "Non-Null field %s resolved as a null!" name)
let private coercionError value tyName path ctx = resolverError path ctx (GraphQLException <| sprintf "Value '%O' could not be coerced to scalar %s" value tyName)
let private interfaceImplError ifaceName tyName path ctx = resolverError path ctx (GraphQLException <| sprintf "GraphQL Interface '%s' is not implemented by the type '%s'" ifaceName tyName)
let private unionImplError unionName tyName path ctx = resolverError path ctx (GraphQLException (sprintf "GraphQL Union '%s' is not implemented by the type '%s'" unionName tyName))
let private deferredNullableError name tyName path ctx = resolverError path ctx (GraphQLException (sprintf "Deferred field %s of type '%s' must be nullable" name tyName))
let private streamListError name tyName path ctx = resolverError path ctx (GraphQLException (sprintf "Streamed field %s of type '%s' must be list" name tyName))

let private resolved name v : AsyncVal<ResolverResult<KeyValuePair<string, obj>>> = AsyncVal.wrap <| Ok(KeyValuePair(name, box v), None, [])


let buildDeferredResult (data : obj) (errs : Error list) (path : obj list) : Output =
    let formattedErrors =
        errs
        |> List.map(fun (msg, path) -> NameValueLookup.ofList ["message", upcast msg; "path", upcast path])
    let formattedPath = List.rev path
    match formattedErrors with
    | [] -> upcast NameValueLookup.ofList [ "data", data; "path", upcast formattedPath ]
    | _ -> upcast NameValueLookup.ofList [ "data", data; "errors", upcast formattedErrors; "path", upcast formattedPath ]

let deferResults path (res : ResolverResult<obj>) : IObservable<Output> =
    match res with
    | Ok (data, deferred, errs) ->
        let deferredData = Observable.singleton <| buildDeferredResult data errs path
        Option.fold Observable.concat2 deferredData deferred
    | Error errs -> Observable.singleton <| buildDeferredResult null errs path

/// Collect together an array of results using the appropriate execution strategy.
let collectFields (strategy : ExecutionStrategy) (rs : AsyncVal<ResolverResult<KeyValuePair<string, obj>>> []) : AsyncVal<ResolverResult<KeyValuePair<string, obj> []>> = asyncVal {

        let! collected =
            match strategy with
            | Parallel -> AsyncVal.collectParallel rs
            | Sequential -> AsyncVal.collectSequential rs

        let data = Array.zeroCreate (collected.Length)

        let merge r acc =
            match (r, acc) with
            | Ok(field, d, e), Ok(i, deferred, errs) ->
                Array.set data i field
                Ok(i - 1, Option.mergeWith Observable.merge2 deferred d, e @ errs)
            | Error e, Ok (_, _, errs) -> Error (e @ errs)
            | Ok (_, _, e), Error errs -> Error (e @ errs)
            | Error e, Error errs -> Error (e @ errs)
        return
            Array.foldBack merge collected (Ok (data.Length - 1, None, []))
            |> ResolverResult.mapValue(fun _ -> data)
    }

let rec private direct (returnDef : OutputDef) (ctx : ResolveFieldContext) (path : obj list) (parent : obj) (value : obj) : AsyncVal<ResolverResult<KeyValuePair<string, obj>>> =
    let name = ctx.ExecutionInfo.Identifier
    match returnDef with
    | Object objDef ->
        let fields =
            match ctx.ExecutionInfo.Kind with
            | SelectFields fields -> fields
            | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind
        executeObjectFields fields name objDef ctx path value
    | Scalar scalarDef ->
        match scalarDef.CoerceValue value with
        | Some v' -> resolved name v'
        | None -> raiseError <| coercionError value scalarDef.Name path ctx
    | Enum enumDef ->
        let enumCase = enumDef.Options |> Array.tryPick(fun case -> if case.Value.Equals(value) then coerceStringValue value else None)
        match enumCase with
        | Some v' -> resolved name (v' :> obj)
        | None -> raiseError <| coercionError value enumDef.Name path  ctx
    | List (Output innerDef) ->
        let innerCtx =
            match ctx.ExecutionInfo.Kind with
            | ResolveCollection innerPlan -> { ctx with ExecutionInfo = { innerPlan with ReturnDef = innerDef } }
            | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind
        let resolveItem index item =
            executeResolvers innerCtx (box index :: path) value (toOption item |> AsyncVal.wrap)
        match value with
        | :? System.Collections.IEnumerable as enumerable ->
            enumerable
            |> Seq.cast<obj>
            |> Seq.toArray
            |> Array.mapi resolveItem
            |> collectFields Parallel
            |> AsyncVal.map(ResolverResult.mapValue(fun items -> KeyValuePair(name, items |> Array.map(fun d -> d.Value) |> box)))
        | _ -> raise <| GraphQLException (sprintf "Expected to have enumerable value in field '%s' but got '%O'" ctx.ExecutionInfo.Identifier (value.GetType()))
    | Nullable (Output innerDef) ->
        let innerCtx = { ctx with ExecutionInfo = { ctx.ExecutionInfo with IsNullable = true; ReturnDef = innerDef } }
        executeResolvers innerCtx path parent (toOption value |> AsyncVal.wrap)
        |> AsyncVal.map(Result.catchError (fun errs -> (KeyValuePair(name, null), None, errs)) >> Ok)
    | Interface iDef ->
        let possibleTypesFn = ctx.Schema.GetPossibleTypes
        let resolver = resolveInterfaceType possibleTypesFn iDef
        let resolvedDef = resolver value
        let typeMap =
            match ctx.ExecutionInfo.Kind with
            | ResolveAbstraction typeMap -> typeMap
            | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind
        match Map.tryFind resolvedDef.Name typeMap with
        | Some fields -> executeObjectFields fields name resolvedDef ctx path value
        | None -> raiseError <| interfaceImplError iDef.Name resolvedDef.Name path ctx
    | Union uDef ->
        let possibleTypesFn = ctx.Schema.GetPossibleTypes
        let resolver = resolveUnionType possibleTypesFn uDef
        let resolvedDef = resolver value
        let typeMap =
            match ctx.ExecutionInfo.Kind with
            | ResolveAbstraction typeMap -> typeMap
            | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind
        match Map.tryFind resolvedDef.Name typeMap with
        | Some fields -> executeObjectFields fields name resolvedDef ctx path (uDef.ResolveValue value)
        | None -> raiseError <| unionImplError uDef.Name resolvedDef.Name path ctx
    | _ -> failwithf "Unexpected value of returnDef: %O" returnDef

and deferred (ctx : ResolveFieldContext) (path : obj list) (parent : obj) (value : obj) =
    let info = ctx.ExecutionInfo
    let name = info.Identifier
    let deferred =
        executeResolvers ctx path parent (toOption value |> AsyncVal.wrap)
        |> Observable.ofAsyncVal
        |> Observable.bind(ResolverResult.mapValue(fun d -> d.Value) >> deferResults path)
    AsyncVal.wrap <| Ok(KeyValuePair(info.Identifier, null), Some deferred, [])

and private streamed (options : BufferedStreamOptions) (innerDef : OutputDef) (ctx : ResolveFieldContext) (path : obj list) (parent : obj) (value : obj) =
    let info = ctx.ExecutionInfo
    let name = info.Identifier
    let innerCtx =
        match info.Kind with
        | ResolveCollection innerPlan -> { ctx with ExecutionInfo = innerPlan }
        | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind

    let collectBuffered : (int * ResolverResult<KeyValuePair<string, obj>>) list -> IObservable<Output> = function
        | [] -> Observable.empty
        | [(index, result)] ->
            result
            |> ResolverResult.mapValue(fun d -> box [|d.Value|])
            |> deferResults (box index :: path)
        | chunk ->
            let data = Array.zeroCreate (chunk.Length)
            let merge (index, r : ResolverResult<KeyValuePair<string, obj>>) (i, indicies, deferred, errs) =
                match r with
                | Ok (item, d, e) ->
                    Array.set data i item.Value
                    (i - 1, box index :: indicies, Option.mergeWith Observable.merge2 d deferred, e @ errs)
                | Error e -> (i - 1, box index :: indicies, deferred, e @ errs)
            let (_, indicies, deferred, errs) = List.foldBack merge chunk (chunk.Length - 1, [], None, [])
            deferResults (box indicies :: path) (Ok (box data, deferred, errs))

    let buffer (items : IObservable<int * ResolverResult<KeyValuePair<string, obj>>>) : IObservable<Output> =
        let buffered =
            match options.Interval, options.PreferredBatchSize with
            | Some i, None -> Observable.bufferByTiming i items
            | None, Some c -> Observable.bufferByElementCount c items
            | Some i, Some c -> Observable.bufferByTimingAndElementCount i c items
            | None, None -> Observable.map(List.singleton) items
        buffered
        |> Observable.bind collectBuffered

    let resolveItem index item = asyncVal {
            let! result = executeResolvers innerCtx (box index :: path) parent (toOption item |> AsyncVal.wrap)
            return (index, result)
        }

    match value with
    | :? System.Collections.IEnumerable as enumerable ->
        let stream =
            enumerable
            |> Seq.cast<obj>
            |> Seq.toArray
            |> Array.mapi resolveItem
            |> Observable.ofAsyncValSeq
            |> buffer
        AsyncVal.wrap <| Ok(KeyValuePair(info.Identifier, box [||]), Some stream, [])
    | _ -> raise <| GraphQLException (sprintf "Expected to have enumerable value in field '%s' but got '%O'" ctx.ExecutionInfo.Identifier (value.GetType()))

and private live (ctx : ResolveFieldContext) (path : obj list) (parent : obj) (value : obj) =
    let info = ctx.ExecutionInfo
    let name = info.Identifier

    let rec getObjectName = function
        | Object objDef -> objDef.Name
        | Scalar scalarDef -> scalarDef.Name
        | Enum enumDef -> enumDef.Name
        | Nullable (Output innerDef) -> getObjectName innerDef
        | Interface iDef -> iDef.Name
        | Union uDef ->
            let resolver = resolveUnionType ctx.Schema.GetPossibleTypes uDef
            getObjectName (resolver value)
        | returnDef -> failwithf "Unexpected value of returnDef: %O" returnDef

    let typeName = getObjectName info.ParentDef

    /// So the updatedValue here is actually the fresh parent.
    let resolveUpdate updatedValue =
        executeResolvers ctx path parent (updatedValue |> Some |> AsyncVal.wrap)
        |> AsyncVal.map(ResolverResult.mapValue(fun d -> d.Value) >> deferResults path)
        |> Observable.ofAsyncVal
        |> Observable.merge

    let provider = ctx.Schema.LiveFieldSubscriptionProvider
    let filter = provider.TryFind typeName name |> Option.map (fun x -> x.Filter)
    let updates =
        match filter with
        | Some filterFn -> provider.Add (filterFn parent) typeName name |> Observable.bind resolveUpdate
        | None -> failwithf "No live provider for %s:%s" typeName name

    executeResolvers ctx path parent (value |> Some |> AsyncVal.wrap)
    |> AsyncVal.map(Result.map(fun (data, deferred, errs) -> (data, Some <| Option.fold Observable.merge2 updates deferred, errs)))

/// Actually execute the resolvers.
and private executeResolvers (ctx : ResolveFieldContext) (path : obj list) (parent : obj) (value : AsyncVal<obj option>) : AsyncVal<ResolverResult<KeyValuePair<string, obj>>> =
    let info = ctx.ExecutionInfo
    let name = info.Identifier
    let returnDef = info.ReturnDef

    let rec innerListDef = function
        | Nullable (Output innerDef) -> innerListDef innerDef
        | List (Output innerDef) -> Some innerDef
        | _ -> None

    let (|HasList|_|) = innerListDef

    /// Run a resolution strategy with the provided context.
    /// This handles all null resolver errors/error propagation.
    let resolveWith (ctx : ResolveFieldContext) (onSuccess : ResolveFieldContext -> obj list -> obj -> obj -> AsyncVal<ResolverResult<KeyValuePair<string, obj>>>) : AsyncVal<ResolverResult<KeyValuePair<string, obj>>> = asyncVal {
            let! resolved =
                value
                |> AsyncVal.map Ok
                |> AsyncVal.rescue(resolverError path ctx >> List.singleton >> Error)
            match resolved with
            | Error errs when ctx.ExecutionInfo.IsNullable -> return Ok (KeyValuePair(name, null), None, errs)
            | Ok None when ctx.ExecutionInfo.IsNullable -> return Ok (KeyValuePair(name, null), None, [])
            | Error errs -> return Error errs
            | Ok None -> return Error [nullResolverError name path ctx]
            | Ok (Some v) -> return! onSuccess ctx path parent v
        }

    match info.Kind, returnDef with
    | ResolveDeferred innerInfo, _ when innerInfo.IsNullable -> // We can only defer nullable fields
        deferred
        |> resolveWith { ctx with ExecutionInfo = { innerInfo with IsNullable = false } }
    | ResolveDeferred innerInfo, _ ->
        raiseError <| deferredNullableError (innerInfo.Identifier) (innerInfo.ReturnDef.ToString()) path ctx
    | ResolveStreamed (innerInfo, mode), HasList innerDef -> // We can only stream lists
        streamed mode innerDef
        |> resolveWith { ctx with ExecutionInfo = innerInfo; }
    | ResolveStreamed (innerInfo, _), _ ->
        raiseError <| streamListError innerInfo.Identifier (returnDef.ToString()) path ctx
    | ResolveLive innerInfo, _ ->
        live
        |> resolveWith { ctx with ExecutionInfo = innerInfo }
    | _ ->
        direct returnDef
        |> resolveWith ctx


and executeObjectFields (fields : ExecutionInfo list) (objName : string) (objDef : ObjectDef) (ctx : ResolveFieldContext) (path : obj list) (value : obj) : AsyncVal<ResolverResult<KeyValuePair<string, obj>>> = asyncVal {
        let executeField field =
            let argDefs = ctx.Context.FieldExecuteMap.GetArgs(objDef.Name, field.Definition.Name)
            let resolver = ctx.Context.FieldExecuteMap.GetExecute(objDef.Name, field.Definition.Name)
            let fieldCtx = createFieldContext objDef argDefs ctx field
            let fieldPath = (field.Identifier :> obj :: path)
            executeResolvers fieldCtx fieldPath value (resolveField resolver fieldCtx value)
        let! res =
            fields
            |> List.toArray
            |> Array.map executeField
            |> collectFields Parallel
        match res with
        | Error errs -> return Error errs
        | Ok(kvps, def, errs) -> return Ok (KeyValuePair(objName, box <| NameValueLookup(kvps)), def, errs)
    }

let internal compileSubscriptionField (subfield: SubscriptionFieldDef) =
    match subfield.Resolve with
    | Resolve.BoxedFilterExpr(_, _, _, filter) -> fun ctx a b -> filter ctx a b |> AsyncVal.wrap |> AsyncVal.toAsync
    | Resolve.BoxedAsyncFilterExpr(_, _, _, filter) -> filter
    | _ -> raise <| GraphQLException ("Invalid filter expression for subscription field!")

let internal compileField (fieldDef: FieldDef) : ExecuteField =
    match fieldDef.Resolve with
    | Resolve.BoxedSync(_, _, resolve) ->
        fun resolveFieldCtx value ->
            try
                resolve resolveFieldCtx value
                |> AsyncVal.wrap
            with e -> AsyncVal.Failure(e)

    | Resolve.BoxedAsync(_, _, resolve) ->
        fun resolveFieldCtx value ->
            asyncVal {
                return! resolve resolveFieldCtx value
            }
    | Resolve.BoxedExpr (resolve) ->
        fun resolveFieldCtx value ->
            downcast resolve resolveFieldCtx value
    | _ ->
        fun _ _ -> raise (InvalidOperationException(sprintf "Field '%s' has been accessed, but no resolve function for that field definition was provided. Make sure, you've specified resolve function or declared field with Define.AutoField method" fieldDef.Name))

let private (|String|Other|) (o : obj) =
    match o with
    | :? string as s -> String s
    | _ -> Other

let private executeQueryOrMutation (resultSet: (string * ExecutionInfo) []) (ctx: ExecutionContext) (objdef: ObjectDef) (rootValue : obj) : AsyncVal<GQLResponse> =
    let executeRootOperation (name, info) =
        let fDef = info.Definition
        let argDefs = ctx.FieldExecuteMap.GetArgs(ctx.ExecutionPlan.RootDef.Name, info.Definition.Name)
        let args = getArgumentValues argDefs info.Ast.Arguments ctx.Variables
        let path = [info.Identifier :> obj]
        let fieldCtx =
            { ExecutionInfo = info
              Context = ctx
              ReturnType = fDef.TypeDef
              ParentType = objdef
              Schema = ctx.Schema
              Args = args
              Variables = ctx.Variables }
        let execute = ctx.FieldExecuteMap.GetExecute(ctx.ExecutionPlan.RootDef.Name, info.Definition.Name)
        asyncVal {
            let! result =
                executeResolvers fieldCtx path rootValue (resolveField execute fieldCtx rootValue)
                |> AsyncVal.rescue (resolverError path fieldCtx >> List.singleton >> Error)
            match result with
            | Error errs when info.IsNullable -> return Ok(KeyValuePair(name, null), None, errs)
            | Error errs -> return Error errs
            | Ok r -> return Ok r
        }

    asyncVal {
        match! resultSet |> Array.map executeRootOperation |> collectFields ctx.ExecutionPlan.Strategy with
        | Ok (data, Some deferred, errs) -> return GQLResponse.Deferred(NameValueLookup(data), errs, deferred, ctx.Metadata)
        | Ok (data, None, errs) -> return GQLResponse.Direct(NameValueLookup(data), errs, ctx.Metadata)
        | Error errs -> return GQLResponse.Direct(null, errs, ctx.Metadata)
    }

let private executeSubscription (resultSet: (string * ExecutionInfo) []) (ctx: ExecutionContext) (objdef: SubscriptionObjectDef) value =
    // Subscription queries can only have one root field
    let nameOrAlias, info = Array.head resultSet
    let subdef = info.Definition :?> SubscriptionFieldDef
    let args = getArgumentValues subdef.Args info.Ast.Arguments ctx.Variables
    let returnType = subdef.OutputTypeDef
    let fieldCtx =
        { ExecutionInfo = info
          Context = ctx
          ReturnType = returnType
          ParentType = objdef
          Schema = ctx.Schema
          Args = args
          Variables = ctx.Variables }
    let onValue v = asyncVal {
            match! executeResolvers fieldCtx [ info.Identifier ] value (toOption v |> AsyncVal.wrap) with
            | Ok (data, None, []) -> return NameValueLookup.ofList["data", box <| NameValueLookup.ofList [nameOrAlias, data.Value]] :> Output
            | Ok (data, None, errs) -> return NameValueLookup.ofList["data", box <| NameValueLookup.ofList [nameOrAlias, data.Value]; "errors", upcast errs] :> Output
            | Ok (_, Some _, _) -> return failwithf "Deferred/Streamed/Live are not supported for subscriptions!"
            | Error errs -> return NameValueLookup.ofList["data", null; "errors", upcast errs] :> Output
        }
    ctx.Schema.SubscriptionProvider.Add fieldCtx value subdef
    |> Observable.bind(onValue >> Observable.ofAsyncVal)

let private compileInputObject (indef: InputObjectDef) =
    indef.Fields
    |> Array.iter(fun input ->
        let errMsg = sprintf "Input object '%s': in field '%s': " indef.Name input.Name
        input.ExecuteInput <- compileByType errMsg input.TypeDef)

let private compileObject (objdef: ObjectDef) (executeFields: FieldDef -> unit) =
    objdef.Fields
    |> Map.iter (fun _ fieldDef ->
        executeFields fieldDef
        fieldDef.Args
        |> Array.iter (fun arg ->
            let errMsg = sprintf "Object '%s': field '%s': argument '%s': " objdef.Name fieldDef.Name arg.Name
            arg.ExecuteInput <- compileByType errMsg arg.TypeDef))

let internal compileSchema (ctx : SchemaCompileContext) =
    ctx.Schema.TypeMap.ToSeq()
    |> Seq.iter (fun (tName, x) ->
        match x with
        | SubscriptionObject subdef ->
            compileObject subdef (fun sub ->
                let filter =
                    match sub with
                    | :? SubscriptionFieldDef as subField -> compileSubscriptionField subField
                    | _ -> failwithf "Schema error: subscription object '%s' does have a field '%s' that is not a subscription field definition." subdef.Name sub.Name
                ctx.Schema.SubscriptionProvider.Register { Name = sub.Name; Filter = filter })
        | Object objdef ->
            compileObject objdef (fun fieldDef -> ctx.FieldExecuteMap.SetExecute(tName, fieldDef))
        | InputObject indef -> compileInputObject indef
        | _ -> ())

let internal coerceVariables (variables: VarDef list) (vars: Map<string, obj>) =
    variables
    |> List.fold (fun acc vardef -> Map.add vardef.Name (coerceVariable vardef vars) acc) Map.empty

let internal executeOperation (ctx : ExecutionContext) : AsyncVal<GQLResponse> =
    let resultSet =
        ctx.ExecutionPlan.Fields
        |> List.filter (fun info -> info.Include ctx.Variables)
        |> List.map (fun info -> (info.Identifier, info))
        |> List.toArray
    match ctx.ExecutionPlan.Operation.OperationType with
    | Query -> executeQueryOrMutation resultSet ctx ctx.Schema.Query ctx.RootValue
    | Mutation ->
        match ctx.Schema.Mutation with
        | Some m -> executeQueryOrMutation resultSet ctx m ctx.RootValue
        | None -> raise(InvalidOperationException("Attempted to make a mutation but no mutation schema was present!"))
    | Subscription ->
        match ctx.Schema.Subscription with
        | Some s ->
            AsyncVal.wrap(GQLResponse.Stream(executeSubscription resultSet ctx s ctx.RootValue, ctx.Metadata))
        | None -> raise(InvalidOperationException("Attempted to make a subscription but no subscription schema was present!"))

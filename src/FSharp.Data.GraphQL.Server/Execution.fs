/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Execution

open System
open System.Reflection
open System.Runtime.InteropServices;
open System.Collections.Generic
open System.Collections.Concurrent
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Planning
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Reflection.FSharpReflectionExtensions

/// Name value lookup used as output to be serialized into JSON.
/// It has a form of a dictionary with fixed set of keys. Values under keys
/// can be set, but no new entry can be added or removed, once lookup
/// has been initialized.
/// This dicitionay implements structural equality.
type NameValueLookup(keyValues: KeyValuePair<string, obj> []) =
    let kvals = keyValues |> Array.distinctBy (fun kv -> kv.Key)
    let setValue key value =
        let mutable i = 0
        while i < kvals.Length do
            if kvals.[i].Key = key then
                kvals.[i] <- KeyValuePair<string, obj>(key, value) 
                i <- Int32.MaxValue
            else i <- i+1
    let getValue key = (kvals |> Array.find (fun kv -> kv.Key = key)).Value
    let rec structEq (x: NameValueLookup) (y: NameValueLookup) =
        if Object.ReferenceEquals(x, y) then true
        elif Object.ReferenceEquals(y, null) then false
        elif x.Count <> y.Count then false
        else
            x.Buffer
            |> Array.forall2 (fun (a: KeyValuePair<string, obj>) (b: KeyValuePair<string, obj>) ->
                if a.Key <> b.Key then false
                else 
                    match a.Value, b.Value with
                    | :? NameValueLookup, :? NameValueLookup as o -> structEq (downcast fst o) (downcast snd o)
                    | :? seq<obj>, :? seq<obj> -> Seq.forall2 (=) (a.Value :?> seq<obj>) (b.Value :?> seq<obj>)
                    | a1, b1 -> a1 = b1) y.Buffer
    let pad (sb: System.Text.StringBuilder) times = for i in 0..times do sb.Append("\t") |> ignore
    let rec stringify (sb: System.Text.StringBuilder) deep (o:obj) =
        match o with
        | :? NameValueLookup as lookup ->
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
                stringify sb (deep+1) i
                sb.Append(", ") |> ignore
            sb.Append("]") |> ignore
        | other -> 
            if other <> null 
            then sb.Append(other.ToString()) |> ignore
            else sb.Append("null") |> ignore
        ()
    /// Create new NameValueLookup from given list of key-value tuples.
    static member ofList (l: (string * obj) list) = NameValueLookup(l)
    /// Returns raw content of the current lookup.
    member private x.Buffer : KeyValuePair<string, obj> [] = kvals
    /// Return a number of entries stored in current lookup. It's fixed size.
    member x.Count = kvals.Length
    /// Updates an entry's value under given key. It will throw an exception
    /// if provided key cannot be found in provided lookup.
    member x.Update key value = setValue key value
    override x.Equals(other) = 
        match other with
        | :? NameValueLookup as lookup -> structEq x lookup
        | _ -> false
    override x.GetHashCode() =
        let mutable hash = 0
        for kv in kvals do
            hash <- (hash*397) ^^^ (kv.Key.GetHashCode()) ^^^ (if kv.Value = null then 0 else kv.Value.GetHashCode())
        hash
    override x.ToString() = 
        let sb =Text.StringBuilder()
        stringify sb 1 x
        sb.ToString()
    interface IEquatable<NameValueLookup> with
        member x.Equals(other) = structEq x other
    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (kvals :> System.Collections.IEnumerable).GetEnumerator()
    interface IEnumerable<KeyValuePair<string, obj>> with
        member x.GetEnumerator() = (kvals :> IEnumerable<KeyValuePair<string, obj>>).GetEnumerator()
    interface IDictionary<string, obj> with
        member x.Add(_, _) = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member x.Add(_) = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member x.Clear() = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member x.Contains(item) = kvals |> Array.exists ((=) item)
        member x.ContainsKey(key) = kvals |> Array.exists (fun kv -> kv.Key = key)
        member x.CopyTo(array, arrayIndex) = kvals.CopyTo(array, arrayIndex)
        member x.Count = x.Count
        member x.IsReadOnly = true
        member x.Item
            with get (key) = getValue key
            and set (key) v = setValue key v
        member x.Keys = upcast (kvals |> Array.map (fun kv -> kv.Key))
        member x.Values = upcast (kvals |> Array.map (fun kv -> kv.Value))
        member x.Remove(_:string) = 
            raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
            false
        member x.Remove(_:KeyValuePair<string,obj>) = 
            raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
            false
        member x.TryGetValue(key, value) = 
            match kvals |> Array.tryFind (fun kv -> kv.Key = key) with
            | Some kv -> value <- kv.Value; true
            | None -> value <- null; false
    new(t: (string * obj) list) = 
        NameValueLookup(t |> List.map (fun (k, v) -> KeyValuePair<string,obj>(k, v)) |> List.toArray)
    new(t: string []) = 
        NameValueLookup(t |> Array.map (fun k -> KeyValuePair<string,obj>(k, null)))
                
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
                
let rec private createCompletion (possibleTypesFn: TypeDef -> ObjectDef []) (returnDef: OutputDef) (fieldExecuteMap: FieldExecuteMap): ResolveFieldContext -> obj -> AsyncVal<obj> =
    match returnDef with
    | Object objdef -> 
        fun (ctx: ResolveFieldContext) value -> 
            match ctx.ExecutionInfo.Kind with
            | SelectFields fields -> executeFields objdef ctx value fields fieldExecuteMap
            | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind 
    
    | Scalar scalardef ->
        let (coerce: obj -> obj option) = scalardef.CoerceValue
        fun _ value -> 
            coerce value
            |> Option.toObj
            |> AsyncVal.wrap
    
    | List (Output innerdef) ->
        let (innerfn: ResolveFieldContext -> obj -> AsyncVal<obj>) = createCompletion possibleTypesFn innerdef fieldExecuteMap
        fun ctx (value: obj) ->
            let innerCtx =
                match ctx.ExecutionInfo.Kind with
                | ResolveCollection innerPlan -> { ctx with ExecutionInfo = innerPlan }
                | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind 
            match value with
            | :? string as s -> 
                innerfn innerCtx (s)
                |> AsyncVal.map (fun x -> upcast [| x |])
            | :? System.Collections.IEnumerable as enumerable ->
                let completed =
                    enumerable
                    |> Seq.cast<obj>
                    |> Seq.map (fun x -> innerfn innerCtx x)
                    |> Seq.toArray
                    |> AsyncVal.collectParallel
                    |> AsyncVal.map box
                completed
            | _ -> raise <| GraphQLException (sprintf "Expected to have enumerable value in field '%s' but got '%O'" ctx.ExecutionInfo.Identifier (value.GetType()))
    
    | Nullable (Output innerdef) ->
        let innerfn = createCompletion possibleTypesFn innerdef fieldExecuteMap
        let optionDef = typedefof<option<_>>
        fun ctx value ->
            let t = value.GetType()
            if value = null then AsyncVal.empty
#if NETSTANDARD1_6           
            elif t.GetTypeInfo().IsGenericType && t.GetTypeInfo().GetGenericTypeDefinition() = optionDef then
#else       
            elif t.IsGenericType && t.GetGenericTypeDefinition() = optionDef then
#endif                  
                let _, fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, t)
                innerfn ctx fields.[0]
            else innerfn ctx value
    
    | Interface idef ->
        let resolver = resolveInterfaceType possibleTypesFn idef
        fun ctx value -> 
            let resolvedDef = resolver value
            let typeMap =
                match ctx.ExecutionInfo.Kind with
                | ResolveAbstraction typeMap -> typeMap
                | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind 
            match Map.tryFind resolvedDef.Name typeMap with
            | Some fields -> executeFields resolvedDef ctx value fields fieldExecuteMap
            | None -> raise(GraphQLException (sprintf "GraphQL interface '%s' is not implemented by the type '%s'" idef.Name resolvedDef.Name))   
        
    | Union udef ->
        let resolver = resolveUnionType possibleTypesFn udef
        fun ctx value ->
            let resolvedDef = resolver value
            let typeMap =
                match ctx.ExecutionInfo.Kind with
                | ResolveAbstraction typeMap -> typeMap
                | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind 
            match Map.tryFind resolvedDef.Name typeMap with
            | Some fields -> executeFields resolvedDef ctx (udef.ResolveValue value) fields fieldExecuteMap
            | None -> raise(GraphQLException (sprintf "GraphQL union '%s' doesn't have a case of type '%s'" udef.Name resolvedDef.Name))   
        
    | Enum _ ->
        fun _ value -> 
            let result = coerceStringValue value
            AsyncVal.wrap (result |> Option.map box |> Option.toObj)
    
    | _ -> failwithf "Unexpected value of returnDef: %O" returnDef

and internal compileField possibleTypesFn (fieldDef: FieldDef) (fieldExecuteMap: FieldExecuteMap) : ExecuteField =
    match fieldDef.Resolve with
    | Resolve.BoxedSync(inType, outType, resolve) ->
        let completed = createCompletion possibleTypesFn (fieldDef.TypeDef) fieldExecuteMap
        fun resolveFieldCtx value ->
            try
                let res = resolve resolveFieldCtx value
                if res = null
                then AsyncVal.empty
                else completed resolveFieldCtx res 
            with
            | :? AggregateException as e ->
                e.InnerExceptions |> Seq.iter (resolveFieldCtx.AddError)
                AsyncVal.empty
            | ex -> 
                resolveFieldCtx.AddError ex
                AsyncVal.empty

    | Resolve.BoxedAsync(inType, outType, resolve) ->
        let completed = createCompletion possibleTypesFn (fieldDef.TypeDef) fieldExecuteMap
        fun resolveFieldCtx value -> 
            try
                resolve resolveFieldCtx value
                |> AsyncVal.ofAsync
                |> AsyncVal.bind (completed resolveFieldCtx)
            with
            | :? AggregateException as e ->
                e.InnerExceptions |> Seq.iter (resolveFieldCtx.AddError)
                AsyncVal.empty
            | ex -> 
                resolveFieldCtx.AddError(ex)
                AsyncVal.empty

    | Resolve.BoxedExpr (resolve) ->
        fun resolveFieldCtx value -> 
            try
                downcast resolve resolveFieldCtx value
            with
            | :? AggregateException as e ->
                e.InnerExceptions |> Seq.iter (resolveFieldCtx.AddError)
                AsyncVal.empty
            | ex -> 
                resolveFieldCtx.AddError(ex)
                AsyncVal.empty

    | Undefined -> 
        fun _ _ -> raise (InvalidOperationException(sprintf "Field '%s' has been accessed, but no resolve function for that field definition was provided. Make sure, you've specified resolve function or declared field with Define.AutoField method" fieldDef.Name))

    /// Takes an object type and a field, and returns that field’s type on the object type, or null if the field is not valid on the object type
and private getFieldDefinition (ctx: ExecutionContext) (objectType: ObjectDef) (field: Field) : FieldDef option =
        match field.Name with
        | "__schema" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some (upcast SchemaMetaFieldDef)
        | "__type" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some (upcast TypeMetaFieldDef)
        | "__typename" -> Some (upcast TypeNameMetaFieldDef)
        | fieldName -> objectType.Fields |> Map.tryFind fieldName
        
and private createFieldContext objdef ctx (info: ExecutionInfo) =
    let fdef = info.Definition
    let args = getArgumentValues fdef.Args info.Ast.Arguments ctx.Variables
    { ExecutionInfo = info
      Context = ctx.Context
      ReturnType = fdef.TypeDef
      ParentType = objdef
      Schema = ctx.Schema
      Args = args
      Variables = ctx.Variables }         

and private executeFields (objdef: ObjectDef) (ctx: ResolveFieldContext) (value: obj) fieldInfos (fieldExecuteMap: FieldExecuteMap) : AsyncVal<obj> = 
    let resultSet =
        fieldInfos
        |> List.filter (fun info -> info.Include ctx.Variables)
        |> List.map (fun info -> (info.Identifier, info))
        |> List.toArray
    resultSet
    |> Array.map (fun (name, info) ->  
        let innerCtx = createFieldContext objdef ctx info
        let execute = fieldExecuteMap.GetExecute(objdef.Name, info.Definition.Name)
        let res = execute innerCtx value
        res 
        |> AsyncVal.map (fun x -> KeyValuePair<_,_>(name, x))
        |> AsyncVal.rescue (fun e -> ctx.AddError e; KeyValuePair<_,_>(name, null)))
    |> AsyncVal.collectParallel
    |> AsyncVal.map (fun x -> upcast NameValueLookup x)

let internal executePlan (ctx: ExecutionContext) (plan: ExecutionPlan) (objdef: ObjectDef) (fieldExecuteMap: FieldExecuteMap) value =
    let resultSet =
        plan.Fields
        |> List.filter (fun info -> info.Include ctx.Variables)
        |> List.map (fun info -> (info.Identifier, info))
        |> List.toArray
    let results =
        resultSet
        |> Array.map (fun (name, info) ->
            let fdef = info.Definition
            let args = getArgumentValues fdef.Args info.Ast.Arguments ctx.Variables
            let fieldCtx = 
                { ExecutionInfo = info
                  Context = ctx
                  ReturnType = fdef.TypeDef
                  ParentType = objdef
                  Schema = ctx.Schema
                  Args = args
                  Variables = ctx.Variables } 
            let execute = fieldExecuteMap.GetExecute(ctx.ExecutionPlan.RootDef.Name, info.Definition.Name)
            let res = execute fieldCtx value
            //let res = //info.Definition.Execute fieldCtx value
            res 
            |> AsyncVal.map (fun r -> KeyValuePair<_,_>(name, r))
            |> AsyncVal.rescue (fun e -> fieldCtx.AddError e; KeyValuePair<_,_>(name, null)))
    match plan.Strategy with
    | ExecutionStrategy.Parallel -> AsyncVal.collectParallel results
    | ExecutionStrategy.Sequential   -> AsyncVal.collectSequential results

let private compileInputObject (indef: InputObjectDef) =
    indef.Fields
    |> Array.iter(fun input -> 
        let errMsg = sprintf "Input object '%s': in field '%s': " indef.Name input.Name
        input.ExecuteInput <- compileByType errMsg input.TypeDef)

let internal compileSchema possibleTypesFn types (fieldExecuteMap: FieldExecuteMap) =
    types
    |> Map.toSeq 
    |> Seq.iter (fun (tName, x) ->
        match x with
        | Object objdef -> 
            objdef.Fields
            |> Map.iter (fun _ fieldDef -> 
                
                fieldExecuteMap.SetExecute(tName, fieldDef.Name, compileField possibleTypesFn fieldDef fieldExecuteMap)

                fieldDef.Args
                |> Array.iter (fun arg -> 
                    let errMsg = sprintf "Object '%s': field '%s': argument '%s': " objdef.Name fieldDef.Name arg.Name
                    arg.ExecuteInput <- compileByType errMsg arg.TypeDef))
        | InputObject indef -> compileInputObject indef
        | _ -> ())

let private coerceVariables (variables: VarDef list) (vars: Map<string, obj>) =
    variables
    |> List.fold (fun acc vardef -> Map.add vardef.Name (coerceVariable vardef vars) acc) Map.empty
                
let internal evaluate (schema: #ISchema) (executionPlan: ExecutionPlan) (variables: Map<string, obj>) (root: obj) errors (fieldExecuteMap: FieldExecuteMap) : AsyncVal<NameValueLookup> = 
    let variables = coerceVariables executionPlan.Variables variables
    let ctx = {
        Schema = schema
        ExecutionPlan = executionPlan
        RootValue = root
        Variables = variables
        Errors = errors }
    let result = executePlan ctx executionPlan schema.Query fieldExecuteMap root 
    result |> AsyncVal.map (NameValueLookup)

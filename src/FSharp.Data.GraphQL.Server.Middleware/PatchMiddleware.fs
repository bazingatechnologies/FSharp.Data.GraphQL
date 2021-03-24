namespace FSharp.Data.GraphQL.Samples.StarWarsApi.Middleware

open System
open System.Collections.Concurrent
open System.Collections.Generic
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

[<AutoOpen>]
module PatchMiddleware =

    let [<Literal>] PatchKey = "patch"
    let [<Literal>] ValidatorMetadataKey = "validator"
    let [<Literal>] UpdaterMetadataKey = "updater"

    let getFieldValidationKey key propertyName =
        System.String.Join('/', key, propertyName)

    type Updater<'vm> = Action<ResolveFieldContext,'vm,obj>
    type Validator<'v> = Func<'v, ValidationState>

    let getPatchKeyNotFoundException patchKey = KeyNotFoundException $"'%s{patchKey}' key is not found in metadata. Maybe you forgot to register PatchMiddleware"

    type ResolveFieldContext with

        member ctx.PatchResults key : 'vm =
            match ctx.Context.Metadata.TryFind<ConcurrentDictionary<string, obj>> PatchKey with
            | Some patches -> patches.[key] :?> 'vm
            | None -> raise <| getPatchKeyNotFoundException PatchKey

        member ctx.CachePatchResults key patchViewModel =
            match ctx.Context.Metadata.TryFind<ConcurrentDictionary<string, obj>> PatchKey with
            | Some patches -> patches.[key] <- patchViewModel
            | None -> raise <| getPatchKeyNotFoundException PatchKey

        member ctx.PatchFieldValidationState key propertyName : ValidationState =
            match ctx.Context.Metadata.TryFind<ConcurrentDictionary<string, obj>> PatchKey with
            | None -> raise <| getPatchKeyNotFoundException PatchKey
            | Some patches ->
                let key = getFieldValidationKey key propertyName
                match patches.TryGetValue(key) with
                | true, value -> value :?> ValidationState
                | false, _ -> Ok

        member ctx.CachePatchFieldValidationState key propertyName errors =
            match ctx.Context.Metadata.TryFind<ConcurrentDictionary<string, obj>> PatchKey with
            | None -> raise <| getPatchKeyNotFoundException PatchKey
            | Some patches ->
                let key = getFieldValidationKey key propertyName
                patches.[key] <- (errors : ValidationState)

    let resolveArgs returnNulls info variables =
        let argDefs = info.Definition.Args
        if Array.isEmpty argDefs then Map.empty
        else
            let argumentValue variables (argdef: InputFieldDef) (argument: Argument) =
                match argdef.ExecuteInput argument.Value variables  with
                | null -> argdef.DefaultValue
                | value -> Some value
            let inline argVal vars argDef argOpt =
                match argOpt with
                | Some arg -> argumentValue vars argDef arg
                | None -> argDef.DefaultValue
            let resolveLinqArg vars (name : string, argdef, arg) =
                let value = argVal vars argdef arg
                match value with
                | Some value -> Some (name, value)
                | None when returnNulls -> Some (name, Unchecked.defaultof<_>)
                | None -> None
            let args = info.Ast.Arguments
            argDefs
            |> Seq.map (fun a -> (a.Name, a, args |> List.tryFind (fun x -> x.Name = a.Name)))
            |> Seq.choose (resolveLinqArg variables)
            |> Map.ofSeq

    let tryGetPatchField (ctx : ResolveFieldContext) =
        match ctx.ExecutionInfo.Kind with
        | SelectFields fieldInfos ->
            fieldInfos
            |> Seq.where (fun parentInfo -> parentInfo.Ast.Name = "patch")
            |> Seq.tryHead
        | _ -> None
        |> Option.map (fun info ->
            let args = resolveArgs false info ctx.Context.Variables
            let ctx' = { ctx with Args = args }
            struct (info, ctx'))

    let applyPatch resultKey parentInfo ctx viewModel =
        match parentInfo.Kind with
        | SelectFields patchFieldInfos ->
            patchFieldInfos |> List.fold (fun viewModel info ->
                try
                    let args = resolveArgs true info ctx.Variables
                    match args.TryFind info.Definition.Name with
                    | None -> ()
                    | Some value ->
                        let validationState =
                            match info.Definition.Metadata.TryFind<Validator<obj>> ValidatorMetadataKey with
                            | Some validate -> validate.Invoke value
                            | None -> Ok
                        match validationState with
                        | Errors _ -> ctx.CachePatchFieldValidationState resultKey info.Definition.Name validationState
                        | Ok ->
                            let update : Updater<_> = (info.Definition.Metadata.TryFind UpdaterMetadataKey).Value
                            update.Invoke (ctx, viewModel, value)
                    viewModel
                with _ -> viewModel
            ) viewModel |> box
            |> ignore
        | _ -> ()

    let asyncPatchIfRequested resultKey ctx patchHandler = async {
        match tryGetPatchField ctx with
        | Some struct (info, ctx) ->
            let patcher = applyPatch resultKey info ctx
            let fields =
                match info.Kind with
                | SelectFields patchFieldInfos -> patchFieldInfos |> Seq.map (fun info -> info.Definition.Name)
                | _ -> Seq.empty
            let handler = patchHandler ctx fields patcher : Async<Result<'VM, ErrorMessage>>
            let! result = handler
            ctx.CachePatchResults resultKey result
        | None -> ()
    }

    let patch (ctx : ResolveFieldContext) (viewModel : obj) =
        match ctx.ExecutionInfo.Kind with
        | SelectFields fieldInfos ->
            fieldInfos
            |> Seq.where (fun parentInfo -> parentInfo.Ast.Name = "patch" )
            |> Seq.iter (fun parentInfo ->
                printfn "%A" parentInfo.Ast// ctx.ExecutionInfo
                match parentInfo.Kind with
                | SelectFields patchFieldInfos ->
                    patchFieldInfos |> List.fold (fun viewModel info ->
                        let argDefs = info.Definition.Args
                        let args =
                            if Array.isEmpty argDefs then Map.empty
                            else
                                let argumentValue variables (argdef: InputFieldDef) (argument: Argument) =
                                    match argdef.ExecuteInput argument.Value variables  with
                                    | null -> argdef.DefaultValue
                                    | value -> Some value
                                let inline argVal vars argDef argOpt =
                                    match argOpt with
                                    | Some arg -> argumentValue vars argDef arg
                                    | None -> argDef.DefaultValue
                                let resolveLinqArg vars (name : string, argdef, arg) =
                                    argVal vars argdef arg |> Option.map (fun v -> (name, v))
                                let args = info.Ast.Arguments
                                argDefs
                                |> Seq.map (fun a -> (a.Name, a, args |> List.tryFind (fun x -> x.Name = a.Name)))
                                |> Seq.choose (resolveLinqArg ctx.Variables)
                                |> Map.ofSeq
                        let update = (info.Definition.Metadata.TryFind UpdaterMetadataKey).Value
                        let value = args.[info.Definition.Name]
                        update viewModel value
                        viewModel
                    ) viewModel
                    |> ignore
                | _ -> ()
            )
            | _ -> ()


type internal PatchMiddleware () =
    let middleware (ctx : ExecutionContext) (next : ExecutionContext -> AsyncVal<GQLResponse>) =
        // Add only when patch operation present
        let ctx = { ctx with Metadata = ctx.Metadata.Add("patch", ConcurrentDictionary<string, obj> ()) }
        next ctx
    interface IExecutorMiddleware with
        member __.CompileSchema = None
        member __.PostCompileSchema = None
        member __.PlanOperation = None
        member __.ExecuteOperationAsync = Some (middleware)

namespace FSharp.Data.GraphQL.Samples.StarWarsApi.Middleware

open System
open System.Threading.Tasks
open FSharp.Data.GraphQL.Types
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Linq.RuntimeHelpers
open Microsoft.AspNetCore.Authorization
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open FSharp.Data.GraphQL.Samples.StarWarsApi

type FieldPolicyMiddleware<'Val, 'Res> =
    ResolveFieldContext -> 'Val -> (ResolveFieldContext -> 'Val -> Async<'Res>) -> Async<'Res>

type internal CustomPolicyFieldDefinition<'Val, 'Res>(source : FieldDef<'Val>, middleware : FieldPolicyMiddleware<'Val, 'Res>) =
    interface FieldDef<'Val> with
        member __.Name = source.Name
        member __.Description = source.Description
        member __.DeprecationReason = source.DeprecationReason
        member __.TypeDef = source.TypeDef
        member __.Args = source.Args
        member __.Metadata = source.Metadata
        member __.Resolve =
            let changeAsyncResolver expr =
                let expr =
                    match expr with
                    | WithValue (_, _, e) -> e
                    | _ -> failwith "Unexpected resolver expression."
                let resolver = <@ fun ctx input -> middleware ctx input (%%expr : ResolveFieldContext -> 'Val -> Async<'Res>) @>
                let compiledResolver = LeafExpressionConverter.EvaluateQuotation resolver
                Expr.WithValue(compiledResolver, resolver.Type, resolver)

            let changeSyncResolver expr =
                let expr =
                    match expr with
                    | WithValue (_, _, e) -> e
                    | _ -> failwith "Unexpected resolver expression."
                let resolver = <@ fun ctx input -> middleware ctx input (fun ctx input -> ((%%expr : ResolveFieldContext -> 'Val -> 'Res) ctx input) |> async.Return) @>
                let compiledResolver = LeafExpressionConverter.EvaluateQuotation resolver
                Expr.WithValue(compiledResolver, resolver.Type, resolver)

            match source.Resolve with
            | Sync (input, output, expr) -> Async (input, output, changeSyncResolver expr)
            | Async (input, output, expr) -> Async (input, output, changeAsyncResolver expr)
            | Undefined -> failwith "Field has no resolve function."
            | x -> failwith <| sprintf "Resolver '%A' is not supported." x
    interface IEquatable<FieldDef> with
        member __.Equals(other) = source.Equals(other)
    override __.Equals y = source.Equals y
    override __.GetHashCode() = source.GetHashCode()
    override __.ToString() = source.ToString()

[<AutoOpen>]
module TypeSystemExtensions =

    let handlePolicies (policies : string array) (ctx : ResolveFieldContext) value = async {
        let root : Root = downcast ctx.Context.RootValue
        let serviceProvider = root.ServiceProvider
        let authorizationService = serviceProvider.GetRequiredService<IAuthorizationService>()
        let principal = serviceProvider.GetRequiredService<IHttpContextAccessor>().HttpContext.User
        let! authorizationResults =
            policies
            |> Seq.map (fun p -> authorizationService.AuthorizeAsync(principal, value, p))
            |> Seq.toArray
            |> Task.WhenAll
            |> Async.AwaitTask
        let requirements =
            authorizationResults
            |> Seq.where (fun r -> not r.Succeeded)
            |> Seq.collect (fun r -> r.Failure.FailedRequirements)
        if Seq.isEmpty requirements
        then return Ok ()
        else return Error "Forbidden"
    }

    [<Literal>]
    let AuthorizationPolicy = "AuthorizationPolicy"

    type FieldDef<'Val> with

        member this.WithPolicyMiddleware<'Res>(middleware : FieldPolicyMiddleware<'Val, 'Res>) : FieldDef<'Val> =
            upcast CustomPolicyFieldDefinition(this, middleware)

        member field.WithAuthorizationPolicies<'Res>([<ParamArray>] policies : string array) : FieldDef<'Val> =

            let middleware ctx value (resolver : ResolveFieldContext -> 'Val -> Async<'Res>) : Async<'Res> = async {
                let! result = handlePolicies policies ctx value
                match result with
                | Ok _ -> return! resolver ctx value
                | Error error ->
                    let ex = Exception (error)
                    ctx.AddError ex
                    return raise <| ex
            }

            field.WithPolicyMiddleware<'Res> middleware

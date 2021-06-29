namespace FSharp.Data.GraphQL

open System
open System.Collections
open FSharp.Reflection
open FSharp.Quotations
open FSharp.Data.GraphQL.Types.Introspection

module internal QuotationHelpers =
    let rec coerceValues fieldTypeLookup fields =
        let arrayExpr (arrayType : Type) (v : obj) =
            let typ = arrayType.GetElementType()
            let instance =
                match v with
                | :? IEnumerable as x -> Seq.cast<obj> x |> Array.ofSeq
                | _ -> failwith "Unexpected array value."
            let exprs = coerceValues (fun _ -> typ) instance
            Expr.NewArray(typ, exprs)
        let tupleExpr (tupleType : Type) (v : obj) =
            let typ = FSharpType.GetTupleElements tupleType |> Array.mapi (fun i t -> i, t) |> Map.ofArray
            let fieldTypeLookup i = typ.[i]
            let fields = FSharpValue.GetTupleFields v
            let exprs = coerceValues fieldTypeLookup fields
            Expr.NewTuple(exprs)
        Array.mapi (fun i v ->
                let expr =
                    if isNull v then simpleTypeExpr v
                    else
                        let tpy = v.GetType()
                        if tpy.IsArray then arrayExpr tpy v
                        elif FSharpType.IsTuple tpy then tupleExpr tpy v
                        elif FSharpType.IsUnion tpy then unionExpr v |> snd
                        elif FSharpType.IsRecord tpy then recordExpr v |> snd
                        else simpleTypeExpr v
                Expr.Coerce(expr, fieldTypeLookup i)
        ) fields |> List.ofArray

    and simpleTypeExpr instance = Expr.Value(instance)

    and unionExpr instance =
        let caseInfo, fields = FSharpValue.GetUnionFields(instance, instance.GetType())
        let fieldInfo = caseInfo.GetFields()
        let fieldTypeLookup indx = fieldInfo.[indx].PropertyType
        caseInfo.DeclaringType, Expr.NewUnionCase(caseInfo, coerceValues fieldTypeLookup fields)

    and recordExpr instance =
        let typ = instance.GetType()
        let fields = FSharpValue.GetRecordFields(instance)
        let fieldInfo = FSharpType.GetRecordFields(typ)
        let fieldTypeLookup indx = fieldInfo.[indx].PropertyType
        typ, Expr.NewRecord(instance.GetType(), coerceValues fieldTypeLookup fields)

    and arrayExpr (instance : 'a array) =
        let typ = typeof<'a>
        let arrayType = instance.GetType()
        let exprs = coerceValues (fun _ -> typ) (instance |> Array.map box)
        arrayType, Expr.NewArray(typ, exprs)

    let createLetExpr varType instance body args =
        let var = Var("instance", varType)
        Expr.Let(var, instance, body args (Expr.Var(var)))

    let quoteUnion instance =
        let func instance = unionExpr instance ||> createLetExpr
        Tracer.runAndMeasureExecutionTime "Quoted union type" (fun _ -> func instance)

    let quoteRecord instance =
        let func instance = recordExpr instance ||> createLetExpr
        Tracer.runAndMeasureExecutionTime "Quoted record type" (fun _ -> func instance)

    let quoteArray instance =
        let func instance = arrayExpr instance ||> createLetExpr
        Tracer.runAndMeasureExecutionTime "Quoted array type" (fun _ -> func instance)

[<AutoOpen>]
module internal Patterns =
    let (|NamedTypeRef|ContainerTypeRef|) (x: IntrospectionTypeRef) =
        match x.Name, x.OfType with
        | Some name, None -> NamedTypeRef(x.Kind, name)
        | None, Some innerType -> ContainerTypeRef(x.Kind, innerType)
        | _, _ -> failwithf "Invalid introspection type ref. Expected named or container type %A" x

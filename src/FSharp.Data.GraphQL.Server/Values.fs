/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module internal FSharp.Data.GraphQL.Values

open System
open System.Reflection
open System.Collections.Generic
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns

/// Tries to convert type defined in AST into one of the type defs known in schema.
let inline tryConvertAst schema ast =
    let rec convert isNullable (schema: ISchema) (ast: InputType) : TypeDef option =
        match ast with
        | NamedType name -> 
            match schema.TryFindType name with
            | Some namedDef ->
                Some (if isNullable then upcast namedDef.MakeNullable() else upcast namedDef)
            | None -> None
        | ListType inner ->
            convert true schema inner
            |> Option.map (fun i -> 
                if isNullable 
                then upcast i.MakeList().MakeNullable()
                else upcast i.MakeList())
        | NonNullType inner ->
            convert false schema inner
    convert true schema ast

let inline private notAssignableMsg (innerDef: InputDef) value : string =
    sprintf "value of type %s is not assignable from %s" innerDef.Type.Name (value.GetType().Name)

let rec internal compileByType (errMsg: string) (inputDef: InputDef): ExecuteInput =
    match inputDef with
    | Scalar scalardef -> 
        variableOrElse (scalardef.CoerceInput >> Option.toObj)
    | InputObject objdef -> 
        let objtype = objdef.Type
        let ctor = ReflectionHelper.matchConstructor objtype (objdef.Fields |> Array.map (fun x -> x.Name))
        let mapper =
            ctor.GetParameters()
            |> Array.map(fun param -> 
                match objdef.Fields |> Array.tryFind(fun field -> field.Name = param.Name) with
                | Some x -> x
                | None -> 
                    failwithf "Input object '%s' refers to type '%O', but constructor parameter '%s' doesn't match any of the defined input fields" objdef.Name objtype param.Name)
        fun value variables ->
            match value with
            | ObjectValue props ->
                let args = 
                    mapper 
                    |> Array.map (fun field -> 
                        match Map.tryFind field.Name props with
                        | None -> null
                        | Some prop -> field.ExecuteInput prop variables)
                let instance = ctor.Invoke(args)
                instance
            | Variable variableName -> 
                match Map.tryFind variableName variables with
                | Some found -> found
                | None -> null
            | _ -> null                 
    | List (Input innerdef) -> 
        let inner = compileByType errMsg innerdef       
        let cons, nil = ReflectionHelper.listOfType innerdef.Type

        fun value variables ->
            match value with
            | ListValue list ->
                let mappedValues = list |> List.map (fun value -> inner value variables)
                nil |> List.foldBack cons mappedValues
            | Variable variableName -> variables.[variableName]
            | _ -> 
                // try to construct a list from single element
                let single = inner value variables
                if single = null then null else cons single nil
    | Nullable (Input innerdef) -> 
        let inner = compileByType errMsg innerdef
        let some, none = ReflectionHelper.optionOfType innerdef.Type
                
        fun variables value ->
            let i = inner variables value
            match i with
            | null -> none
            | coerced -> 
                let c = some coerced
                if c <> null then c
                else raise(GraphQLException (errMsg + notAssignableMsg innerdef coerced))
    | Enum enumdef -> 
        fun value variables ->
            match value with
            | Variable variableName -> variables.[variableName]
            | _ ->
                let coerced = coerceEnumInput value
                match coerced with
                | None -> null
                | Some s -> ReflectionHelper.parseUnion enumdef.Type s
    | _ -> failwithf "Unexpected value of inputDef: %O" inputDef
                
let rec private coerceVariableValue isNullable typedef (vardef: VarDef) (input: obj) (errMsg: string) : obj = 
    match typedef with
    | Scalar scalardef -> 
        match scalardef.CoerceValue input with
        | None when isNullable -> null
        | None -> 
            raise (GraphQLException <| errMsg + (sprintf "expected value of type %s but got None" scalardef.Name))
        | Some res -> res
    | Nullable (Input innerdef) -> 
        let some, none = ReflectionHelper.optionOfType innerdef.Type
        let coerced = coerceVariableValue true innerdef vardef input errMsg
        if coerced <> null
        then 
            let s = some coerced
            if s <> null
            then s 
            else raise (GraphQLException <| errMsg + (sprintf "value of type %O is not assignable from %O" innerdef.Type (coerced.GetType())))
        else none
    | List (Input innerdef) ->
        let cons, nil = ReflectionHelper.listOfType innerdef.Type
        match input with
        | null when isNullable -> null
        | null -> raise(GraphQLException <| errMsg + (sprintf "expected value of type %s, but no value was found." (vardef.TypeDef.ToString())))
        // special case - while single values should be wrapped with a list in this scenario,
        // string would be treat as IEnumerable and coerced into a list of chars
        | :? string as s -> 
            let single = coerceVariableValue false innerdef vardef s (errMsg + "element ")
            cons single nil
        | :? System.Collections.IEnumerable as iter -> 
            let mapped =
                iter
                |> Seq.cast<obj>
                |> Seq.map (fun elem -> coerceVariableValue false innerdef vardef elem (errMsg + "list element "))
                //TODO: optimize
                |> Seq.toList
                |> List.rev
                |> List.fold (fun acc coerced -> cons coerced acc) nil
            mapped
        | other -> raise (GraphQLException <| errMsg + (sprintf "Cannot coerce value of type '%O' to list." (other.GetType())))
    | InputObject objdef -> coerceVariableInputObject objdef vardef input (errMsg + (sprintf "in input object '%s': " objdef.Name))
    | _ -> raise (GraphQLException <| errMsg + "Only Scalars, Nullables, Lists and InputObjects are valid type definitions.")

and private coerceVariableInputObject (objdef) (vardef: VarDef) (input: obj) errMsg =
    //TODO: this should be eventually coerced to complex object
    match input with
    | :? Map<string, obj> as map ->
        let mapped = 
            objdef.Fields
            |> Array.map (fun field -> 
                let valueFound = Map.tryFind field.Name map |> Option.toObj
                (field.Name, coerceVariableValue false field.TypeDef vardef valueFound (errMsg + (sprintf "in field '%s': " field.Name))))
            |> Map.ofArray
        upcast mapped
    | _ -> input
    
let internal coerceVariable (vardef: VarDef) (inputs) =
    let vname = vardef.Name 
    match Map.tryFind vname inputs with
    | None -> 
        match vardef.DefaultValue with
        | Some defaultValue -> 
            let errMsg = (sprintf "Variable '%s': " vname)
            let executeInput = compileByType errMsg vardef.TypeDef
            executeInput defaultValue inputs
        | None -> 
            match vardef.TypeDef with
            | Nullable _ -> null
            | _ -> raise (GraphQLException (sprintf "Variable '$%s' of required type %s has no value provided." vname (vardef.TypeDef.ToString())))
    | Some input -> coerceVariableValue false vardef.TypeDef vardef input (sprintf "Variable '$%s': " vname)
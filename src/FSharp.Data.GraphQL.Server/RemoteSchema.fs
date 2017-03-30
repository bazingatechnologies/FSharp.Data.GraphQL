namespace FSharp.Data.GraphQL.RemoteSchema

open System    
open System.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open Microsoft.FSharp.Quotations
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open System.Net
open System.IO
open System.Collections.Generic

[<AutoOpen>]
module RemoteSchema =
    type RemoteSchemaData = {
        Ast: Ast.Field
        SchemaName: string
        Root: obj
        OperationType: Ast.OperationType
        Variables: Map<string, obj>
    }

    type RequestSchemaExecutor() = 
        let mutable schemaMap = new Dictionary<string, ISchema>()
        member this.RegisterSchema(name: string, schema: ISchema) = 
            schemaMap.Add(name, schema)

        member this.Execute(remoteSchemaData: RemoteSchemaData) =
            match schemaMap.TryGetValue(remoteSchemaData.SchemaName) with
            | true, schema ->
                let operationDefinition = 
                    {
                        Ast.OperationDefinition.OperationType = remoteSchemaData.OperationType
                        Ast.OperationDefinition.Name = None
                        Ast.OperationDefinition.VariableDefinitions = []
                        Ast.OperationDefinition.Directives = []
                        Ast.OperationDefinition.SelectionSet = [
                              Ast.Selection.Field (remoteSchemaData.Ast)
                        ]
                    }

                let doc = {
                    Ast.Document.Definitions = [
                        Ast.Definition.OperationDefinition operationDefinition
                    ]
                }

                let ex = Executor(schema)
                let result = Async.RunSynchronously <| ex.AsyncExecute(doc, remoteSchemaData.Root, remoteSchemaData.Variables)
                result

            | false, _ ->
                let res = new Dictionary<string, obj>()
                res.Add("errors", [new Exception("Schema is not registered")])
                upcast res


    type FSharp.Data.GraphQL.Types.SchemaDefinitions.Define with
        static member RemoteSchemaField(name : string, serializer : RemoteSchemaData -> obj, transport : obj -> IDictionary<string, obj>) : FieldDef<'Val> = 
            let resolve (resolveFieldCtx : ResolveFieldContext) (value : obj) : AsyncVal<obj> = 
                let plan = resolveFieldCtx.Context.ExecutionPlan
                let ast = resolveFieldCtx.ExecutionInfo.Ast

                let rsd = {
                    RemoteSchemaData.Ast = ast
                    SchemaName = name
                    Root = resolveFieldCtx.Context.RootValue
                    OperationType = plan.Operation.OperationType
                    Variables = resolveFieldCtx.Variables
                }

                let dictRes = transport(serializer(rsd))

                match dictRes.TryGetValue("errors") with
                | true, errorsObj ->
                    // TODO: add exception adding
                    let errors = errorsObj :?> string seq
                    resolveFieldCtx.AddError(new Exception("Remote schema exception."))
                    ()
                | false, _ -> ()
                
                let resData = dictRes.["data"] :?> Collections.Generic.IDictionary<string, obj>
                AsyncVal.wrap(resData.[ast.Name] :> obj)

            Define.CustomField(name, resolve)

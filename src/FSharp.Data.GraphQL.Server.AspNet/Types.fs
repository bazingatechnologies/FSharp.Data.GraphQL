namespace FSharp.Data.GraphQL.Types

open System.IO
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Types

type GraphQLQuery =
    { ExecutionPlan : ExecutionPlan
      Variables : Map<string, obj> }

type GraphQLQueryRequest =
    | Single of GraphQLQuery
    | Batch of GraphQLQuery list

type WebSocketClientMessage =
    | ConnectionInit of token : string option * session : string option * scheme : string option
    | ConnectionTerminate
    | Start of id : string * payload : GraphQLQuery
    | Stop of id : string
    | ParseError of id : string option * err : string

type WebSocketServerMessage =
    | ConnectionAck
    | ConnectionError of err : string
    | Data of id : string * payload : Output
    | Error of id : string option * err : string
    | Complete of id : string


type FileUpload =
    { Name : string
      ContentType : string
      Size : int
      Content : Stream
      Path : string
      Hash : string }

[<AutoOpen>]
module SchemaDefinitions =
    let FileUpload =
        Define.Scalar<FileUpload>(
            name = "Upload",
            description = "The `Upload` scalar type represents a file upload.",
            coerceValue = (fun value ->
                match value with
                | :? FileUpload as upload -> Some upload
                | _ -> None),
            coerceInput = fun value ->
                raise <| invalidOp("Upload cannot be coerced from  AST input.")
        )
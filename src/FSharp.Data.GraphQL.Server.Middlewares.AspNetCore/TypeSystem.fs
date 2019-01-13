namespace FSharp.Data.GraphQL.Server.Middlewares.AspNetCore

open System.IO
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Ast

/// Represents a file in a GraphQL file upload.
type File =
      /// Gets the name of the file.
    { Name : string
      /// Gets the type of the content of the file.
      ContentType : string
      /// Gets a stream which returns the content of the file when read.
      Content : Stream }

/// Contains customized schema definitions for extensibility features.
[<AutoOpen>]
module SchemaDefinitions =
    let private coerceStreamInput (_ : Value) : Stream option =
        failwith "Can not coerce stream input. The type `Stream` can only be passed as a variable through a multipart request."
    
    let private coerceStreamValue (value : obj) =
        match value with
        | :? Stream as stream -> Some stream
        | _ -> None

    /// GraphQL type for binary data stream representation.
    let Stream : ScalarDefinition<Stream> =
        { Name = "Stream"
          Description = Some "The `Stream` type represents a stream of binary data."
          CoerceInput = coerceStreamInput
          CoerceValue = coerceStreamValue }

    /// GraphQL type for multipart request file uploads.
    let Upload : InputObjectDefinition<File> =
        { Name = "Upload"
          Description = Some "The `Upload` type is used to represent a file upload to the server."
          FieldsFn = fun () -> 
          [| 
            Define.Input("name", String, description = "Gets the name of the file.")
            Define.Input("contentType", String, description = "Gets the MIME content type of the file.")
            Define.Input("content", Stream, description = "Gets the content of the file.")
          |] }
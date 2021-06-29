namespace FSharp.Data.GraphQL.Samples.FileUpload

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.AspNet
open type FSharp.Data.GraphQL.Types.SchemaDefinitions.Define

type UploadsInput =
  { Files : FileUpload list }

type FileInfo =
    { Hash: string
      Path: string
      FileName: string
      ContentType: string }

type Root = unit

module Schema =
    let mkFileInfo (upload: FileUpload) =
        { Hash = upload.Hash
          Path = upload.Path
          FileName = upload.Name
          ContentType = upload.ContentType }


    let UploadsType =
        InputObject<UploadsInput>(
            name = "Uploads",
            description = "The `Upload` scalar type represents a file upload.",
            fields = [
                Input("files", ListOf FileUpload)
            ]
        )

    let FileInfoType =
        Object<FileInfo>(
            name = "FileInfo",
            description = "The `FileInfo` type represents info about an upload",
            fields = [
                AutoField("hash", String)
                AutoField("path", String)
                AutoField("fileName", String)
                AutoField("contentType", String)
            ]
        )

    let Query =
        Object<Root>(
            name = "Query",
            fields = [
                Field("Message", String, "A greeting", fun ctx _ -> "Hello World")
            ]
        )

    let Mutation =
        Object<Root>(
            name = "Mutation",
            fields = [
                Field("singleUpload", FileInfoType, "upload a single file", [Input("file", FileUpload)],
                    fun ctx _ ->
                        let file = ctx.Arg("file")
                        mkFileInfo file
                    )
                Field("multiUpload", ListOf FileInfoType, "upload a single file", [Input("files", ListOf FileUpload)],
                    fun ctx _ ->
                        let files = ctx.Arg("files")
                        files |> List.map mkFileInfo
                    )
                Field("multiUploadRecord", ListOf FileInfoType, "upload a single file", [Input("files", UploadsType)],
                    fun ctx _ ->
                        let files = ctx.Arg("files")
                        files.Files |> List.map mkFileInfo
                    )
            ]
        )
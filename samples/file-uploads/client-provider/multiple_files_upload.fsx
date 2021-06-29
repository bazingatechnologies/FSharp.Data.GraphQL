#r "../../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/netstandard2.0/Microsoft.Extensions.Http.dll"
#r "../../../src/FSharp.Data.GraphQL.Shared/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

open System
open System.IO
open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"http://localhost:8080/graphql", uploadInputTypeName = "Upload">

let multipleFiles =
    MyProvider.Operation<"""mutation multipleFilesUpload($files: [Upload!]!) {
      multiUpload(files: $files) {
        hash
        path
        fileName
        contentType
      }
    }""">()

let singleFile =
    MyProvider.Operation<"""mutation singleFilesUpload($file: Upload!) {
      singleUpload(file: $file) {
        hash
        path
        fileName
        contentType
      }
    }""">()


let multiFileRecord =
    MyProvider.Operation<"""mutation multiFileRecord($files: Uploads!) {
      multiUploadRecord(files: $files) {
        hash
        path
        fileName
        contentType
      }
    }""">()

let dir = __SOURCE_DIRECTORY__

let uploadSingle() =
    let input = new Upload(File.OpenRead($"{dir}/txt_file.txt"), "text.txt", ownsStream = true)
    let result = singleFile.Run(input)
    printfn "Single Data: %A" result.Data

let uploadMulti() =
    let input =
        [| new Upload(File.OpenRead($"{dir}/txt_file.txt"), "text.txt", ownsStream = true)
           new Upload(File.OpenRead($"{dir}/png_file.png"), "image.png", ownsStream = true) |]
    let result = multipleFiles.Run(input)
    printfn "Multi Data: %A" result.Data

let uploadMultiRecord() =
    let input =
        MyProvider.Types.Uploads([|
           new Upload(File.OpenRead($"{dir}/txt_file.txt"), "text.txt", ownsStream = true)
           new Upload(File.OpenRead($"{dir}/png_file.png"), "image.png", ownsStream = true) |])
    let result = multiFileRecord.Run(input)
    printfn "Multi Record Data: %A" result.Data

uploadSingle()
uploadMulti()
uploadMultiRecord()
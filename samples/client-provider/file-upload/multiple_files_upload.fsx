// Uncomment those to use build script client assembly
//#r "../../../bin/FSharp.Data.GraphQL.Client/net47/FSharp.Data.GraphQL.Client.dll"
//#r "../../../bin/FSharp.Data.GraphQL.Shared/net47/FSharp.Data.GraphQL.Shared.dll"

// Uncomment those to use build script client assembly using netstandard2.0
//#r "../../../bin/FSharp.Data.GraphQL.Shared/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
//#r "../../../bin/FSharp.Data.GraphQL.Client/netstandard2.0/netstandard.dll"
//#r "../../../bin/FSharp.Data.GraphQL.Client/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

// Uncomment those to use dotnet build command for the client assembly
// #r "../../../src/FSharp.Data.GraphQL.Shared/bin/Debug/net47/FSharp.Data.GraphQL.Shared.dll"
// #r "../../../src/FSharp.Data.GraphQL.Client/bin/Debug/net47/FSharp.Data.GraphQL.Client.dll"

//Uncomment those to use dotnet build command for the client assembly using netstandard2.0
#r "../../../src/FSharp.Data.GraphQL.Shared/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/netstandard.dll"
#r "../../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/Microsoft.Extensions.Http.dll"
#r "../../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

open System
open System.IO
open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"http://localhost:3001/graphql", uploadInputTypeName = "Upload">

let mutation =
    MyProvider.Operation<"""mutation multipleFilesUpload($request: MultipleUploadRequest!) {
      multipleUpload(request: $request) {
        id
        path
        filename
        mimetype
      }
    }""">()

let upload() =
    let input =
        [| new Upload(File.OpenRead("txt_file.txt"), "text.txt", ownsStream = true)
           new Upload(File.OpenRead("png_file.png"), "image.png", ownsStream = true) |]
    let request = MyProvider.Types.MultipleUploadRequest(input)
    let result = mutation.Run(request)
    input |> Array.iter (fun x -> (x :> IDisposable).Dispose())
    printfn "Data: %A" result.Data

upload()
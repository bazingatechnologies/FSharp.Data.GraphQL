module FSharp.Data.GraphQL.IntegrationTests.LocalProviderTests

open Xunit
open Helpers
open FSharp.Data.GraphQL

let [<Literal>] ServerUrl = "http://localhost:8085"
let [<Literal>] EmptyGuidAsString = "00000000-0000-0000-0000-000000000000"

type Provider = GraphQLProvider<ServerUrl, uploadInputTypeName = "Upload", explicitOptionalParameters = false>

let context = Provider.GetContext(ServerUrl)

type Input = Provider.Types.Input
type InputField = Provider.Types.InputField

module SimpleOperation =
    let operation =
        Provider.Operation<"""query Q($input: Input) {
            echo(input: $input) {
              single {
                ...Field
              }
              list {
                ...Field
              }
            }
          }

          fragment Field on OutputField {
            string
            stringOption
            int
            intOption
            uri
            deprecated
            guid
          }""">()

    type Operation = Provider.Operations.Q

    let validateResult (input : Input option) (result : Operation.OperationResult) =
        result.Data.IsSome |> equals true
        input |> Option.iter (fun input ->
            result.Data.Value.Echo.IsSome |> equals true
            input.List |> Option.iter (fun list ->
                result.Data.Value.Echo.Value.List.IsSome |> equals true
                let input = list |> Array.map (fun x -> x.Int, x.IntOption, x.String, x.StringOption, x.Uri, x.Guid.ToString())
                let output = result.Data.Value.Echo.Value.List.Value |> Array.map (fun x -> x.Int, x.IntOption, x.String, x.StringOption, x.Uri, x.Guid)
                input |> equals output)
            input.Single |> Option.iter (fun single ->
                result.Data.Value.Echo.Value.Single.IsSome |> equals true
                let input = single.Int, single.IntOption, single.String, single.StringOption, single.Uri, single.Guid.ToString()
                let output = result.Data.Value.Echo.Value.Single.Value |> map (fun x -> x.Int, x.IntOption, x.String, x.StringOption, x.Uri, x.Guid)
                input |> equals output))

[<Fact>]
let ``Should be able to execute a query without sending input field``() =
    SimpleOperation.operation.Run()
    |> SimpleOperation.validateResult None

[<Fact>]
let ``Should be able to execute a query using context, without sending input field``() =
    SimpleOperation.operation.Run(context)
    |> SimpleOperation.validateResult None

[<Fact>]
let ``Should be able to execute a query without sending input field asynchronously``() =
    SimpleOperation.operation.AsyncRun()
    |> Async.RunSynchronously
    |> SimpleOperation.validateResult None

[<Fact>]
let ``Should be able to execute a query using context, without sending input field, asynchronously``() =
    SimpleOperation.operation.AsyncRun(context)
    |> Async.RunSynchronously
    |> SimpleOperation.validateResult None

[<Fact>]
let ``Should be able to execute a query sending an empty input field``() =
    let input = Input()
    SimpleOperation.operation.Run(input)
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query using context, sending an empty input field``() =
    let input = Input()
    SimpleOperation.operation.Run(context, input)
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query without sending an empty input field asynchronously``() =
    let input = Input()
    SimpleOperation.operation.AsyncRun(input)
    |> Async.RunSynchronously
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query using context, sending an empty input field, asynchronously``() =
    let input = Input()
    SimpleOperation.operation.AsyncRun(context, input)
    |> Async.RunSynchronously
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query sending an input field with single field``() =
    let single = InputField("A", 2, System.Uri("http://localhost:1234"), EmptyGuidAsString)
    let input = Input(single)
    SimpleOperation.operation.Run(input)
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query using context, sending an an input field with single field``() =
    let single = InputField("A", 2, System.Uri("http://localhost:1234"),  EmptyGuidAsString)
    let input = Input(single)
    SimpleOperation.operation.Run(context, input)
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query without sending an an input field with single field asynchronously``() =
    let single = InputField("A", 2, System.Uri("http://localhost:1234"),  EmptyGuidAsString)
    let input = Input(single)
    SimpleOperation.operation.AsyncRun(input)
    |> Async.RunSynchronously
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query using context, sending an an input field with single field, asynchronously``() =
    let single = InputField("A", 2, System.Uri("http://localhost:1234"), EmptyGuidAsString)
    let input = Input(single)
    SimpleOperation.operation.AsyncRun(context, input)
    |> Async.RunSynchronously
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query sending an input field with list field``() =
    let list = [|InputField("A", 2, System.Uri("http://localhost:4321"), EmptyGuidAsString)|]
    let input = Input(list)
    SimpleOperation.operation.Run(input)
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query using context, sending an an input field with list field``() =
    let list = [|InputField("A", 2, System.Uri("http://localhost:4321"), EmptyGuidAsString)|]
    let input = Input(list)
    SimpleOperation.operation.Run(context, input)
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query without sending an an input field with list field asynchronously``() =
    let list = [|InputField("A", 2, System.Uri("http://localhost:4321"),  EmptyGuidAsString)|]
    let input = Input(list)
    SimpleOperation.operation.AsyncRun(input)
    |> Async.RunSynchronously
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query using context, sending an an input field with list field, asynchronously``() =
    let list = [|InputField("A", 2, System.Uri("http://localhost:4321"), EmptyGuidAsString)|]
    let input = Input(list)
    SimpleOperation.operation.AsyncRun(context, input)
    |> Async.RunSynchronously
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query sending an input field with single and list fields``() =
    let single = InputField("A", 2, System.Uri("http://localhost:1234"), EmptyGuidAsString)
    let list = [|InputField("A", 2, System.Uri("http://localhost:4321"), EmptyGuidAsString)|]
    let input = Input(single, list)
    SimpleOperation.operation.Run(input)
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query using context, sending an an input field with single and list fields``() =
    let single = InputField("A", 2, System.Uri("http://localhost:1234"), EmptyGuidAsString)
    let list = [|InputField("A", 2, System.Uri("http://localhost:4321"), EmptyGuidAsString)|]
    let input = Input(single, list)
    SimpleOperation.operation.Run(context, input)
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query without sending an an input field with single and list fields asynchronously``() =
    let single = InputField("A", 2, System.Uri("http://localhost:1234"), EmptyGuidAsString)
    let list = [|InputField("A", 2, System.Uri("http://localhost:4321"), EmptyGuidAsString)|]
    let input = Input(single, list)
    SimpleOperation.operation.AsyncRun(input)
    |> Async.RunSynchronously
    |> SimpleOperation.validateResult (Some input)

[<Fact>]
let ``Should be able to execute a query using context, sending an an input field with single and list fields, asynchronously``() =
    let single = InputField("A", 2, System.Uri("http://localhost:1234"), EmptyGuidAsString)
    let list = [|InputField("A", 2, System.Uri("http://localhost:4321"), EmptyGuidAsString)|]
    let input = Input(single, list)
    SimpleOperation.operation.AsyncRun(context, input)
    |> Async.RunSynchronously
    |> SimpleOperation.validateResult (Some input)

module SingleRequiredUploadOperation =
    let operation =
        Provider.Operation<"""mutation SingleUpload($file: Upload!) {
            singleUpload(file: $file) {
              name
              contentType
              contentAsText
            }
          }""">()

    type Operation = Provider.Operations.SingleUpload

    let validateResult (file : File) (result : Operation.OperationResult) =
        result.Data.IsSome |> equals true
        result.Data.Value.SingleUpload.Name |> equals file.Name
        result.Data.Value.SingleUpload.ContentAsText |> equals file.Content
        result.Data.Value.SingleUpload.ContentType |> equals file.ContentType

[<Fact>]
let ``Should be able to execute a single required upload``() =
    let file = { Name = "file.txt"; ContentType = "text/plain"; Content = "Sample text file contents" }
    SingleRequiredUploadOperation.operation.Run(file.MakeUpload())
    |> SingleRequiredUploadOperation.validateResult file

[<Fact>]
let ``Should be able to execute a single required upload asynchronously``() =
    let file = { Name = "file.txt"; ContentType = "text/plain"; Content = "Sample text file contents" }
    SingleRequiredUploadOperation.operation.AsyncRun(file.MakeUpload())
    |> Async.RunSynchronously
    |> SingleRequiredUploadOperation.validateResult file

module SingleOptionalUploadOperation =
    let operation =
        Provider.Operation<"""mutation NullableSingleUpload($file: Upload) {
            nullableSingleUpload(file: $file) {
              name
              contentType
              contentAsText
            }
          }""">()

    type Operation = Provider.Operations.NullableSingleUpload

    let validateResult (file : File option) (result : Operation.OperationResult) =
        result.Data.IsSome |> equals true
        file |> Option.iter (fun file ->
        result.Data.Value.NullableSingleUpload.IsSome |> equals true
        result.Data.Value.NullableSingleUpload.Value.Name |> equals file.Name
        result.Data.Value.NullableSingleUpload.Value.ContentAsText |> equals file.Content
        result.Data.Value.NullableSingleUpload.Value.ContentType |> equals file.ContentType)

[<Fact>]
let ``Should be able to execute a single optional upload by passing a file``() =
    let file = { Name = "file.txt"; ContentType = "text/plain"; Content = "Sample text file contents" }
    SingleOptionalUploadOperation.operation.Run(file.MakeUpload())
    |> SingleOptionalUploadOperation.validateResult (Some file)

[<Fact>]
let ``Should be able to execute a single optional upload by passing a file, asynchronously``() =
    let file = { Name = "file.txt"; ContentType = "text/plain"; Content = "Sample text file contents" }
    SingleOptionalUploadOperation.operation.AsyncRun(file.MakeUpload())
    |> Async.RunSynchronously
    |> SingleOptionalUploadOperation.validateResult (Some file)

[<Fact>]
let ``Should be able to execute a single optional upload by not passing a file``() =
    SingleOptionalUploadOperation.operation.Run()
    |> SingleOptionalUploadOperation.validateResult None

[<Fact>]
let ``Should be able to execute a single optional upload by not passing a file asynchronously``() =
    SingleOptionalUploadOperation.operation.AsyncRun()
    |> Async.RunSynchronously
    |> SingleOptionalUploadOperation.validateResult None

module RequiredMultipleUploadOperation =
    let operation =
        Provider.Operation<"""mutation MultipleUpload($files: [Upload!]!) {
            multipleUpload(files: $files) {
              name
              contentType
              contentAsText
            }
          }""">()

    type Operation = Provider.Operations.MultipleUpload

    let validateResult (files : File []) (result : Operation.OperationResult) =
        result.Data.IsSome |> equals true
        let receivedFiles =
            result.Data.Value.MultipleUpload
            |> Array.map (fun file -> { Name = file.Name; ContentType = file.ContentType; Content = file.ContentAsText })
        receivedFiles |> equals files

[<Fact>]
let ``Should be able to execute a multiple required upload``() =
    let files =
        [| { Name = "file1.txt"; ContentType = "text/plain"; Content = "Sample text file contents 1" }
           { Name = "file2.txt"; ContentType = "text/plain"; Content = "Sample text file contents 2" } |]
    RequiredMultipleUploadOperation.operation.Run(files |> Array.map (fun f -> f.MakeUpload()))
    |> RequiredMultipleUploadOperation.validateResult files

[<Fact>]
let ``Should be able to execute a multiple required upload asynchronously``() =
    let files =
        [| { Name = "file1.txt"; ContentType = "text/plain"; Content = "Sample text file contents 1" }
           { Name = "file2.txt"; ContentType = "text/plain"; Content = "Sample text file contents 2" } |]
    RequiredMultipleUploadOperation.operation.AsyncRun(files |> Array.map (fun f -> f.MakeUpload()))
    |> Async.RunSynchronously
    |> RequiredMultipleUploadOperation.validateResult files

module OptionalMultipleUploadOperation =
    let operation =
        Provider.Operation<"""mutation NullableMultipleUpload($files: [Upload!]) {
            nullableMultipleUpload(files: $files) {
              name
              contentType
              contentAsText
            }
          }""">()

    type Operation = Provider.Operations.NullableMultipleUpload

    let validateResult (files : File [] option) (result : Operation.OperationResult) =
        result.Data.IsSome |> equals true
        let receivedFiles =
            result.Data.Value.NullableMultipleUpload
            |> Option.map (Array.map (fun file -> { Name = file.Name; ContentType = file.ContentType; Content = file.ContentAsText }))
        receivedFiles |> equals files

[<Fact>]
let ``Should be able to execute a multiple upload``() =
    let files =
        [| { Name = "file1.txt"; ContentType = "text/plain"; Content = "Sample text file contents 1" }
           { Name = "file2.txt"; ContentType = "text/plain"; Content = "Sample text file contents 2" } |]
    OptionalMultipleUploadOperation.operation.Run(files |> Array.map (fun f -> f.MakeUpload()))
    |> OptionalMultipleUploadOperation.validateResult (Some files)

[<Fact>]
let ``Should be able to execute a multiple upload asynchronously``() =
    let files =
        [| { Name = "file1.txt"; ContentType = "text/plain"; Content = "Sample text file contents 1" }
           { Name = "file2.txt"; ContentType = "text/plain"; Content = "Sample text file contents 2" } |]
    OptionalMultipleUploadOperation.operation.AsyncRun(files |> Array.map (fun f -> f.MakeUpload()))
    |> Async.RunSynchronously
    |> OptionalMultipleUploadOperation.validateResult (Some files)

[<Fact>]
let ``Should be able to execute a multiple upload by sending no uploads``() =
    OptionalMultipleUploadOperation.operation.Run()
    |> OptionalMultipleUploadOperation.validateResult None

[<Fact>]
let ``Should be able to execute a multiple upload asynchronously by sending no uploads``() =
    OptionalMultipleUploadOperation.operation.AsyncRun()
    |> Async.RunSynchronously
    |> OptionalMultipleUploadOperation.validateResult None

module OptionalMultipleOptionalUploadOperation =
    let operation =
        Provider.Operation<"""mutation NullableMultipleNullableUpload($files: [Upload]) {
            nullableMultipleNullableUpload(files: $files) {
              name
              contentType
              contentAsText
            }
          }""">()

    type Operation = Provider.Operations.NullableMultipleNullableUpload

    let validateResult (files : File option [] option) (result : Operation.OperationResult) =
        result.Data.IsSome |> equals true
        let receivedFiles =
            result.Data.Value.NullableMultipleNullableUpload
            |> Option.map (Array.map (Option.map (fun file -> { Name = file.Name; ContentType = file.ContentType; Content = file.ContentAsText })))
        receivedFiles |> equals files

[<Fact>]
let ``Should be able to execute a multiple optional upload``() =
    let files =
        [| Some { Name = "file1.txt"; ContentType = "text/plain"; Content = "Sample text file contents 1" }
           Some { Name = "file2.txt"; ContentType = "text/plain"; Content = "Sample text file contents 2" } |]
    OptionalMultipleOptionalUploadOperation.operation.Run(files |> Array.map (Option.map (fun f -> f.MakeUpload())))
    |> OptionalMultipleOptionalUploadOperation.validateResult (Some files)

[<Fact>]
let ``Should be able to execute a multiple optional upload asynchronously``() =
    let files =
        [| Some { Name = "file1.txt"; ContentType = "text/plain"; Content = "Sample text file contents 1" }
           Some { Name = "file2.txt"; ContentType = "text/plain"; Content = "Sample text file contents 2" } |]
    OptionalMultipleOptionalUploadOperation.operation.AsyncRun(files |> Array.map (Option.map (fun f -> f.MakeUpload())))
    |> Async.RunSynchronously
    |> OptionalMultipleOptionalUploadOperation.validateResult (Some files)

[<Fact>]
let ``Should be able to execute a multiple optional upload by sending no uploads``() =
    OptionalMultipleOptionalUploadOperation.operation.Run()
    |> OptionalMultipleOptionalUploadOperation.validateResult None

[<Fact>]
let ``Should be able to execute a multiple optional upload asynchronously by sending no uploads``() =
    OptionalMultipleOptionalUploadOperation.operation.AsyncRun()
    |> Async.RunSynchronously
    |> OptionalMultipleOptionalUploadOperation.validateResult None

[<Fact>]
let ``Should be able to execute a multiple optional upload by sending some uploads``() =
    let files =
        [| Some { Name = "file1.txt"; ContentType = "text/plain"; Content = "Sample text file contents 1" }
           None
           Some { Name = "file2.txt"; ContentType = "text/plain"; Content = "Sample text file contents 2" }
           None |]
    OptionalMultipleOptionalUploadOperation.operation.Run(files |> Array.map (Option.map (fun f -> f.MakeUpload())))
    |> OptionalMultipleOptionalUploadOperation.validateResult (Some files)

[<Fact>]
let ``Should be able to execute a multiple optional upload asynchronously by sending some uploads``() =
    let files =
        [| Some { Name = "file1.txt"; ContentType = "text/plain"; Content = "Sample text file contents 1" }
           None
           Some { Name = "file2.txt"; ContentType = "text/plain"; Content = "Sample text file contents 2" }
           None |]
    OptionalMultipleOptionalUploadOperation.operation.AsyncRun(files |> Array.map (Option.map (fun f -> f.MakeUpload())))
    |> Async.RunSynchronously
    |> OptionalMultipleOptionalUploadOperation.validateResult (Some files)

module UploadRequestOperation =
    let operation =
        Provider.Operation<"""mutation UploadRequestOperation($request: UploadRequest!) {
            uploadRequest(request: $request) {
              single {
                ...File
              }
              multiple {
                ...File
              }
              nullableMultiple {
                ...File
              }
              nullableMultipleNullable {
                ...File
              }
            }
          }

          fragment File on UploadedFile {
            name
            contentType
            contentAsText
          }""">()

    type Operation = Provider.Operations.UploadRequestOperation

    type Request = Provider.Types.UploadRequest

    let validateResult (request : FilesRequest) (result : Operation.OperationResult) =
        result.Data.IsSome |> equals true
        result.Data.Value.UploadRequest.Single.ToDictionary() |> File.FromDictionary |> equals request.Single
        result.Data.Value.UploadRequest.Multiple |> Array.map ((fun x -> x.ToDictionary()) >> File.FromDictionary) |> equals request.Multiple
        result.Data.Value.UploadRequest.NullableMultiple |> Option.map (Array.map ((fun x -> x.ToDictionary()) >> File.FromDictionary)) |> equals request.NullableMultiple
        result.Data.Value.UploadRequest.NullableMultipleNullable |> Option.map (Array.map (Option.map ((fun x -> x.ToDictionary()) >> File.FromDictionary))) |> equals request.NullableMultipleNullable

[<Fact>]
let ``Should be able to upload files inside another input type``() =
    let request =
        { Single = { Name = "single.txt"; ContentType = "text/plain"; Content = "Single file content" }
          Multiple =
            [| { Name = "multiple1.txt"; ContentType = "text/plain"; Content = "Multiple files first file content" }
               { Name = "multiple2.txt"; ContentType = "text/plain"; Content = "Multiple files second file content" } |]
          NullableMultiple = Some [| { Name = "multiple3.txt"; ContentType = "text/plain"; Content = "Multiple files third file content" } |]
          NullableMultipleNullable =
            Some [| Some { Name = "multiple4.txt"; ContentType = "text/plain"; Content = "Multiple files fourth file content" }; None |] }
    let input =
        let makeUpload (x : File) = x.MakeUpload()
        UploadRequestOperation.Request(single = makeUpload request.Single,
                                       multiple = Array.map makeUpload request.Multiple,
                                       nullableMultiple = Array.map makeUpload request.NullableMultiple.Value,
                                       nullableMultipleNullable = Array.map (Option.map makeUpload) request.NullableMultipleNullable.Value)
    UploadRequestOperation.operation.Run(input)
    |> UploadRequestOperation.validateResult request
module FSharp.Data.GraphQL.Tests.MultipartRequestTests

open System
open System.Text
open System.IO
open FSharp.Data.GraphQL.Server.Middlewares.AspNetCore
open Microsoft.AspNetCore.WebUtilities
open Xunit

let private getReader boundary stream = MultipartReader(boundary, stream)

let private getStream (content : string) : Stream = upcast new MemoryStream(Encoding.UTF8.GetBytes(content))

let private read reader = MultipartRequest.read reader |> Async.AwaitTask |> Async.RunSynchronously

// MIME text contents must always have CRLF as line breaks
let private normalizeContent (content : string) = content.Replace("\r\n", "\n").Replace("\n", "\r\n")

[<Fact>]
let ``Should be able to read a single operation request with one file``() =
    let content = normalizeContent """--------------------------cec8e8123c05ba25
Content-Disposition: form-data; name="operations"

{ "query": "mutation ($file: Upload!) { singleUpload(file: $file) { id } }", "variables": { "file": null } }
--------------------------cec8e8123c05ba25
Content-Disposition: form-data; name="map"

{ "0": ["variables.file"] }
--------------------------cec8e8123c05ba25
Content-Disposition: form-data; name="0"; filename="a.txt"
Content-Type: text/plain

Alpha file content.

--------------------------cec8e8123c05ba25--"""
    use stream = getStream(content)
    let reader = getReader "------------------------cec8e8123c05ba25" stream
    let request = read reader
    request.Operations
    |> single
    |> queryEquals "mutation ($file: Upload!) { singleUpload(file: $file) { id } }"
    |> containsFile "file"
    |> hasName "a.txt"
    |> hasContentType "text/plain"
    |> hasContent "Alpha file content.\r\n"

[<Fact>]
let ``Should be able to read a single operation request with multiple files``() =
    let content = normalizeContent """--------------------------ec62457de6331cad
Content-Disposition: form-data; name="operations"

{ "query": "mutation($files: [Upload!]!) { multipleUpload(files: $files) { id } }", "variables": { "files": [null, null] } }
--------------------------ec62457de6331cad
Content-Disposition: form-data; name="map"

{ "0": ["variables.files.0"], "1": ["variables.files.1"] }
--------------------------ec62457de6331cad
Content-Disposition: form-data; name="0"; filename="b.txt"
Content-Type: text/plain

Bravo file content.

--------------------------ec62457de6331cad
Content-Disposition: form-data; name="1"; filename="c.txt"
Content-Type: text/plain

Charlie file content.

--------------------------ec62457de6331cad--"""
    use stream = getStream(content)
    let reader = getReader "------------------------ec62457de6331cad" stream
    let request = read reader
    request.Operations
    |> single
    |> queryEquals "mutation($files: [Upload!]!) { multipleUpload(files: $files) { id } }"
    |> containsFiles "files"
    |> hasContentTypes [ "text/plain"; "text/plain" ]
    |> hasNames [ "b.txt"; "c.txt" ]
    |> hasContents [ "Bravo file content.\r\n"; "Charlie file content.\r\n" ]

[<Fact>]
let ``Should be able to read a multiple operation request``() =
    let content = normalizeContent """--------------------------627436eaefdbc285
Content-Disposition: form-data; name="operations"

[{ "query": "mutation ($file: Upload!) { singleUpload(file: $file) { id } }", "variables": { "file": null } }, { "query": "mutation($files: [Upload!]!) { multipleUpload(files: $files) { id } }", "variables": { "files": [null, null] } }]
--------------------------627436eaefdbc285
Content-Disposition: form-data; name="map"

{ "0": ["0.variables.file"], "1": ["1.variables.files.0"], "2": ["1.variables.files.1"] }
--------------------------627436eaefdbc285
Content-Disposition: form-data; name="0"; filename="a.txt"
Content-Type: text/plain

Alpha file content.

--------------------------627436eaefdbc285
Content-Disposition: form-data; name="1"; filename="b.txt"
Content-Type: text/plain

Bravo file content.

--------------------------627436eaefdbc285
Content-Disposition: form-data; name="2"; filename="c.txt"
Content-Type: text/plain

Charlie file content.

--------------------------627436eaefdbc285--"""
    use stream = getStream(content)
    let reader = getReader "------------------------627436eaefdbc285" stream
    let request = read reader
    request.Operations
    |> containsOperation "mutation ($file: Upload!) { singleUpload(file: $file) { id } }"
    |> containsFile "file"
    |> hasName "a.txt"
    |> hasContentType "text/plain"
    |> hasContent "Alpha file content.\r\n"
    request.Operations
    |> containsOperation "mutation($files: [Upload!]!) { multipleUpload(files: $files) { id } }"
    |> containsFiles "files"
    |> hasContentTypes [ "text/plain"; "text/plain" ]
    |> hasNames [ "b.txt"; "c.txt" ]
    |> hasContents [ "Bravo file content.\r\n"; "Charlie file content.\r\n" ]
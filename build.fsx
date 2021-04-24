#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.Tools.Git
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.Core.ReleaseNotes
nuget Fake.Core.UserInput
nuget Fake.DotNet.MSBuild
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Fsc
nuget Fake.Api.GitHub
nuget Fake.DotNet.Paket
nuget Octokit
nuget FSharp.Core //"

#if !FAKE
#load ".fake/build.fsx/intellisense.fsx"
#endif

open System
open System.IO
open System.Collections.Generic
open System.Threading
open Fake
open Fake.Tools.Git
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Tools
open Fake.Core
open Fake.Api
open Octokit

// --------------------------------------------------------------------------------------
// Information about the project are used
// --------------------------------------------------------------------------------------
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"


let project = "FSharp.Data.GraphQL"
let summary = "FSharp implementation of Facebook GraphQL query language"
let gitName = "FSharp.Data.GraphQL"
let release = ReleaseNotes.load "RELEASE_NOTES.md"
let projectRepo = "https://github.com/fsprojects/FSharp.Data.GraphQL.git"

// Generate assembly info files with the right version & up-to-date information
Target.create "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ AssemblyInfo.Title projectName
          AssemblyInfo.Product project
          AssemblyInfo.Description summary
          AssemblyInfo.Version release.AssemblyVersion
          AssemblyInfo.FileVersion release.AssemblyVersion ]
    let internalsVisibility (fsproj: string) =
        match fsproj with
        | f when f.EndsWith "FSharp.Data.GraphQL.Shared.fsproj" ->
            [ AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Server"
              AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Client"
              AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Client.DesignTime"
              AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Tests" ]
        | f when f.EndsWith "FSharp.Data.GraphQL.Server.fsproj" ->
            [ AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Benchmarks"
              AssemblyInfo.InternalsVisibleTo "FSharp.Data.GraphQL.Tests" ]
        | _ -> []

    let getProjectDetails (projectPath:string) =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.fsproj"
    //-- "src/FSharp.Data.GraphQL.Client.DesignTime/FSharp.Data.GraphQL.Client.DesignTime.fsproj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, _, folderName, attributes) ->
         AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") (attributes @ internalsVisibility projFileName)
    )
)

// --------------------------------------------------------------------------------------
// Clean build results

Target.create "Clean" (fun _ ->
    Shell.cleanDirs ["bin"; "temp"]
)

Target.create "CleanDocs" (fun _ ->
    Shell.cleanDirs ["docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

// We need to disable parallel restoring of projects to because running paket in parallel from Mono
// is giving errors in Unix based operating systems.
Target.create "Restore" (fun _ ->
    !! "src/**/*.??proj"
    -- "src/**/*.shproj"
    |> Seq.iter (DotNet.restore id))


Target.create "Build" <| fun _ ->
    "FSharp.Data.GraphQL.sln"
    |>  DotNet.build (fun o ->
            { o with Configuration = DotNet.BuildConfiguration.Release })

let startGraphQLServer (project: string) (streamRef: DataRef<Stream>) =
    DotNet.build (fun options ->
        { options with
            Configuration = DotNet.BuildConfiguration.Release}) project

    let projectName = Path.GetFileNameWithoutExtension(project)
    let projectPath = Path.GetDirectoryName(project)
    let serverExe = projectPath </> "bin" </> "Release" </> "net5.0" </> (projectName + ".dll")

    CreateProcess.fromRawCommandLine "dotnet" serverExe
    |> CreateProcess.withStandardInput (CreatePipe streamRef)
    |> Proc.start
    |> ignore

    System.Threading.Thread.Sleep(2000)

let runTests (project : string) =
    DotNet.build (fun options ->
        { options with
            Configuration = DotNet.BuildConfiguration.Release} ) project
    DotNet.test (fun options ->
        { options with
            Configuration = DotNet.BuildConfiguration.Release
            Common = { options.Common with
                        CustomParams = Some "--no-build -v=normal" } }) project

let starWarsServerStream = StreamRef.Empty
Target.create "StartStarWarsServer" (fun _ ->
    Target.activateFinal "StopStarWarsServer"
    let project = "samples" </> "star-wars-api" </> "FSharp.Data.GraphQL.Samples.StarWarsApi.fsproj"
    startGraphQLServer project starWarsServerStream
)

Target.createFinal "StopStarWarsServer" (fun _ ->
    try
        starWarsServerStream.Value.Write([|0uy|],0,1)
    with
    | e -> printfn "%s" e.Message
)


let integrationServerStream = StreamRef.Empty
Target.create "StartIntegrationServer" (fun _ ->
    Target.activateFinal "StopIntegrationServer"
    let project = "tests" </> "FSharp.Data.GraphQL.IntegrationTests.Server" </> "FSharp.Data.GraphQL.IntegrationTests.Server.fsproj"
    startGraphQLServer project integrationServerStream
)

Target.createFinal "StopIntegrationServer" (fun _ ->
    try
        integrationServerStream.Value.Write([|0uy|],0,1)
    with
    | e -> printfn "%s" e.Message
)

Target.create "RunUnitTests" (fun _ ->
    runTests "tests/FSharp.Data.GraphQL.Tests/FSharp.Data.GraphQL.Tests.fsproj"
)

Target.create "RunIntegrationTests" (fun _ ->
    runTests "tests/FSharp.Data.GraphQL.IntegrationTests/FSharp.Data.GraphQL.IntegrationTests.fsproj"
)

let prepareDocGen () =
    Shell.rm "docs/release-notes.md"
    Shell.cp "RELEASE_NOTES.md" "docs/RELEASE_NOTES.md"
    Shell.rename "docs/release-notes.md" "docs/RELEASE_NOTES.md"

    Shell.rm "docs/license.md"
    Shell.cp "LICENSE.txt" "docs/LICENSE.txt"
    Shell.rename "docs/license.md" "docs/LICENSE.txt"

    Shell.cleanDir ".fsdocs"

Target.create "GenerateDocs" (fun _ ->
    prepareDocGen ()
    DotNet.exec id "fsdocs" "build --clean" |> ignore
)

Target.create "GenerateDocsWatch" (fun _ ->
    prepareDocGen ()
    DotNet.exec id "fsdocs" "watch --clean" |> ignore
    System.Console.ReadKey() |> ignore
)

Target.create "ReleaseDocs" (fun _ ->
    Git.Repository.clone "" projectRepo "temp/gh-pages"
    Git.Branches.checkoutBranch "temp/gh-pages" "gh-pages"
    Shell.copyRecursive "output" "temp/gh-pages" true |> printfn "%A"
    Git.CommandHelper.runSimpleGitCommand "temp/gh-pages" "add ." |> printfn "%s"
    let cmd = sprintf """commit -a -m "Update generated documentation for version %s""" release.NugetVersion
    Git.CommandHelper.runSimpleGitCommand "temp/gh-pages" cmd |> printfn "%s"
    Git.Branches.push "temp/gh-pages"
)

let pack id =
    Shell.cleanDir <| sprintf "nuget/%s.%s" project id
    Paket.pack(fun p ->
        { p with
            ToolType = ToolType.CreateLocalTool()
            Version = release.NugetVersion
            OutputPath = sprintf "nuget/%s.%s" project id
            TemplateFile = sprintf "src/%s.%s/%s.%s.fsproj.paket.template" project id project id
            MinimumFromLockFile = true
            IncludeReferencedProjects = false })

let publishPackage id =
    pack id
    Paket.push(fun p ->
        { p with
            ToolType = ToolType.CreateLocalTool()
            WorkingDir = sprintf "nuget/%s.%s" project id
            PublishUrl = "https://www.nuget.org/api/v2/package" })

Target.create "PublishServer" (fun _ ->
    publishPackage "Server"
)

Target.create "PublishClient" (fun _ ->
    publishPackage "Client"
)

Target.create "PublishMiddleware" (fun _ ->
    publishPackage "Server.Middleware"
)

Target.create "PackShared" (fun _ ->
    pack "Shared"
)

Target.create "PackServer" (fun _ ->
    pack "Server"
)

Target.create "PackClient" (fun _ ->
    pack "Client"
)

Target.create "PackMiddleware" (fun _ ->
    pack "Server.Middleware"
)

Target.create "PackAspNet" (fun _ ->
    pack "Server.AspNet"
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build -t <Target>' to override

Target.create "All" ignore
Target.create "PackAll" ignore

"Clean"
  ==> "Restore"
  =?> ("AssemblyInfo", BuildServer.isLocalBuild)
  ==> "Build"
  ==> "RunUnitTests"
  ==> "StartStarWarsServer"
  ==> "StartIntegrationServer"
  ==> "RunIntegrationTests"
  ==> "All"
  =?> ("GenerateDocs", Environment.environVar "APPVEYOR" = "True")

"CleanDocs"
  ==> "GenerateDocs"

"PackShared"
  ==> "PackServer"
  ==> "PackClient"
  ==> "PackMiddleware"
  ==> "PackAspNet"
  ==> "PackAll"

Target.runOrDefault "All"
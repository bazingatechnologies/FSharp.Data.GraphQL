namespace FSharp.Data.GraphQL.IntegrationTests.Server


open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Http.Features
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Server
open FSharp.Data.GraphQL.Server.AspNet

module Program =
    let configureApp (app : IApplicationBuilder) =
        let schema = Schema(Schema.QueryType, Schema.MutationType)
        let executor = Executor(schema)
        let buildRoot ctx = async { return { RequestId = Guid.NewGuid().ToString() } }
        app.UseGraphQL(executor, buildRoot, path="/") |> ignore

    let configureServices (services : IServiceCollection) =
        services.AddDataProtection() |> ignore
        services.Configure(fun (formOptions:FormOptions) ->
            formOptions.MultipartBodyLengthLimit <- 2_147_483_648L
            formOptions.ValueLengthLimit <- Int32.MaxValue
            formOptions.BufferBodyLengthLimit <- 2147483648L
        ) |> ignore

    let configureLogging (loggerBuilder : ILoggingBuilder) =
        loggerBuilder.AddFilter(fun lvl -> lvl.Equals LogLevel.Error)
                     .AddConsole()
                     .AddDebug() |> ignore

    [<EntryPoint>]
    let main _ =
        WebHost
            .CreateDefaultBuilder()
            .Configure(Action<IApplicationBuilder> configureApp)
            .ConfigureServices(configureServices)
            .ConfigureLogging(configureLogging)
            .UseKestrel(fun options ->
                options.ListenAnyIP(8085)
                options.Limits.MaxRequestBodySize <- Nullable 2147483648L
                options.Limits.MaxRequestBufferSize <- Nullable 2147483648L
            )
            .Build()
            .Run()
        0

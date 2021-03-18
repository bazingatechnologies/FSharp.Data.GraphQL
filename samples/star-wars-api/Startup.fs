namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open Microsoft.AspNetCore.Authorization
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Giraffe
open FSharp.Data.GraphQL.Samples.StarWarsApi.Authorization

type Startup private () =
    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    member __.ConfigureServices(services: IServiceCollection) =
        services.AddAuthorization(fun options ->
                    options.AddPolicy(Policies.CanSetMoon, fun policy -> policy.Requirements.Add(IsCharacterRequierment (Seq.singleton "droid"))))
                .AddScoped<IAuthorizationHandler, IsCharacterHandler>()
        |> ignore

        services.AddGiraffe()
                .Configure(Action<KestrelServerOptions>(fun x -> x.AllowSynchronousIO <- true))
                .Configure(Action<IISServerOptions>(fun x -> x.AllowSynchronousIO <- true))
        |> ignore

    member __.Configure(app: IApplicationBuilder) =
        let errorHandler (ex : Exception) (log : ILogger) =
            log.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
            clearResponse >=> setStatusCode 500
        app
            .UseGiraffeErrorHandler(errorHandler)
            .UseWebSockets()
            .UseMiddleware<GraphQLWebSocketMiddleware<Root>>(Schema.executor, fun () -> { RequestId = Guid.NewGuid().ToString(); ServiceProvider = app.ApplicationServices })
            .UseGiraffe HttpHandlers.webApp

    member val Configuration : IConfiguration = null with get, set

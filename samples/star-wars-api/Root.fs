namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open FSharp.Data.GraphQL.Types

type Root =
    { RequestId: string
      ServiceProvider: IServiceProvider }

module Root =
    type ResolveFieldContext with
         member this.Root : Root = downcast this.Context.RootValue

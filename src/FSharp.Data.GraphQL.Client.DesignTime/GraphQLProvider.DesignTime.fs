/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open FSharp.Data.GraphQL

[<TypeProvider>]
type GraphQLTypeProvider (config) as this =
    inherit TypeProviderForNamespaces(config,
                                      assemblyReplacementMap = [
                                          "FSharp.Data.GraphQL.Client.DesignTime", "FSharp.Data.GraphQL.Client"
                                        ],
                                      addDefaultProbingLocation = true)

    let ns = "FSharp.Data.GraphQL"
    let asm = Assembly.GetExecutingAssembly()
    let provider = Provider.makeGraphQLProvider(asm, ns, config.ResolutionFolder)
    do this.AddNamespace(ns, [provider])

[<assembly:TypeProviderAssembly>]
do()
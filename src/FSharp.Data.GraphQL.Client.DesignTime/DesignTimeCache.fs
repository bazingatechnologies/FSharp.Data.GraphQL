/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

#if IS_DESIGNTIME

open System
open FSharp.Data.GraphQL.Client
open ProviderImplementation.ProvidedTypes
open FSharp.Data.GraphQL.Validation

type internal ProviderSettings =
    { IntrospectionLocation : IntrospectionLocation
      CustomHttpHeadersLocation : StringLocation
      UploadInputTypeName : string option
      ResolutionFolder : string
      ClientQueryValidation: bool
      ExplicitOptionalParameters: bool
      FragmentsFolder: string option }

module internal ProviderDesignTimeCache =
    let private expiration = CacheExpirationPolicy.SlidingExpiration(TimeSpan.FromSeconds 30.0)
    let private cache = MemoryCache<ProviderSettings, ProvidedTypeDefinition>(expiration)
    let getOrAdd (key : ProviderSettings) (defMaker : unit -> ProvidedTypeDefinition) =
        cache.GetOrAddResult key defMaker

module internal QueryValidationDesignTimeCache =
    let cache : IValidationResultCache = upcast MemoryValidationResultCache()
    let getOrAdd (key : ValidationResultKey) (resMaker : unit -> ValidationResult<AstError>) =
        cache.GetOrAdd resMaker key

#endif

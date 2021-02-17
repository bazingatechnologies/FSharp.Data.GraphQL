/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System.Collections.Generic
open System.Reactive.Linq
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection
open FSharp.Data.GraphQL.Helpers
open System.Reactive.Subjects

type private Channel = ISubject<obj>

type private ChannelBag() =
    let untagged = List<Channel>()
    let tagged = Dictionary<Tag, List<Channel>>()
    member __.AddNew(tags : Tag seq) : Channel =
        let channel = new Subject<obj>()
        untagged.Add(channel)
        let adder tag =
            match tagged.TryGetValue(tag) with
            | true, channels -> 
                channels.Add(channel)
            | false, _ -> 
                let channels = List<Channel>()
                channels.Add(channel)
                tagged.Add(tag, channels)
        tags |> Seq.iter adder
        upcast channel
    member __.GetAll() =
        untagged |> Seq.map id
    member __.GetByTag(tag) =
        match tagged.TryGetValue(tag) with
        | false, _ -> Seq.empty
        | true, channels -> channels |> Seq.map id

type private SubscriptionManager() =
    let subscriptions = Dictionary<string, Subscription * ChannelBag>()
    member __.Add(subscription : Subscription) =
        subscriptions.Add(subscription.Name, (subscription, new ChannelBag()))
    member __.TryGet(subscriptionName) =
        match subscriptions.TryGetValue(subscriptionName) with
        | true, sub -> Some sub
        | _ -> None

/// A configuration object fot the GraphQL server schema.
type SchemaConfig =
    { /// List of types that couldn't be resolved from schema query root
      /// tree traversal, but should be included anyway.
      Types : NamedDef list
      /// List of custom directives that should be included as known to the schema.
      Directives : DirectiveDef list
      /// Function called when errors occurred during query execution.
      /// It's used to retrieve messages shown as output to the client.
      /// May be also used to log messages before returning them.
      ParseError : exn -> string
      /// Provider for the back-end of the subscription system.
      SubscriptionProvider : ISubscriptionProvider
      /// Provider for the back-end of the live query subscription system.
      LiveFieldSubscriptionProvider : ILiveFieldSubscriptionProvider
    }
    /// Returns the default Subscription Provider, backed by Observable streams.
    static member DefaultSubscriptionProvider() =
        let subscriptionManager = new SubscriptionManager()
        { new ISubscriptionProvider with
            member __.AsyncRegister (subscription : Subscription) = async {
                return subscriptionManager.Add(subscription) }

            member __.Add (ctx: ResolveFieldContext) (root: obj) (subdef: SubscriptionFieldDef) =
                let tags = subdef.TagsResolver ctx
                match subscriptionManager.TryGet(subdef.Name) with
                | Some (sub, channels) -> 
                    channels.AddNew(tags)
                    |> Observable.mapAsync (fun o -> sub.Filter ctx root o)
                    |> Observable.choose id
                | None -> Observable.Empty()

            member __.AsyncPublish<'T> (name: string) (value: 'T) = async {
                match subscriptionManager.TryGet(name) with
                | Some (_, channels) -> channels.GetAll() |> Seq.iter (fun channel -> channel.OnNext(value))
                | None -> () }
                
            member __.AsyncPublishTag<'T> (name: string) (tag : Tag) (value: 'T) = async {
                match subscriptionManager.TryGet(name) with
                | Some (_, channels) -> channels.GetByTag(tag) |> Seq.iter (fun channel -> channel.OnNext(value))
                | None -> () } }

    /// Returns the default live field Subscription Provider, backed by Observable streams.
    static member DefaultLiveFieldSubscriptionProvider() =
        let registeredSubscriptions = new Dictionary<string * string, ILiveFieldSubscription * Subject<obj>>()
        { new ILiveFieldSubscriptionProvider with
            member __.HasSubscribers (typeName : string) (fieldName : string) =
                let key = typeName, fieldName
                match registeredSubscriptions.TryGetValue(key) with
                | true, (_, channel) -> channel.HasObservers
                | _ -> false
            member __.IsRegistered (typeName : string) (fieldName : string) =
                let key = typeName, fieldName
                registeredSubscriptions.ContainsKey(key)
            member __.AsyncRegister (subscription : ILiveFieldSubscription) = async {
                let key = subscription.TypeName, subscription.FieldName
                let value = subscription, new Subject<obj>()
                registeredSubscriptions.Add(key, value) }
            member __.TryFind (typeName : string) (fieldName : string) =
                let key = typeName, fieldName
                match registeredSubscriptions.TryGetValue(key) with
                | (true, (sub, _)) -> Some sub
                | _ -> None
            member __.Add filterFn (typeName : string) (fieldName : string) =
                let key = typeName, fieldName
                match registeredSubscriptions.TryGetValue(key) with
                | true, (sub, channel) -> channel |> Observable.choose (fun x -> if filterFn x then Some (sub.Project x) else None)
                | false, _ -> Observable.Empty()
            member __.AsyncPublish<'T> (typeName : string) (fieldName : string) (value : 'T) = async {
                let key = typeName, fieldName
                match registeredSubscriptions.TryGetValue(key) with
                | true, (_, channel) -> channel.OnNext(box value)
                | false, _ -> () } }
    /// Default SchemaConfig used by Schema when no config is provided.
    static member Default =
        { Types = []
          Directives = [ IncludeDirective; SkipDirective; DeferDirective; StreamDirective; LiveDirective ]
          ParseError = fun e -> e.Message
          SubscriptionProvider = SchemaConfig.DefaultSubscriptionProvider()
          LiveFieldSubscriptionProvider = SchemaConfig.DefaultLiveFieldSubscriptionProvider() }
    /// <summary>
    /// Default SchemaConfig with buffered stream support.
    /// This config modifies the stream directive to have two optional arguments: 'interval' and 'preferredBatchSize'.
    /// This argument will allow the user to buffer streamed results in a query by timing and/or batch size preferences.
    /// </summary>
    /// <param name="streamOptions">
    /// The way the buffered stream will behavior by default - standard values for the 'interval' and 'preferredBatchSize' arguments.
    /// </param>
    static member DefaultWithBufferedStream(streamOptions : BufferedStreamOptions) =
        let streamDirective =
            let args = [|
                Define.Input(
                    "interval", 
                    Nullable Int, 
                    defaultValue = streamOptions.Interval,
                    description = "An optional argument used to buffer stream results. " +
                        "When it's value is greater than zero, stream results will be buffered for milliseconds equal to the value, then sent to the client. " +
                        "After that, starts buffering again until all results are streamed.")
                Define.Input(
                    "preferredBatchSize", 
                    Nullable Int, 
                    defaultValue = streamOptions.PreferredBatchSize,
                    description = "An optional argument used to buffer stream results. " +
                        "When it's value is greater than zero, stream results will be buffered until item count reaches this value, then sent to the client. " +
                        "After that, starts buffering again until all results are streamed.") |]
            { StreamDirective with Args = args }
        { SchemaConfig.Default with
            Directives = [ IncludeDirective; SkipDirective; DeferDirective; streamDirective; LiveDirective ] }

/// GraphQL server schema. Defines the complete type system to be used by GraphQL queries.
type Schema<'Root> (query: ObjectDef<'Root>, ?mutation: ObjectDef<'Root>, ?subscription: SubscriptionObjectDef<'Root>, ?config: SchemaConfig) =
    let initialTypes: NamedDef list =
        [ Int
          String
          Boolean
          Float
          ID
          Date
          Uri
          __Schema
          query ]

    let schemaConfig =
        match config with
        | None -> SchemaConfig.Default
        | Some c -> c

    let typeMap : TypeMap =
        let m = mutation |> function Some (Named n) -> [n] | _ -> []
        let s = subscription |> function Some (Named n) -> [n] | _ -> []
        initialTypes @ s @ m @ schemaConfig.Types |> TypeMap.FromSeq
           
    let rec resolveTypeReference (typeDef: TypeDef): TypeDef =
        match typeDef with
        | :? ObjectRef as objRef ->

            match typeMap.TryFind<ObjectDef> objRef.Name with
            | Some realDef ->
                upcast realDef // TODO: resolveObjectReferences here too

            | None ->
                failwithf "Referenced type %s not found. Please provide referenced types through SchemaConfig.Types."
                    objRef.Name

        //        | :? ObjectDef as objDef ->
        //            upcast resolveObjectReferences (downcast objDef)

        | :? NullableDef as nullDef ->
            upcast (resolveTypeReference nullDef.OfType).MakeNullable()

        | :? ListOfDef as listOfDef ->
            upcast (resolveTypeReference listOfDef.OfType).MakeList()

        | _ ->
            typeDef

    and resolveObjectReferences (objDef: ObjectDef<'a>): ObjectDef<'a> =
        objDef.WithFields <|
            Seq.map (fun (KeyValue (_, field)) ->
                upcast CustomOutputFieldDefinition(field, downcast resolveTypeReference field.TypeDef)
            ) objDef.Fields

        
        
    let query = resolveObjectReferences query
    let mutation = Option.map resolveObjectReferences mutation

    let getImplementations (typeMap : TypeMap) =
        typeMap.ToSeq()
        |> Seq.choose (fun (_, v) ->
            match v with
            | Object odef -> Some odef
            | _ -> None)
        |> Seq.fold (fun acc objdef ->
            objdef.Implements
            |> Array.fold (fun acc' iface ->
                match Map.tryFind iface.Name acc' with
                | Some list -> Map.add iface.Name (objdef::list) acc'
                | None -> Map.add iface.Name [objdef] acc') acc) Map.empty

    let implementations = lazy (getImplementations typeMap)

    let getPossibleTypes abstractDef =
        match abstractDef with
        | Union u -> u.Options
        | Interface i -> Map.find i.Name (implementations.Force()) |> Array.ofList
        | _ -> [||]

    let rec introspectTypeRef isNullable (namedTypes: Map<string, IntrospectionTypeRef>) typedef =
        match typedef with
        | Nullable inner -> introspectTypeRef true namedTypes inner
        | List inner ->
            if isNullable
            then IntrospectionTypeRef.List(introspectTypeRef false namedTypes inner)
            else IntrospectionTypeRef.NonNull(introspectTypeRef true namedTypes typedef)
        | Named named ->
            if isNullable
            then Map.find named.Name namedTypes
            else IntrospectionTypeRef.NonNull(introspectTypeRef true namedTypes typedef)
        | _ -> failwithf "Unexpected value of typedef: %O" typedef

    let introspectInput (namedTypes: Map<string, IntrospectionTypeRef>) (inputDef: InputFieldDef) : IntrospectionInputVal =
        // We need this so a default value that is an option is not printed as "Some"
        let stringify = 
            function
            | ObjectOption x -> string x
            | x -> string x
        { Name = inputDef.Name
          Description = inputDef.Description
          Type = introspectTypeRef (Option.isSome inputDef.DefaultValue) namedTypes inputDef.TypeDef
          DefaultValue = inputDef.DefaultValue |> Option.map stringify }

    let introspectField (namedTypes: Map<string, IntrospectionTypeRef>) (fdef: FieldDef) =
        { Name = fdef.Name
          Description = fdef.Description
          Args = fdef.Args |> Array.map (introspectInput namedTypes)
          Type = introspectTypeRef false namedTypes fdef.TypeDef
          IsDeprecated = Option.isSome fdef.DeprecationReason
          DeprecationReason = fdef.DeprecationReason }

    let instrospectSubscriptionField (namedTypes: Map<string, IntrospectionTypeRef>) (subdef: SubscriptionFieldDef) =
        { Name = subdef.Name
          Description = subdef.Description
          Args = subdef.Args |> Array.map (introspectInput namedTypes) 
          Type = introspectTypeRef false namedTypes subdef.OutputTypeDef
          IsDeprecated = Option.isSome subdef.DeprecationReason
          DeprecationReason = subdef.DeprecationReason }

    let introspectEnumVal (enumVal: EnumVal) : IntrospectionEnumVal =
        { Name = enumVal.Name
          Description = enumVal.Description
          IsDeprecated = Option.isSome enumVal.DeprecationReason
          DeprecationReason = enumVal.DeprecationReason }

    let locationToList location =
        System.Enum.GetValues(typeof<DirectiveLocation>)
        |> Seq.cast<DirectiveLocation>
        |> Seq.filter (fun v -> int(location) &&& int(v) <> 0)
        |> Seq.toArray

    let introspectDirective (namedTypes: Map<string, IntrospectionTypeRef>) (directive: DirectiveDef) : IntrospectionDirective =
        { Name = directive.Name
          Description = directive.Description
          Locations = locationToList directive.Locations
          Args = directive.Args |> Array.map (introspectInput namedTypes) }

    let introspectType (namedTypes: Map<string, IntrospectionTypeRef>) typedef =
        match typedef with
        | Scalar scalardef ->
            IntrospectionType.Scalar(scalardef.Name, scalardef.Description)
        | SubscriptionObject subdef ->
            let fields =
                subdef.Fields
                |> Map.toArray
                |> Array.map (snd >> instrospectSubscriptionField namedTypes)
            let interfaces =
                subdef.Implements
                |> Array.map (fun idef -> Map.find idef.Name namedTypes)
            IntrospectionType.Object(subdef.Name, subdef.Description, fields, interfaces)
        | Object objdef ->
            let fields =
                objdef.Fields
                |> Map.toArray
                |> Array.map (snd >> introspectField namedTypes)
            let interfaces =
                objdef.Implements
                |> Array.map (fun idef -> Map.find idef.Name namedTypes)
            IntrospectionType.Object(objdef.Name, objdef.Description, fields, interfaces)
        | InputObject inObjDef ->
            let inputs =
                inObjDef.Fields
                |> Array.map (introspectInput namedTypes)
            IntrospectionType.InputObject(inObjDef.Name, inObjDef.Description, inputs)
        | Union uniondef ->
            let possibleTypes =
                getPossibleTypes uniondef
                |> Array.map (fun tdef -> Map.find tdef.Name namedTypes)
            IntrospectionType.Union(uniondef.Name, uniondef.Description, possibleTypes)
        | Enum enumdef ->
            let enumVals =
                enumdef.Options
                |> Array.map introspectEnumVal
            IntrospectionType.Enum(enumdef.Name, enumdef.Description, enumVals)
        | Interface idef ->
            let fields =
                idef.Fields
                |> Array.map (introspectField namedTypes)
            let possibleTypes =
                getPossibleTypes idef
                |> Array.map (fun tdef -> Map.find tdef.Name namedTypes)
            IntrospectionType.Interface(idef.Name, idef.Description, fields, possibleTypes)
        | _ -> failwithf "Unexpected value of typedef: %O" typedef

    let introspectSchema (types : TypeMap) : IntrospectionSchema =
        let inamed =
            types.ToSeq()
            |> Seq.map (fun (typeName, typedef) ->
                match typedef with
                | Scalar x -> typeName, { Kind = TypeKind.SCALAR; Name = Some typeName; Description = x.Description; OfType = None }
                | Object x -> typeName, { Kind = TypeKind.OBJECT; Name = Some typeName; Description = x.Description; OfType = None }
                | InputObject x -> typeName, { Kind = TypeKind.INPUT_OBJECT; Name = Some typeName; Description = x.Description; OfType = None }
                | Union x -> typeName, { Kind = TypeKind.UNION; Name = Some typeName; Description = x.Description; OfType = None }
                | Enum x -> typeName, { Kind = TypeKind.ENUM; Name = Some typeName; Description = x.Description; OfType = None }
                | Interface x -> typeName, { Kind = TypeKind.INTERFACE; Name = Some typeName; Description = x.Description; OfType = None }
                | _ -> failwithf "Unexpected value of typedef: %O" typedef)
            |> Map.ofSeq
        let itypes =
            types.ToSeq()
            |> Seq.toArray
            |> Array.map (snd >> (introspectType inamed))
        let idirectives =
            schemaConfig.Directives
            |> List.map (introspectDirective inamed)
            |> List.toArray
        { QueryType = Map.find query.Name inamed
          MutationType = mutation |> Option.map (fun m -> Map.find m.Name inamed)
          SubscriptionType = subscription |> Option.map(fun s -> Map.find s.Name inamed)
          Types = itypes
          Directives = idirectives }

    let introspected = lazy (introspectSchema typeMap)

    interface ISchema with
        member __.TypeMap = typeMap
        member __.Directives = schemaConfig.Directives |> List.toArray
        member __.Introspected = introspected.Force()
        member __.Query = upcast query
        member __.Mutation = mutation |> Option.map (fun x -> upcast x)
        member __.Subscription = subscription |> Option.map (fun x -> upcast x)
        member __.TryFindType typeName = typeMap.TryFind(typeName, includeDefaultTypes = true)
        member __.GetPossibleTypes typedef = getPossibleTypes typedef
        member __.ParseError exn = schemaConfig.ParseError exn
        member x.IsPossibleType abstractdef (possibledef: ObjectDef) =
            match (x :> ISchema).GetPossibleTypes abstractdef with
            | [||] -> false
            | possibleTypes -> possibleTypes |> Array.exists (fun t -> t.Name = possibledef.Name)
        member __.SubscriptionProvider = schemaConfig.SubscriptionProvider
        member __.LiveFieldSubscriptionProvider = schemaConfig.LiveFieldSubscriptionProvider

    interface ISchema<'Root> with
        member __.Query = query
        member __.Mutation = mutation
        member __.Subscription = subscription

    interface System.Collections.Generic.IEnumerable<NamedDef> with
        member __.GetEnumerator() = (typeMap.ToSeq() |> Seq.map snd).GetEnumerator()

    interface System.Collections.IEnumerable with
        member __.GetEnumerator() = (typeMap.ToSeq() |> Seq.map snd :> System.Collections.IEnumerable).GetEnumerator()

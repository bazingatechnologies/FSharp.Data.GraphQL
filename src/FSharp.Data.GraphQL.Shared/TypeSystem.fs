/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
namespace FSharp.Data.GraphQL.Types

open System
open System.Reflection
open System.Collections
open System.Collections.Concurrent
open System.Collections.Generic
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Extensions
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Reflection
open FSharp.Linq.RuntimeHelpers

/// Enum describing parts of the GraphQL query document AST, where
/// related directive is valid to be used.
[<Flags>]
type DirectiveLocation =
    | QUERY = 1
    | MUTATION = 2
    | SUBSCRIPTION = 4
    | FIELD = 8
    | FRAGMENT_DEFINITION = 16
    | FRAGMENT_SPREAD = 32
    | INLINE_FRAGMENT = 64
    | SCHEMA = 128
    | SCALAR = 256
    | OBJECT = 512
    | FIELD_DEFINITION = 1024
    | ARGUMENT_DEFINITION = 2048
    | INTERFACE = 4096
    | UNION = 8192
    | ENUM = 16384
    | ENUM_VALUE = 32768
    | INPUT_OBJECT = 65536
    | INPUT_FIELD_DEFINITION = 131072

module Introspection =

    /// Type kind. GraphQL type system puts all types into one of eight categories.
    type TypeKind =
        | SCALAR = 1
        | OBJECT  = 2
        | INTERFACE = 3
        | UNION = 4
        | ENUM = 5
        | INPUT_OBJECT = 6
        | LIST = 7
        | NON_NULL = 8

    /// Introspection descriptor of a directive (i.e. @skip(if:...), @include(if:...) etc).
    type IntrospectionDirective =
        { /// Directive name.
          Name : string
          /// Description of a target directive.
          Description : string option
          /// Array of AST locations, where it's valid to place target directive.
          Locations : DirectiveLocation []
          /// Array of arguments, current directive can be parametrized with.
          Args : IntrospectionInputVal [] }

    /// Introspection descriptor of a GraphQL type defintion.
    and IntrospectionType =
        { /// Which kind category current type belongs to.
          Kind : TypeKind
          /// Type name. Must be unique in scope of the defined schema.
          Name : string
          /// Optional type description.
          Description : string option
          /// Array of field descriptors defined within current type.
          /// Only present for Object and Interface types.
          Fields : IntrospectionField [] option
          /// Array of interfaces implemented by output object type defintion.
          Interfaces : IntrospectionTypeRef [] option
          /// Array of type references being possible implementation of current type.
          /// Only present for Union types (list of union cases) and Interface types
          /// (list of all objects implementing interface in scope of the schema).
          PossibleTypes : IntrospectionTypeRef [] option
          /// Array of enum values defined by current Enum type.
          EnumValues : IntrospectionEnumVal [] option
          /// Array of input fields defined by current InputObject type.
          InputFields : IntrospectionInputVal [] option
          /// Type param reference - used only by List and NonNull types.
          OfType : IntrospectionTypeRef option }

        /// <summary>
        /// Constructs an introspection descriptor for a <see cref="TypeKind.SCALAR"/> types.
        /// </summary>
        /// <param name="name">Type name (unique in the scope of current schema).</param>
        /// <param name="description">Optional type description.</param>
        static member Scalar(name : string, description : string option) =
            { Kind = TypeKind.SCALAR
              Name = name
              Description = description
              Fields = None
              Interfaces = None
              PossibleTypes = None
              EnumValues = None
              InputFields = None
              OfType = None }

        /// <summary>
        /// Constructs an introspection descriptor for a <see cref="TypeKind.OBJECT"/> types.
        /// </summary>
        /// <param name="name">Type name (unique in the scope of current schema).</param>
        /// <param name="description">Optional type description.</param>
        /// <param name="fields">Array of fields defined in current object.</param>
        /// <param name="interfaces">Array of interfaces, current object implements.</param>
        static member Object(name : string, description : string option, fields : IntrospectionField [],
                             interfaces : IntrospectionTypeRef []) =
            { Kind = TypeKind.OBJECT
              Name = name
              Description = description
              Fields = Some fields
              Interfaces = Some interfaces
              PossibleTypes = None
              EnumValues = None
              InputFields = None
              OfType = None }

        /// <summary>
        /// Constructs an introspection descriptor for a <see cref="TypeKind.INPUT_OBJECT"/> types.
        /// </summary>
        /// <param name="name">Type name (unique in the scope of current schema).</param>
        /// <param name="description">Optional type description.</param>
        /// <param name="inputFields">Array of input fields defined in current input object.</param>
        static member InputObject(name : string, description : string option, inputFields : IntrospectionInputVal []) =
            { Kind = TypeKind.INPUT_OBJECT
              Name = name
              Description = description
              Fields = None
              Interfaces = None
              PossibleTypes = None
              EnumValues = None
              InputFields = Some inputFields
              OfType = None }

        /// <summary>
        /// Constructs an introspection descriptor for a <see cref="TypeKind.UNION"/> types.
        /// </summary>
        /// <param name="name">Type name (unique in the scope of current schema).</param>
        /// <param name="description">Optional type description.</param>
        /// <param name="possibleTypes">Array of union case types. They can be any type defined in GraphQL schema.</param>
        static member Union(name : string, description : string option, possibleTypes : IntrospectionTypeRef []) =
            { Kind = TypeKind.UNION
              Name = name
              Description = description
              Fields = None
              Interfaces = None
              PossibleTypes = Some possibleTypes
              EnumValues = None
              InputFields = None
              OfType = None }

        /// <summary>
        /// Constructs an introspection descriptor for a <see cref="TypeKind.ENUM"/> types.
        /// </summary>
        /// <param name="name">Type name (unique in the scope of current schema).</param>
        /// <param name="description">Optional type description.</param>
        /// <param name="enumValues">Array of enum value descriptors.</param>
        static member Enum(name : string, description : string option, enumValues : IntrospectionEnumVal []) =
            { Kind = TypeKind.ENUM
              Name = name
              Description = description
              Fields = None
              Interfaces = None
              PossibleTypes = None
              EnumValues = Some enumValues
              InputFields = None
              OfType = None }

        /// <summary>
        /// Constructs an introspection descriptor for a <see cref="TypeKind.INTERFACE"/> types.
        /// </summary>
        /// <param name="name">Type name (unique in the scope of current schema).</param>
        /// <param name="description">Optional type description.</param>
        /// <param name="fields">Array of fields being part of the interface contract.</param>
        /// <param name="possibleTypes">Array of schema objects implementing target interface.</param>
        static member Interface(name : string, description : string option, fields : IntrospectionField [],
                                possibleTypes : IntrospectionTypeRef []) =
            { Kind = TypeKind.INTERFACE
              Name = name
              Description = description
              Fields = Some fields
              Interfaces = None
              PossibleTypes = Some possibleTypes
              EnumValues = None
              InputFields = None
              OfType = None }

    /// Introspection type reference. Used to navigate between type dependencies inside introspected schema.
    and IntrospectionTypeRef =
        { /// Referenced type kind.
          Kind : TypeKind
          /// Type name. None if referenced type is List or NonNull.
          Name : string option
          /// Optional type description.
          Description : string option
          /// Type param reference. Used only by List and NonNull types.
          OfType : IntrospectionTypeRef option }

        /// <summary>
        /// Constructs an introspection type reference for List types.
        /// </summary>
        /// <param name="inner">Type reference for type used as List's type param.</param>
        static member List(inner : IntrospectionTypeRef) =
            { Kind = TypeKind.LIST
              Name = None
              Description = None
              OfType = Some inner }

        /// <summary>
        /// Constructs an introspection type reference for NonNull types.
        /// </summary>
        /// <param name="inner">Type reference for type used as NonNull's type param.</param>
        static member NonNull(inner : IntrospectionTypeRef) =
            { Kind = TypeKind.NON_NULL
              Name = None
              Description = None
              OfType = Some inner }

        /// <summary>
        /// Constructs an introspection type reference for any named type defintion
        /// (any type other than List or NonNull) with unique name included.
        /// </summary>
        /// <param name="inner">Introspection type descriptor to construct reference from.</param>
        static member Named(inner : IntrospectionType) =
            { Kind = inner.Kind
              Name = Some inner.Name
              Description = inner.Description
              OfType = None }

    /// Introspection descriptor for input values (InputObject fields or field arguments).
    and IntrospectionInputVal =
        { /// Input argument name.
          Name : string
          /// Optional input argument description.
          Description : string option
          /// Introspection reference to argument's type.
          Type : IntrospectionTypeRef
          /// Default arguments value, if provided.
          DefaultValue : string option }

    /// Introspection descriptor for enum values.
    and IntrospectionEnumVal =
        { /// Enum value name - must be unique in scope of defining enum.
          Name : string
          /// Optional enum value description.
          Description : string option
          /// If true, marks current value as deprecated, but still
          /// available for compatibility reasons.
          IsDeprecated : bool
          /// If value is deprecated this field may describe a deprecation reason.
          DeprecationReason : string option }

    /// Introspection descriptor for Object and Interface fields.
    and IntrospectionField =
        { /// Field name. Must be unique in scope of the definin object/interface.
          Name : string
          /// Optional field description.
          Description : string option
          /// Array of field arguments. In GraphQL fields can be parametrized,
          /// working effectively like methods.
          Args : IntrospectionInputVal []
          /// Introspection reference to field's type.
          Type : IntrospectionTypeRef
          /// If true, marks current field as deprecated, but still
          /// available for compatibility reasons.
          IsDeprecated : bool
          /// If field is deprecated here a deprecation reason may be set.
          DeprecationReason : string option }

    /// Introspection descriptor for target schema. Contains informations about
    /// all types defined within current schema.
    and IntrospectionSchema =
        { /// Introspection reference to schema's query root.
          QueryType : IntrospectionTypeRef
          /// Introspection reference to schema's mutation root.
          MutationType : IntrospectionTypeRef option
          /// Introspection reference to schema's subscription root.
          SubscriptionType : IntrospectionTypeRef option
          /// Array of all introspection types defined within current schema.
          /// Includes types for queries, mutations and subscriptions.
          Types : IntrospectionType []
          /// Array of all directives supported by current schema.
          Directives : IntrospectionDirective [] }

/// Represents a subscription as described in the schema.
type Subscription = {
    /// The name of the subscription type in the schema.
    Name : string
    /// Filter function, used to determine what events we will propagate.
    /// The first object is the boxed root value, the second is the boxed value of the input object.
    Filter : ResolveFieldContext -> obj -> obj -> Async<obj option> }

/// Describes the backing implementation for a subscription system.
and ISubscriptionProvider =
    interface
        /// Registers a new subscription type, called at schema compilation time.
        abstract member AsyncRegister : Subscription -> Async<unit>
        /// Creates an active subscription, and returns the IObservable stream of POCO objects that will be projected on.
        abstract member Add : ResolveFieldContext -> obj -> SubscriptionFieldDef -> IObservable<obj>
        /// Publishes an event to the subscription system given the identifier of the subscription type.
        abstract member AsyncPublish<'T> : string -> 'T -> Async<unit>
        /// Publishes an event to the subscription system given the identifier of the subscription type
        /// and a filter identity that can be used to choose which filter functions will be applied.
        abstract member AsyncPublishTag<'T> : string -> Tag -> 'T -> Async<unit>
    end

/// Represents a subscription of a field in a live query.
and ILiveFieldSubscription =
    interface
        /// Determine if we should propagate the event
        abstract member Filter : obj -> obj -> bool
        /// Project out the field marked with the @live directive
        abstract member Project : obj -> obj
        /// The type name of the object that is ready for live query in this subscription.
        abstract member TypeName : string
        /// The field name of the object that is ready for live query in this subscription.
        abstract member FieldName : string
    end

/// Represents a generic typed, subscription field in a live query.
and ILiveFieldSubscription<'Object, 'Field> =
    interface
        inherit ILiveFieldSubscription
        /// Determine if we should propagate the event
        abstract member Filter : 'Object -> 'Object -> bool
        /// Project out the field marked with the @live directive
        abstract member Project : 'Object -> 'Field
    end

/// Represents a subscription of a field in a live query.
and LiveFieldSubscription =
    { /// Determine if we should propagate the event
      Filter : obj -> obj -> bool
      /// Project out the field marked with the @live directive
      Project : obj -> obj
      /// The type name of the object that is ready for live query in this subscription.
      TypeName : string
      /// The field name of the object that is ready for live query in this subscription.
      FieldName : string }
    interface ILiveFieldSubscription with
        member this.Filter x y = this.Filter x y
        member this.Project x = this.Project x
        member this.TypeName = this.TypeName
        member this.FieldName = this.FieldName

/// Represents a generic typed, subscription field in a live query.
and LiveFieldSubscription<'Object, 'Field> =
    { /// Determine if we should propagate the event
      Filter : 'Object -> 'Object -> bool
      /// Project out the field marked with the @live directive
      Project : 'Object -> 'Field
      /// The type name of the object that is ready for live query in this subscription.
      TypeName : string
      /// The field name of the object that is ready for live query in this subscription.
      FieldName : string }
    interface ILiveFieldSubscription<'Object, 'Field> with
        member this.Filter x y = this.Filter x y
        member this.Project x = this.Project x
    interface ILiveFieldSubscription with
        member this.Filter x y = this.Filter (downcast x) (downcast y)
        member this.Project x = upcast this.Project (downcast x)
        member this.TypeName = this.TypeName
        member this.FieldName = this.FieldName

/// Describes the backing implementation of a live query subscription system.
and ILiveFieldSubscriptionProvider =
    interface
        /// Checks if a live field subscription has subscribers.
        abstract member HasSubscribers : string -> string -> bool
        /// Checks if a type and a field is registered in the provider.
        abstract member IsRegistered : string -> string -> bool
        /// Registers a new live query subscription type, called at schema compilation time.
        abstract member AsyncRegister : ILiveFieldSubscription -> Async<unit>
        /// Tries to find a subscription based on the type name and field name.
        abstract member TryFind : string -> string -> ILiveFieldSubscription option
        /// Creates an active subscription, and returns the IObservable stream of projected POCO objects
        abstract member Add : (obj -> bool) -> string -> string -> IObservable<obj>
        /// Publishes an event to the subscription system, given the key of the subscription type.
        abstract member AsyncPublish<'T> : string -> string -> 'T -> Async<unit>
    end

/// Interface used for receiving information about a whole
/// schema and type system defined within it.
and ISchema =
    interface
        inherit seq<NamedDef>

        /// Map of defined types by their names.
        abstract TypeMap : TypeMap

        /// A query root object. Defines all top level fields,
        /// that can be accessed from GraphQL queries.
        abstract Query : ObjectDef

        /// A mutation root object. Defines all top level operations,
        /// that can be performed from GraphQL mutations.
        abstract Mutation : ObjectDef option

        // A subscription root object. Defines all top level operations,
        // that can be performed from GraphQL subscriptions.
        abstract Subscription : SubscriptionObjectDef option

        /// List of all directives supported by the current schema.
        abstract Directives : DirectiveDef []

        /// Method which, given type name, returns Some if provided
        /// type has been defined in current schema. Otherwise None.
        abstract TryFindType : string -> NamedDef option

        /// Returns array of all possible types for provided abstract
        /// type. For Union types, it's the array of all union options.
        /// For Interface types, it's an array of all types - within
        /// schema - implementing target interface.
        abstract GetPossibleTypes : TypeDef -> ObjectDef []

        /// Checks if provided object is a possible type type (case
        /// for Unions and implementation for Interfaces) of provided
        /// abstract type.
        abstract IsPossibleType : AbstractDef -> ObjectDef -> bool

        /// Returns an introspected representation of current schema.
        abstract Introspected : Introspection.IntrospectionSchema

        /// Returns a function called when errors occurred during query execution.
        /// It's used to retrieve messages shown as output to the client.
        /// May be also used to log messages before returning them.
        abstract ParseError : exn -> string

        /// Returns the subscription provider implementation for this schema.
        abstract SubscriptionProvider : ISubscriptionProvider

        /// Returns the live query subscription provider implementation for this schema.
        abstract LiveFieldSubscriptionProvider : ILiveFieldSubscriptionProvider

    end

and ISchema<'Root> =
    interface
        inherit ISchema
        abstract Query : ObjectDef<'Root>
        abstract Mutation : ObjectDef<'Root> option
        abstract Subscription : SubscriptionObjectDef<'Root> option
    end

/// A type alias for a field execute compiler function.
and FieldExecuteCompiler = FieldDef -> ExecuteField

/// A field execute map object.
/// Field execute maps are mutable objects built to compile fields at runtime.
and FieldExecuteMap(compiler : FieldExecuteCompiler) =
    let map = new Dictionary<string * string, ExecuteField * InputFieldDef []>()

    let getKey typeName fieldName =
        if List.exists ((=) fieldName) ["__schema"; "__type"; "__typename" ]
            then "", fieldName
            else typeName, fieldName

    /// <summary>
    /// Sets an execute function for a field of a named type of the schema.
    /// </summary>
    /// <param name="typeName">The type name of the parent object that has the field that needs to be executed.</param>
    /// <param name="def">The FieldDef that will have its execute function configured into the FieldExecuteMap.</param>
    /// <param name="overwrite">
    /// If set to true, and an exists an entry with the <paramref name="typeName"/> and the name of the FieldDef,
    /// then it will be overwritten.
    /// </param>
    member __.SetExecute(typeName: string, def: FieldDef, ?overwrite : bool) =
        let overwrite = defaultArg overwrite false
        let key = typeName, def.Name
        let compiled = compiler def
        let args = def.Args
        match map.ContainsKey(key), overwrite with
        | true, true -> map.Remove(key) |> ignore; map.Add(key, (compiled, args))
        | false, _ -> map.Add(key, (compiled, args))
        | _ -> ()

    /// <summary>
    /// Sets an execute function for a field of an unamed type in the schema.
    /// </summary>
    /// <param name="def">The FieldDef that will have its execute function configured into the FieldExecuteMap.</param>
    /// <param name="overwrite">If set to true, and an exists an entry with the FieldDef name, then it will be overwritten.</param>
    member this.SetExecute(def : FieldDef, ?overwrite : bool) =
        let overwrite = defaultArg overwrite false
        this.SetExecute("", def, overwrite)

    /// <summary>
    /// Gets an ExecuteField based on the name of the type and the name of the field.
    /// </summary>
    /// <param name="typeName">The type name of the parent object that has the field that needs to be executed.</param>
    /// <param name="fieldName">The field name of the object that has the field that needs to be executed.</param>
    member __.GetExecute(typeName: string, fieldName: string) =
        let key = getKey typeName fieldName
        if map.ContainsKey(key) then fst map.[key] else Unchecked.defaultof<ExecuteField>

    /// <summary>
    /// Gets the field arguments based on the name of the type and the name of the field.
    /// </summary>
    /// <param name="typeName">The type name of the parent object that has the field that needs to be executed.</param>
    /// <param name="fieldName">The field name of the object that has the field that needs to be executed.</param>
    member __.GetArgs(typeName : string, fieldName : string) =
        let key = getKey typeName fieldName
        if map.ContainsKey(key) then snd map.[key] else Unchecked.defaultof<InputFieldDef []>

    interface IEnumerable<string * string * ExecuteField> with
        member __.GetEnumerator() =
            let seq = map |> Seq.map(fun kvp -> fst kvp.Key, snd kvp.Key, fst kvp.Value)
            seq.GetEnumerator()

    interface IEnumerable with
        member __.GetEnumerator() =
            let seq = map |> Seq.map(fun kvp -> fst kvp.Value)
            upcast seq.GetEnumerator()

/// Root of GraphQL type system. All type definitions use TypeDef as
/// a common root.
and TypeDef =
    interface

        /// Return .NET CLR type associated with current type definition.
        abstract Type : Type

        /// INTERNAL API: creates a List definition of a current type.
        abstract MakeList : unit -> ListOfDef

        /// INTERNAL API: creates a Nullable definition of a current type.
        abstract MakeNullable : unit -> NullableDef
    end

/// Root of GraphQL type system. Constrained to represent .NET type
/// provided as generic parameter.
and TypeDef<'Val> =
    interface
        inherit TypeDef
    end

/// Representation of all type defintions, that can be uses as inputs.
/// By default only scalars, enums, lists, nullables and input objects
/// are valid input types.
and InputDef =
    interface
        inherit TypeDef
    end

/// Representation of all type defintions, that can be uses as inputs.
/// By default only scalars, enums, lists, nullables and input objects
/// are valid input types. Constrained to represent .NET type provided
/// as generic parameter.
and InputDef<'Val> =
    interface
        inherit InputDef
        inherit TypeDef<'Val>
    end

/// Representation of all type defintions, that can be uses as outputs.
/// By default only scalars, enums, lists, nullables, unions, interfaces
/// and objects are valid output types.
and OutputDef =
    interface
        inherit TypeDef
    end

/// Representation of all type defintions, that can be uses as outputs.
/// By default only scalars, enums, lists, nullables, unions, interfaces
/// and objects are valid input types. Constrained to represent .NET type
/// provided as generic parameter.
and OutputDef<'Val> =
    interface
        inherit OutputDef
        inherit TypeDef<'Val>
    end

/// Representation of leaf type definitions. Leaf types represents leafs
/// of the GraphQL query tree. Each query path must end with a leaf.
/// By default only scalars and enums are valid leaf types.
and LeafDef =
    interface
        inherit TypeDef
    end

/// Representation of composite types. Composites are non-leaf nodes of
/// the GraphQL query tree. Query path cannot end with a composite.
/// Composite type defines list of fields, it consists of. By default
/// only interfaces, unions and objects are valid composite types.
and CompositeDef =
    interface
        inherit TypeDef
    end

/// Representation of abstract types: interfaces and unions. Each abstract
/// type contains a collection of possible object types, which can be resolved
/// from schema.
and AbstractDef =
    interface
        inherit TypeDef
        //NOTE: only abstract types are Interface and Union, which are both composite defs too
        inherit CompositeDef
    end

/// Representation of named types. All named types are registered in
/// a schema. By default only non-named types are nullables and lists.
and NamedDef =
    interface
        inherit TypeDef

        /// Returns a name of the current named type. It must be unique
        /// in scope of the defining schema.
        abstract Name : string
    end

/// A context holding all the information needed for planning an operation.
and PlanningContext =
    { Schema : ISchema
      RootDef : ObjectDef
      Document : Document
      Operation : OperationDefinition
      DocumentId : int
      Metadata : Metadata
      ValidationResult : ValidationResult<AstError> }

/// A function type, which upon execution returns true if related field should
/// be included in result set for the query.
and Includer = Map<string, obj> -> bool

/// A node representing part of the current GraphQL query execution plan.
/// It contains info about both document AST fragment of incoming query as well,
/// as field defintion and type info of related fields, defined in schema.
and ExecutionInfo =
    { /// Field identifier, which may be either field name or alias. For top level execution plan it will be None.
      Identifier : string
      /// Field definition of corresponding type found in current schema.
      Definition : FieldDef
      /// AST node of the parsed query document.
      Ast : Field
      /// A type of execution plan.
      Kind : ExecutionInfoKind
      /// Logic describing, if correlated field should be included in result set.
      Include : Includer
      /// Composite definition being the parent of the current field, execution plan refers to.
      ParentDef : OutputDef
      /// Type definition marking returned type.
      ReturnDef : OutputDef
      /// Flag determining if flag allows to have nullable output.
      IsNullable : bool }

    /// Get a nested info recognized by path provided as parameter. Path may consist of fields names or aliases.
    member this.GetPath (keys: string list) : ExecutionInfo option =
        let rec path info segments =
            match segments with
            | []        ->
                match info.Kind with
                | ResolveCollection inner -> Some inner
                | _ -> Some info
            | head :: tail ->
                match info.Kind with
                | ResolveDeferred inner -> path inner segments
                | ResolveLive inner -> path inner segments
                | ResolveStreamed (inner, _) -> path inner segments
                | ResolveValue -> None
                | ResolveCollection inner -> path inner segments
                | SelectFields fields ->
                    fields
                    |> List.tryFind (fun f -> f.Identifier = head)
                    |> Option.bind (fun f -> path f tail)
                | ResolveAbstraction typeMap ->
                    typeMap
                    |> Map.toSeq
                    |> Seq.map snd
                    |> Seq.collect id
                    |> Seq.tryFind (fun f -> f.Identifier = head)
                    |> Option.bind (fun f -> path f tail)
        path this keys
    override this.ToString () =
        let pad indent (sb: Text.StringBuilder) = for i = 0 to indent do sb.Append '\t' |> ignore
        let nameAs info =
            match info.Ast.Alias with
            | Some alias ->
                info.Ast.Name + " as " + alias + " of " + info.ReturnDef.ToString() + (if info.IsNullable then "" else "!")
            | None -> info.Ast.Name + " of " + info.ReturnDef.ToString() + (if info.IsNullable then "" else "!")
        let rec str indent sb info =
            match info.Kind with
            | ResolveValue ->
                pad indent sb
                sb.Append("ResolveValue: ").AppendLine(nameAs info) |> ignore
            | ResolveDeferred inner -> 
                pad indent sb
                sb.Append("ResolveDeferred: ").AppendLine(nameAs info) |> ignore
                str (indent+1) sb inner
            | ResolveLive inner ->
                pad indent sb
                sb.Append("ResolveLive: ").AppendLine(nameAs info) |> ignore
                str (indent+1) sb inner
            | ResolveStreamed (inner, mode) ->
                pad indent sb
                sb.Append("ResolveStreamed: ").AppendLine(nameAs info) |> ignore
                str (indent+1) sb inner
            | SelectFields fields ->
                pad indent sb
                sb.Append("SelectFields: ").AppendLine(nameAs info) |> ignore
                fields |> List.iter (str (indent+1) sb)
            | ResolveCollection inner ->
                pad indent sb
                sb.Append("ResolveCollection: ").AppendLine(nameAs info) |> ignore
                str (indent+1) sb inner
            | ResolveAbstraction types ->
                pad indent sb
                sb.Append("ResolveAbstraction: ").AppendLine(nameAs info) |> ignore
                types
                |> Map.iter (fun tname fields ->
                    pad (indent+1) sb
                    sb.Append("Case: ").AppendLine(tname) |> ignore
                    fields |> List.iter (str (indent+2) sb))

        let sb = Text.StringBuilder ()
        str 0 sb this
        sb.ToString()

/// Kind of ExecutionInfo, marking a reduction operations, that should be applied to it.
and ExecutionInfoKind =
    /// Reduce scalar or enum to a returned value.
    | ResolveValue
    /// Reduce result set by selecting provided set of fields,
    /// defined inside composite type, current execution info
    /// refers to.
    | SelectFields of fields : ExecutionInfo list
    /// Reduce current field as a collection, applying provided
    /// execution info on each of the collection's element.
    | ResolveCollection of elementPlan : ExecutionInfo
    /// Reduce union or interface types by applying provided set of
    /// field infos depending on what concrete object implementation
    /// will be found.
    | ResolveAbstraction of typeFields : Map<string, ExecutionInfo list>
    /// Reduce result set as a deferred result.
    | ResolveDeferred of ExecutionInfo
    /// Reduce the current field as a stream, applying
    /// the provided execution info on each of the
    /// collection's elements.
    | ResolveStreamed of ExecutionInfo * BufferedStreamOptions
    /// Reduce the current field as a live query.
    | ResolveLive of ExecutionInfo

/// Buffered stream options. Used to specify how the buffer will behavior in a stream.
and BufferedStreamOptions =
      /// The maximum time in milliseconds that the buffer will be filled before being sent to the subscriber.
    { Interval : int option
      /// The maximum number of items that will be buffered before being sent to the subscriber.
      PreferredBatchSize : int option }

/// Wrapper for a resolve method defined by the user or generated by a runtime.
and Resolve =
    /// Resolve function hasn't been defined. Valid only for interface fields.
    | Undefined

    /// Resolve field value in synchronous way.
    /// input defines .NET type of the provided object
    /// output defines .NET type of the returned value
    /// expr is untyped version of Expr<ResolveFieldContext->'Input->'Output>
    | Sync of input:Type * output:Type * expr:Expr

    /// Resolve field value as part of Async computation.
    /// input defines .NET type of the provided object
    /// output defines .NET type of the returned value
    /// expr is untyped version of Expr<ResolveFieldContext->'Input->Async<'Output>>
    | Async of input:Type * output:Type * expr:Expr

    /// Resolves the filter function of a subscription.
    /// root defines the .NET type of the root object
    /// input defines the .NET type of the value being subscribed to
    /// expr is the untyped version of Expr<ResolveFieldContext -> 'Root -> 'Input -> bool>
    | Filter of root: Type * input:Type * output:Type * expr:Expr

    /// Resolves the filter function of a subscription that has asyncronous fields.
    /// root defines the .NET type of the root object
    /// input defines the .NET type of the value being subscribed to
    /// expr is the untyped version of Expr<ResolveFieldContext -> 'Root -> 'Input -> bool>
    | AsyncFilter of root: Type * input:Type * output:Type * expr:Expr
    
    | ResolveExpr of expr:Expr 


    /// Returns an expression defining resolver function.
    member x.Expr =
        match x with
        | Sync(_,_,e) -> e
        | Async(_,_,e) -> e
        | ResolveExpr(e) -> e
        | Undefined -> failwith "Resolve function was not defined"
        | x -> failwith <| sprintf "Unexpected resolve function %A" x

/// Execution strategy for provided queries. Defines if object fields should
/// be resolved either sequentially one-by-one or in parallel.
and ExecutionStrategy =
    /// Object fields will be resolved one-by-one. This is default option
    /// for mutations.
    | Sequential
    /// Object fields will be reslved in parallel. It's only valid for
    /// read-only operations like queries. It's not valid for mutations.
    | Parallel

/// Type representing a variable definition inside GraphQL query.
and VarDef =
    { /// Variable name without prefixed '$'.
      Name: string
      /// Type definition in corresponding GraphQL schema.
      TypeDef: InputDef
      /// Optional default value.
      DefaultValue: Value option }


/// The context used to hold all the information for a schema compiling proccess.
and SchemaCompileContext =
    { Schema : ISchema
      TypeMap : TypeMap
      FieldExecuteMap : FieldExecuteMap }

/// A planning of an execution phase.
/// It is used by the execution process to execute an operation.
and ExecutionPlan =
    { /// Unique identifier of the current execution plan.
      DocumentId : int
      /// AST defintion of current operation.
      Operation : OperationDefinition
      /// Definition of the root type (either query or mutation) used by the
      /// current operation.
      RootDef : ObjectDef
      /// Execution strategy applied on the underlying object's fields.
      Strategy : ExecutionStrategy
      /// List of fields of top level query/mutation object to be resolved.
      Fields : ExecutionInfo list
      /// List of variables defined within executed query.
      Variables : VarDef list
      /// A dictionary of metadata associated with custom operations on the planning of this plan.
      Metadata : Metadata
      /// The validation result for the document being processed.
      ValidationResult : ValidationResult<AstError> }
    member x.Item with get(id) = x.Fields |> List.find (fun f -> f.Identifier = id)

/// Execution context of the current GraphQL operation. It contains a full
/// knowledge about which fields will be accessed, what types are associated
/// with them and what variable values have been set by incoming query.
and ExecutionContext =
    { /// GraphQL schema definition.
      Schema : ISchema
      /// Boxed value of the top level type, root query/mutation.
      RootValue : obj
      /// Execution plan describing, what fiedls are going to be resolved.
      ExecutionPlan : ExecutionPlan
      /// Collection of variables provided to execute current operation.
      Variables : Map<string, obj>
      /// Collection of errors that occurred while executing current operation.
      Errors : ConcurrentBag<exn>
      /// A map of all fields of the query and their respective execution operations.
      FieldExecuteMap : FieldExecuteMap
      /// A simple dictionary to hold metadata that can be used by execution customizations.
      Metadata : Metadata }

/// An execution context for the particular field, applied as the first
/// parameter for target resolve function.
and ResolveFieldContext =
    { /// Fragment of the overall execution plan related to current field.
      ExecutionInfo : ExecutionInfo
      /// Current operation execution context.
      Context : ExecutionContext
      /// GraphQL type definition for the returned value.
      ReturnType : TypeDef
      /// GraphQL type definition for a parent object, current value needs
      /// to be resolved from.
      ParentType : ObjectDef
      /// Current GraphQL schema.
      Schema : ISchema
      /// Untyped map of all argument values used for as current field's
      /// parametrized inputs.
      Args : Map<string, obj>
      /// Variables provided by the operation caller.
      Variables : Map<string, obj> }

    /// Remembers an exception, so it can be included in the final response.
    member x.AddError(error : exn) = x.Context.Errors.Add error

    /// Tries to find an argument by provided name.
    member x.TryArg(name : string) : 't option =
        match Map.tryFind name x.Args with
        | Some o -> Some(o :?> 't)
        | None -> None

    /// Returns an argument by provided name. If argument was not found
    /// and exception will be thrown.
    member x.Arg(name : string) : 't =
        match Map.tryFind name x.Args with
        | Some found -> downcast found
        | None -> raise (System.Collections.Generic.KeyNotFoundException(sprintf "Argument '%s' was not provided within context of a field '%s'. Check if it was supplied within GraphQL query." name x.ExecutionInfo.Identifier))

/// Function type for the compiled field executor.
and ExecuteField = ResolveFieldContext -> obj -> AsyncVal<obj>

/// Untyped representation of the GraphQL field defintion.
/// Can be used only withing object and interface definitions.
and FieldDef =
    interface
        /// Name of the field.
        abstract Name : string
        /// Optional field description.
        abstract Description : string option
        /// Optional field deprecation warning.
        abstract DeprecationReason : string option
        /// Field's GraphQL type definition.
        abstract TypeDef : OutputDef
        /// Field's arguments list.
        abstract Args : InputFieldDef []
        /// Field's metadata.
        abstract Metadata : Metadata
        /// Field resolution function.
        abstract Resolve : Resolve
        /// INTERNAL API: Compiled field executor. To be set only by the runtime.
        inherit IEquatable<FieldDef>
    end

/// A paritally typed representation of the GraphQL field defintion.
/// Contains type parameter describing .NET type used as it's container.
/// Can be used only withing object and interface definitions.
and FieldDef<'Val> =
    interface
        inherit FieldDef
    end

and [<CustomEquality; NoComparison>] internal FieldDefinition<'Val, 'Res> =
    { /// Name of the field.
      Name : string
      /// Optional field description.
      Description : string option
      /// Field's GraphQL type definition.
      TypeDef : OutputDef<'Res>
      /// Field resolution function.
      Resolve : Resolve
      /// Field's arguments list.
      Args : InputFieldDef []
      /// Optional field deprecation warning.
      DeprecationReason : string option
      /// Field metadata definition.
      Metadata : Metadata
      }

    interface FieldDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.DeprecationReason = x.DeprecationReason
        member x.TypeDef = x.TypeDef :> OutputDef
        member x.Args = x.Args
        member x.Resolve = x.Resolve
        member x.Metadata = x.Metadata

    interface FieldDef<'Val>

    interface IEquatable<FieldDef> with
        member x.Equals f = x.Name = f.Name && x.TypeDef :> OutputDef = f.TypeDef && x.Args = f.Args

    override x.Equals y =
        match y with
        | :? FieldDef as f -> (x :> IEquatable<FieldDef>).Equals(f)
        | _ -> false

    override x.GetHashCode() =
        let mutable hash = x.Name.GetHashCode()
        hash <- (hash * 397) ^^^ (x.TypeDef.GetHashCode())
        hash <- (hash * 397) ^^^ (x.Args.GetHashCode())
        hash

    override x.ToString() =
        if not (Array.isEmpty x.Args)
        then x.Name + "(" + String.Join(", ", x.Args) + "): " + x.TypeDef.ToString()
        else x.Name + ": " + x.TypeDef.ToString()

/// An untyped representation of GraphQL scalar type.
and ScalarDef =
    interface
        /// Name of the scalar type.
        abstract Name : string
        /// Optional scalar type description.
        abstract Description : string option
        /// A function used to retrieve a .NET object from provided GraphQL query.
        abstract CoerceInput : Value -> obj option
        /// A function used to set a surrogate representation to be
        /// returned as a query result.
        abstract CoerceValue : obj -> obj option
        inherit TypeDef
        inherit NamedDef
        inherit InputDef
        inherit OutputDef
        inherit LeafDef
    end

/// Concrete representation of the scalar types.
and [<CustomEquality; NoComparison>] ScalarDefinition<'Val> =
    { /// Name of the scalar type.
      Name : string
      /// Optional type description.
      Description : string option
      /// A function used to retrieve a .NET object from provided GraphQL query.
      CoerceInput : Value -> 'Val option
      /// A function used to set a surrogate representation to be
      /// returned as a query result.
      CoerceValue : obj -> 'Val option }

    interface TypeDef with
        member __.Type = typeof<'Val>

        member x.MakeNullable() =
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable

        member x.MakeList() =
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list

    interface InputDef
    interface OutputDef

    interface ScalarDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.CoerceInput input = x.CoerceInput input |> Option.map box
        member x.CoerceValue value = (x.CoerceValue value) |> Option.map box

    interface InputDef<'Val>
    interface OutputDef<'Val>
    interface LeafDef

    interface NamedDef with
        member x.Name = x.Name

    override x.Equals y =
        match y with
        | :? ScalarDefinition<'Val> as s -> x.Name = s.Name
        | _ -> false

    override x.GetHashCode() = x.Name.GetHashCode()
    override x.ToString() = x.Name + "!"

/// A GraphQL representation of single case of the enum type.
/// Enum value return value is always represented as string.
and EnumVal =
    interface
        /// Identifier of the enum value.
        abstract Name : string
        /// Optional enum value description.
        abstract Description : string option
        /// Value to be stringified as a result to the user.
        abstract Value : obj
        /// Optional description of the deprecation reason.
        abstract DeprecationReason : string option
    end

/// A GraphQL representation of single case of the enum type.
/// Enum value return value is always represented as string.
and EnumValue<'Val> =
    { /// Identifier of the enum value.
      Name : string
      /// Value to be stringified as a result to the user.
      Value : 'Val
      /// Optional enum value description.
      Description : string option
      /// Optional description of the deprecation reason.
      DeprecationReason : string option }

    interface EnumVal with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.DeprecationReason = x.DeprecationReason
        member x.Value = upcast x.Value

    override x.ToString() = x.Name

/// A GraphQL representation of the enum type. Enums are leaf types.
/// They have a well-defined set of all possible cases that
/// can be returned to caller.
and EnumDef =
    interface
        /// Enum type name.
        abstract Name : string
        /// Optional enum type description.
        abstract Description : string option
        /// List of available enum cases.
        abstract Options : EnumVal []
        inherit TypeDef
        inherit InputDef
        inherit OutputDef
        inherit LeafDef
        inherit NamedDef
    end

/// A GraphQL representation of the enum type. Enums are leaf types.
/// They have a well-defined set of all possible cases that
/// can be returned to caller.
and EnumDef<'Val> =
    interface
        /// List of available enum cases (typed).
        abstract Options : EnumValue<'Val> []
        inherit EnumDef
        inherit TypeDef<'Val>
        inherit InputDef<'Val>
        inherit OutputDef<'Val>
    end

and internal EnumDefinition<'Val> =
    { /// Enum type name.
      Name : string
      /// Optional enum type description.
      Description : string option
      /// List of available enum cases.
      Options : EnumValue<'Val> [] }
    interface InputDef
    interface OutputDef

    interface TypeDef with
        member __.Type = typeof<'Val>

        member x.MakeNullable() =
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable

        member x.MakeList() =
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list

    interface EnumDef<'Val> with
        member x.Options = x.Options

    interface EnumDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Options =
            x.Options
            |> Seq.ofArray
            |> Seq.cast<EnumVal>
            |> Seq.toArray

    interface NamedDef with
        member x.Name = x.Name

    override x.ToString() = x.Name + "!"

/// GraphQL type definition for objects. Objects are composite output
/// types with set of fields. They can implement GraphQL interfaces
/// and be cases of the GraphQL unions.
and ObjectDef =
    interface
        /// Name of the object type definition.
        abstract Name : string
        /// Optional object definition description.
        abstract Description : string option
        /// Collection of fields defined by the current object.
        abstract Fields : Map<string, FieldDef>
        /// Collection of interfaces implemented by the current object.
        abstract Implements : InterfaceDef []
        /// Optional function used to recognize of provided
        /// .NET object is valid for this GraphQL object definition.
        abstract IsTypeOf : (obj -> bool) option
        inherit TypeDef
        inherit NamedDef
        inherit OutputDef
        inherit CompositeDef
    end

/// GraphQL type definition for objects. Objects are composite output
/// types with set of fields. They can implement GraphQL interfaces
/// and be cases of the GraphQL unions.
and ObjectDef<'Val> =
    interface
        /// Collection of fields defined by the current object.
        abstract Fields : Map<string, FieldDef<'Val>>
        inherit ObjectDef
        inherit TypeDef<'Val>
        inherit OutputDef<'Val>
    end

and [<CustomEquality; NoComparison>] internal ObjectDefinition<'Val> =
    { /// Name of the object type definition.
      Name : string
      /// Optional object definition description.
      Description : string option
      /// Lazy resolver for the object fields. It must be lazy in
      /// order to allow self-recursive type references.
      FieldsFn : Lazy<Map<string, FieldDef<'Val>>>
      /// Collection of interfaces implemented by the current object.
      Implements : InterfaceDef []
      /// Optional function used to recognize of provided
      /// .NET object is valid for this GraphQL object definition.
      IsTypeOf : (obj -> bool) option }

    interface TypeDef with
        member __.Type = typeof<'Val>

        member x.MakeNullable() =
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable

        member x.MakeList() =
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list

    interface OutputDef

    interface ObjectDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Fields = x.FieldsFn.Force() |> Map.map (fun k v -> upcast v)
        member x.Implements = x.Implements
        member x.IsTypeOf = x.IsTypeOf

    interface ObjectDef<'Val> with
        member x.Fields = x.FieldsFn.Force()

    interface NamedDef with
        member x.Name = x.Name

    override x.Equals y =
        match y with
        | :? ObjectDefinition<'Val> as f -> f.Name = x.Name
        | _ -> false

    override x.GetHashCode() =
        let mutable hash = x.Name.GetHashCode()
        hash

    override x.ToString() = x.Name + "!"

and ObjectRef =
    interface
        /// Name of the object being referenced.
        abstract Name : string
        
        inherit TypeDef
        inherit NamedDef
        inherit OutputDef
        inherit CompositeDef
    end

and ObjectRef<'Val> =
    interface
        inherit ObjectRef 
        inherit TypeDef<'Val>
        inherit OutputDef<'Val>
    end
    
and internal ObjectReference<'Val> =
    { Name: string }
    
    interface TypeDef with
        member __.Type = typeof<'Val>

        member x.MakeNullable() =
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable

        member x.MakeList() =
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list
            
    interface ObjectRef<'Val> with
        member x.Name = x.Name
        
    interface NamedDef with
        member x.Name = x.Name

/// A GraphQL interface type defintion. Interfaces are composite
/// output types, that can be implemented by GraphQL objects.
and InterfaceDef =
    interface
        /// Name of the interface type definition.
        abstract Name : string
        /// Optional interface description.
        abstract Description : string option
        /// List of fields to be defined by implementing object
        /// definition in order to satisfy current interface.
        abstract Fields : FieldDef []
        /// Optional funciton used to determine, which object
        /// definition is a concrete implementation of the current
        /// interface for provided .NET object.
        abstract ResolveType : (obj -> ObjectDef) option
        inherit TypeDef
        inherit OutputDef
        inherit CompositeDef
        inherit AbstractDef
        inherit NamedDef
    end

/// A GraphQL interface type defintion. Interfaces are composite
/// output types, that can be implemented by GraphQL objects.
and InterfaceDef<'Val> =
    interface
        /// List of fields to be defined by implementing object
        /// definition in order to satisfy current interface.
        abstract Fields : FieldDef<'Val> []
        inherit TypeDef<'Val>
        inherit OutputDef<'Val>
        inherit InterfaceDef
    end

and [<CustomEquality; NoComparison>] internal InterfaceDefinition<'Val> =
    { /// Name of the interface type definition.
      Name : string
      /// Optional interface description.
      Description : string option
      /// Lazy defintion of fields to be defined by implementing
      /// object definition in order to satisfy current interface.
      /// Must be lazy in order to allow self-referencing types.
      FieldsFn : unit -> FieldDef<'Val> []
      /// Optional funciton used to determine, which object
      /// definition is a concrete implementation of the current
      /// interface for provided .NET object.
      ResolveType : (obj -> ObjectDef) option }

    interface TypeDef with
        member __.Type = typeof<'Val>

        member x.MakeNullable() =
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable

        member x.MakeList() =
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list

    interface OutputDef

    interface InterfaceDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Fields = x.FieldsFn() |> Array.map (fun fdef -> upcast fdef)
        member x.ResolveType = x.ResolveType

    interface InterfaceDef<'Val> with
        member x.Fields = x.FieldsFn()

    interface NamedDef with
        member x.Name = x.Name

    override x.Equals y =
        match y with
        | :? InterfaceDefinition<'Val> as f -> x.Name = f.Name
        | _ -> false

    override x.GetHashCode() =
        let mutable hash = x.Name.GetHashCode()
        hash

    override x.ToString() = x.Name + "!"

/// A GraphQL union definition. Unions are composite output types,
/// that can return one of the defined case objects as outputs.
and UnionDef =
    interface
        /// Name of the union type definition.
        abstract Name : string
        /// Optiona union type description.
        abstract Description : string option
        /// Collection of object cases represented by this union.
        abstract Options : ObjectDef []
        /// Optional funciton used to determine, which object
        /// definition is a concrete implementation of the current
        /// union for provided .NET object.
        abstract ResolveType : (obj -> ObjectDef) option
        /// Helper function which provides ability to retrieve
        /// specific values, that are wrapped in F# discriminated unions.
        abstract ResolveValue : obj -> obj
        inherit TypeDef
        inherit OutputDef
        inherit CompositeDef
        inherit AbstractDef
        inherit NamedDef
    end

/// A GraphQL union definition. Unions are composite output types,
/// that can return one of the defined case objects as outputs.
and UnionDef<'In> =
    interface
        /// Optional funciton used to determine, which object
        /// definition is a concrete implementation of the current
        /// union for provided .NET object.
        abstract ResolveType : ('In -> ObjectDef) option
        /// Helper function which provides ability to retrieve
        /// specific values, that are wrapped in F# discriminated unions.
        abstract ResolveValue : 'In -> obj
        inherit UnionDef
        inherit TypeDef<'In>
        inherit OutputDef<'In>
    end

/// 3.1.4 Unions
and [<CustomEquality; NoComparison>] internal UnionDefinition<'In, 'Out> =
    { /// Name of the union type definition.
      Name : string
      /// Optiona union type description.
      Description : string option
      /// Collection of object cases represented by this union.
      Options : ObjectDef []
      /// Optional funciton used to determine, which object
      /// definition is a concrete implementation of the current
      /// union for provided .NET object.
      ResolveType : ('In -> ObjectDef) option
      /// Helper function which provides ability to retrieve
      /// specific values, that are wrapped in F# discriminated unions.
      ResolveValue : 'In -> 'Out }

    interface TypeDef with
        member __.Type = typeof<'Out>

        member x.MakeNullable() =
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable

        member x.MakeList() =
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list

    interface OutputDef

    interface UnionDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Options = x.Options
        member x.ResolveType = x.ResolveType |> Option.map (fun fn -> (fun value -> fn (value :?> 'In)))
        member x.ResolveValue value = upcast x.ResolveValue(value :?> 'In)

    interface UnionDef<'In> with
        member x.ResolveType = x.ResolveType
        member x.ResolveValue value = upcast x.ResolveValue value

    interface NamedDef with
        member x.Name = x.Name

    override x.Equals y =
        match y with
        | :? UnionDefinition<'In, 'Out> as f -> x.Name = f.Name
        | _ -> false

    override x.GetHashCode() =
        let mutable hash = x.Name.GetHashCode()
        hash <- (hash * 397) ^^^ (x.Options.GetHashCode())
        hash

    override x.ToString() = x.Name + "!"

/// GraphQL type definition for collection types. Lists are both
/// valid input and output types.
and ListOfDef =
    interface
        /// GraphQL type definition of the container element type.
        abstract OfType : TypeDef
        inherit InputDef
        inherit OutputDef
    end

/// GraphQL type definition for collection types. Lists are both
/// valid input and output types.
and ListOfDef<'Val, 'Seq when 'Seq :> 'Val seq> =
    interface
        /// GraphQL type definition of the container element type.
        abstract OfType : TypeDef<'Val>
        inherit TypeDef<'Seq>
        inherit InputDef<'Seq>
        inherit OutputDef<'Seq>
        inherit ListOfDef
    end

and internal ListOfDefinition<'Val, 'Seq when 'Seq :> 'Val seq> =
    { OfType : TypeDef<'Val> }
    interface InputDef

    interface TypeDef with
        member __.Type = typeof<'Seq>
        member x.MakeNullable() =
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable

        member x.MakeList() =
            let list: ListOfDefinition<_, _> = { OfType = x }
            upcast list

    interface OutputDef

    interface ListOfDef with
        member x.OfType = upcast x.OfType

    interface ListOfDef<'Val, 'Seq> with
        member x.OfType = x.OfType

    override x.ToString() = "[" + x.OfType.ToString() + "]!"

/// GraphQL type definition for nullable/optional types.
/// By default all GraphQL types in this library are considered
/// to be NonNull. This definition applies reversed mechanics,
/// allowing them to take null as a valid value.
and NullableDef =
    interface
        /// GraphQL type definition of the nested type.
        abstract OfType : TypeDef
        inherit InputDef
        inherit OutputDef
    end

/// GraphQL type definition for nullable/optional types.
/// By default all GraphQL types in this library are considered
/// to be NonNull. This definition applies reversed mechanics,
/// allowing them to take null as a valid value.
and NullableDef<'Val> =
    interface
        /// GraphQL type definition of the nested type.
        abstract OfType : TypeDef<'Val>
        inherit InputDef<'Val option>
        inherit OutputDef<'Val option>
        inherit NullableDef
    end

and internal NullableDefinition<'Val> =
    { OfType : TypeDef<'Val> }
    interface InputDef

    interface TypeDef with
        member __.Type = typeof<'Val option>
        member x.MakeNullable() = upcast x
        member x.MakeList() =
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list

    interface OutputDef

    interface NullableDef with
        member x.OfType = upcast x.OfType

    interface NullableDef<'Val> with
        member x.OfType = x.OfType

    override x.ToString() =
        match x.OfType with
        | :? NamedDef as named -> named.Name
        | :? ListOfDef as list -> "[" + list.OfType.ToString() + "]"
        | other -> other.ToString()

/// GraphQL tye definition for input objects. They are different
/// from object types (which can be used only as outputs).
and InputObjectDef =
    interface
        /// Name of the input object.
        abstract Name : string
        /// Optional input object description.
        abstract Description : string option
        /// Collection of input object fields.
        abstract Fields : InputFieldDef []
        inherit NamedDef
        inherit InputDef
    end

/// GraphQL tye definition for input objects. They are different
/// from object types (which can be used only as outputs).
and InputObjectDefinition<'Val> =
    { /// Name of the input object.
      Name : string
      /// Optional input object description.
      Description : string option
      /// Function used to define field inputs. It must be lazy
      /// in order to support self-referencing types.
      FieldsFn : unit -> InputFieldDef [] }
    interface InputDef

    interface InputObjectDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Fields = x.FieldsFn()

    interface TypeDef<'Val>
    interface InputDef<'Val>

    interface NamedDef with
        member x.Name = x.Name

    interface TypeDef with
        member __.Type = typeof<'Val>

        member x.MakeNullable() =
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable

        member x.MakeList() =
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list

/// Function type used for resolving input object field values.
and ExecuteInput = Value -> Map<string, obj> -> obj

/// GraphQL field input definition. Can be used as fields for
/// input objects or as arguments for any ordinary field definition.
and InputFieldDef =
    interface
        /// Name of the input field / argument.
        abstract Name : string
        /// Optional input field / argument description.
        abstract Description : string option
        /// GraphQL type definition of the input type.
        abstract TypeDef : InputDef
        /// Optional default input value - used when no input was provided.
        abstract DefaultValue : obj option
        /// INTERNAL API: input execution function -
        /// compiled by the runtime.
        abstract ExecuteInput : ExecuteInput with get, set
        inherit IEquatable<InputFieldDef>
    end

/// INTERNAL API: 3.1.2.1 Object Field Arguments
and [<CustomEquality; NoComparison>] InputFieldDefinition<'In> =
    { /// Name of the input field / argument.
      Name : string
      /// Optional input field / argument description.
      Description : string option
      /// GraphQL type definition of the input type.
      TypeDef : InputDef<'In>
      /// Optional default input value - used when no input was provided.
      DefaultValue : 'In option
      /// INTERNAL API: input execution function -
      /// compiled by the runtime.
      mutable ExecuteInput : ExecuteInput }

    interface InputFieldDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.TypeDef = upcast x.TypeDef
        member x.DefaultValue = x.DefaultValue |> Option.map (fun x -> upcast x)

        member x.ExecuteInput
            with get () = x.ExecuteInput
            and set v = x.ExecuteInput <- v

    interface IEquatable<InputFieldDef> with
        member x.Equals f = x.Name = f.Name && x.TypeDef :> InputDef = f.TypeDef

    override x.Equals y =
        match y with
        | :? InputFieldDef as f -> (x :> IEquatable<InputFieldDef>).Equals(f)
        | _ -> false

    override x.GetHashCode() =
        let mutable hash = x.Name.GetHashCode()
        hash <- (hash * 397) ^^^ (x.TypeDef.GetHashCode())
        hash

    override x.ToString() = x.Name + ": " + x.TypeDef.ToString()

and Tag = System.IComparable

and TagsResolver = ResolveFieldContext -> Tag seq

and SubscriptionFieldDef =
    interface
        abstract OutputTypeDef : OutputDef
        abstract TagsResolver : TagsResolver
        inherit FieldDef
    end

and SubscriptionFieldDef<'Val> =
    interface
        inherit SubscriptionFieldDef
        inherit FieldDef<'Val>
    end

and SubscriptionFieldDef<'Root, 'Input, 'Output> =
    interface
        inherit SubscriptionFieldDef<'Root>
    end

and [<CustomEquality; NoComparison>] SubscriptionFieldDefinition<'Root, 'Input, 'Output> =
    {
        Name : string
        Description : string option
        DeprecationReason : string option
        // The type of the value that the subscription consumes, used to make sure that our filter function is properly typed
        OutputTypeDef : OutputDef<'Output>
        // The type of the root value, we need to thread this into our filter function
        RootTypeDef : OutputDef<'Root>
        Filter : Resolve
        Args : InputFieldDef []
        Metadata : Metadata
        TagsResolver : TagsResolver
    }
    interface FieldDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.DeprecationReason = x.DeprecationReason
        member x.TypeDef = x.RootTypeDef :> OutputDef
        member x.Resolve = x.Filter
        member x.Args = x.Args
        member x.Metadata = x.Metadata
    interface SubscriptionFieldDef with
        member x.OutputTypeDef = x.OutputTypeDef :> OutputDef
        member x.TagsResolver = x.TagsResolver
    interface FieldDef<'Root>
        member x.TypeDef = x.RootTypeDef
    interface SubscriptionFieldDef<'Root, 'Input, 'Output>
    interface IEquatable<FieldDef> with
        member x.Equals f =
            x.Name = f.Name &&
            x.TypeDef :> OutputDef = f.TypeDef &&
            x.Args = f.Args &&
            f :? SubscriptionFieldDef<'Root>
    override x.Equals y =
        match y with
        | :? SubscriptionFieldDef as f -> (x :> IEquatable<FieldDef>).Equals(f)
        | _ -> false

    override x.GetHashCode() =
        let mutable hash = x.Name.GetHashCode()
        hash <- (hash * 397) ^^^ (x.TypeDef.GetHashCode())
        hash <- (hash * 397) ^^^ (x.Args.GetHashCode())
        hash

    override x.ToString() =
        if not (Array.isEmpty x.Args)
        then x.Name + "(" + String.Join(", ", x.Args) + "): " + x.TypeDef.ToString()
        else x.Name + ": " + x.TypeDef.ToString()

and SubscriptionObjectDef =
    interface
        abstract Fields : Map<string, SubscriptionFieldDef>
        inherit ObjectDef
    end

and SubscriptionObjectDef<'Val> =
    interface
        inherit SubscriptionObjectDef
        abstract Fields : Map<string, SubscriptionFieldDef<'Val>>
        inherit ObjectDef<'Val>
    end

and [<CustomEquality; NoComparison>] SubscriptionObjectDefinition<'Val> =
    {
        Name : string
        Description : string option
        Fields : Map<string, SubscriptionFieldDef<'Val>>
    }

    interface TypeDef with
        member __.Type = typeof<'Val>

        member x.MakeNullable() =
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable

        member x.MakeList() =
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list
    interface ObjectDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Fields = x.Fields |> Map.map(fun _ f -> f :> FieldDef)
        member x.Implements = Array.empty : InterfaceDef []
        // TODO: Actually add istypeof
        member x.IsTypeOf = None
    interface ObjectDef<'Val> with
         member x.Fields = x.Fields |> Map.map(fun _ f -> f :> FieldDef<'Val>)

    interface NamedDef with
        member x.Name = x.Name
    interface SubscriptionObjectDef with
        member x.Fields = x.Fields |> Map.map (fun _ f -> upcast f)
    interface SubscriptionObjectDef<'Val> with
        member x.Fields = x.Fields
    override x.Equals y =
        match y with
        | :? SubscriptionObjectDefinition<'Val> as f -> f.Name = x.Name
        | _ -> false

    override x.GetHashCode() =
        let mutable hash = x.Name.GetHashCode()
        hash

    override x.ToString() = x.Name + "!"

/// GraphQL directive defintion.
and DirectiveDef =
    { /// Directive's name - it's NOT '@' prefixed.
      Name : string
      /// Optional directive description.
      Description : string option
      /// Directive location - describes, which part's of the query AST
      /// are valid places to include current directive to.
      Locations : DirectiveLocation
      /// Array of arguments defined within that directive.
      Args : InputFieldDef [] }

/// Metadata object.
/// Metadata objects are used to hold custom information inside fields and contexts
/// used by the GraphQL executor and ISchema.
and Metadata(data : Map<string, obj>) =
    new() = Metadata(Map.empty)

    /// <summary>
    /// Adds (or overwrites) an information to the metadata object, generating a new instance of it.
    /// </summary>
    /// <param name="key">The key to be used to search information for.</param>
    /// <param name="value">The value to be stored inside the metadata.</param>
    member __.Add(key : string, value : obj) = Metadata(data.Add (key, value))

    /// <summary>
    /// Generates a new Metadata instance, filled with items of a string * obj list.
    /// </summary>
    /// <param name="l">A list of string * obj tuples to be used to fill the Metadata object.</param>
    static member FromList(l : (string * obj) list) =
        let rec add (m : Metadata) (l : (string * obj) list) =
            match l with
            | [] -> m
            | (k, v) :: xs -> add (m.Add(k, v)) xs
        add (Metadata()) l

    /// Creates an empty Metadata object.
    static member Empty = Metadata.FromList [ ]

    /// <summary>
    /// Tries to find an value inside the metadata by it's key.
    /// </summary>
    /// <param name="key">The key to be used to search information for.</param>
    member __.TryFind<'Value>(key : string) =
        if data.ContainsKey key then data.Item key :?> 'Value |> Some else None

    override __.ToString() = sprintf "%A" data

/// Map of types of an ISchema.
/// The map of types is used to plan and execute queries.
and TypeMap() =
    let map = Dictionary<string, NamedDef>()
    let isDefaultType name =
        let defaultTypes =
            [ "__Schema"
              "__Directive"
              "__InputValue"
              "__Type"
              "__EnumValue"
              "__Field"
              "__TypeKind"
              "__DirectiveLocation" ]
        defaultTypes |> List.exists (fun x -> x = name)

    let rec named (tdef : TypeDef) =
        match tdef with
        | :? NamedDef as n -> Some n
        | :? NullableDef as n -> named n.OfType
        | :? ListOfDef as l -> named l.OfType
        | _ -> None

    /// <summary>
    /// Adds (or optionally overwrites) a type to the type map.
    /// </summary>
    /// <param name="def">The NamedDef to be added to the type map. It's name will be used as the key.</param>
    /// <param name="overwrite">If set to true, and another NamedDef exists with the same name, it will be overwritten.</param>
    member __.AddType(def : NamedDef, ?overwrite : bool) =
        let overwrite = defaultArg overwrite false
        let add name def overwrite =
            if not (map.ContainsKey(name))
            then map.Add(name, def)
            elif overwrite
            then map.[name] <- def
        let asNamed x = 
            match named x with 
            | Some n -> n
            | _ -> failwith "Expected a Named type!"
        let rec insert (def : NamedDef) =
            match def with
            | :? ScalarDef as sdef -> add sdef.Name def overwrite
            | :? EnumDef as edef -> add edef.Name def overwrite
            | :? SubscriptionObjectDef as sdef ->
                add sdef.Name def overwrite
                sdef.Fields
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.collect (fun x -> Array.append [| x.OutputTypeDef :> TypeDef |] (x.Args |> Array.map (fun a -> upcast a.TypeDef)))
                |> Seq.map asNamed
                |> Seq.filter (fun x -> not (map.ContainsKey(x.Name)))
                |> Seq.iter insert
            | :? ObjectDef as odef ->
                add odef.Name def overwrite
                odef.Fields
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.collect (fun x -> Seq.append (x.TypeDef :> TypeDef |> Seq.singleton) (x.Args |> Seq.map (fun a -> upcast a.TypeDef)))
                |> Seq.map asNamed
                |> Seq.filter (fun x -> not (map.ContainsKey(x.Name)))
                |> Seq.iter insert
                odef.Implements
                |> Seq.iter insert
            | :? ObjectRef ->
                ()
            | :? InterfaceDef as idef ->
                add idef.Name def overwrite
                idef.Fields
                |> Seq.map (fun x -> asNamed x.TypeDef)
                |> Seq.filter (fun x -> not (map.ContainsKey(x.Name)))
                |> Seq.iter insert
            | :? UnionDef as udef ->
                add udef.Name def overwrite
                udef.Options
                |> Seq.iter insert
            | :? ListOfDef as ldef ->
                match named ldef.OfType with
                | Some innerdef -> insert innerdef
                | None -> ()
            | :? NullableDef as ndef ->
                match named ndef.OfType with
                | Some innerdef -> insert innerdef
                | None -> ()
            | :? InputObjectDef as iodef ->
                add iodef.Name def overwrite
                iodef.Fields
                |> Seq.collect (fun x -> (x.TypeDef :> TypeDef) |> Seq.singleton)
                |> Seq.map (fun x -> match named x with Some n -> n | _ -> failwith "Expected a Named type!")
                |> Seq.filter (fun x -> not (map.ContainsKey(x.Name)))
                |> Seq.iter insert
            | _ -> failwith "Unexpected type!"
        insert def

    /// <summary>
    /// Adds (or optionally overwrites) types to the type map.
    /// </summary>
    /// <param name="defs">The NamedDef sequence to be added to the type map. Their names will be used as keys.</param>
    /// <param name="overwrite">If set to true, and another NamedDef exists with the same name on the sequence, it will be overwritten.</param>
    member this.AddTypes(defs : NamedDef seq, ?overwrite : bool) =
        let overwrite = defaultArg overwrite false
        defs |> Seq.iter (fun def -> this.AddType(def, overwrite))

    /// Converts this type map to a sequence of string * NamedDef values, with the first item being the key.
    member __.ToSeq(?includeDefaultTypes : bool) =
        let includeDefaultTypes = defaultArg includeDefaultTypes true
        let result = map |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
        if not includeDefaultTypes
        then result |> Seq.filter (fun (k, _) -> not (isDefaultType k))
        else result

    /// Converts this type map to a list of string * NamedDef values, with the first item being the key.
    member this.ToList(?includeDefaultTypes : bool) =
        let includeDefaultTypes = defaultArg includeDefaultTypes true
        this.ToSeq(includeDefaultTypes) |> List.ofSeq

    /// <summary>
    /// Tries to find a NamedDef in the map by it's key (the name).
    /// </summary>
    /// <param name="name">The name of the NamedDef to be searched for.</param>
    /// <param name="includeDefaultTypes">If set to true, it will search for the NamedDef among the default types.</param>
    member __.TryFind(name : string, ?includeDefaultTypes : bool) =
        let includeDefaultTypes = defaultArg includeDefaultTypes false
        if not includeDefaultTypes && isDefaultType name
        then
            None
        else
            match map.TryGetValue(name) with
            | (true, item) -> Some item
            | _ -> None

    /// <summary>
    /// Tries to find a NamedDef of a specific type in the map by it's key (the name).
    /// </summary>
    /// <param name="name">The name of the NamedDef to be searched for.</param>
    /// <param name="includeDefaultTypes">If set to true, it will search for the NamedDef among the default types.</param>
    member this.TryFind<'Type when 'Type :> NamedDef>(name : string, ?includeDefaultTypes : bool) =
        let includeDefaultTypes = defaultArg includeDefaultTypes false
        match this.TryFind(name, includeDefaultTypes) with
        | Some item ->
            match item with
            | :? 'Type as item -> Some item
            | _ -> None
        | _ -> None

    /// <summary>
    /// Gets all NamedDef's inside the map that are, or implements the specified type.
    /// </summary>
    /// <param name="includeDefaultTypes">If set to true, it will search for the NamedDef among the default types.</param>
    member this.OfType<'Type when 'Type :> NamedDef>(?includeDefaultTypes : bool) =
        let includeDefaultTypes = defaultArg includeDefaultTypes false
        this.ToSeq()
        |> Seq.filter (fun (name, _) -> not includeDefaultTypes && not (isDefaultType name))
        |> Seq.map (snd >> (fun x -> match x with :? 'Type as x -> Some x | _ -> None))
        |> Seq.choose id
        |> List.ofSeq

    /// <summary>
    /// Tries to find a FieldDef inside an ObjectDef by the object name and the field name.
    /// If the map has no ObjectDef types, it won't find anything.
    /// </summary>
    /// <param name="objname">The name of the ObjectDef that has the field that are being searched.</param>
    /// <param name="fname">The name of the FieldDef to be searched for.</param>
    member this.TryFindField(objname : string, fname : string) =
        match this.TryFind<ObjectDef>(objname) with
        | Some odef -> odef.Fields |> Map.tryFind fname
        | None -> None

    /// <summary>
    /// Tries to find a FieldDef inside an ObjectDef by its type, the object name and the field name.
    /// If the map has no ObjectDef types, it won't find anything.
    /// </summary>
    /// <param name="objname">The name of the ObjectDef that has the field that are being searched.</param>
    /// <param name="fname">The name of the FieldDef to be searched for.</param>
    member this.TryFindField<'Type when 'Type :> OutputDef>(objname : string, fname : string) =
        match this.TryFindField(objname, fname) with
        | Some fdef ->
            match fdef.TypeDef with
            | :? 'Type -> Some fdef
            | _ -> None
        | _ -> None

    /// <summary>
    /// Tries to find ObjectDef<'Val> types inside the map, that have fields that are lists of 'Res type.
    /// </summary>
    /// <param name="includeDefaultTypes">If set to true, it will search for the NamedDef among the default types.</param>
    member this.GetTypesWithListFields<'Val, 'Res>(?includeDefaultTypes : bool) =
        let includeDefaultTypes = defaultArg includeDefaultTypes false
        let toSeq map = map |> Map.toSeq |> Seq.map snd
        let map (f : FieldDef<'Val>) =
            let rec isList (fieldTypeDef : TypeDef) =
                match fieldTypeDef with
                | :? NullableDef as x -> isList x.OfType
                | :? ListOfDef<'Res, 'Res seq> -> true
                | _ -> false
            if isList f.TypeDef then Some f else None
        this.OfType<ObjectDef<'Val>>(includeDefaultTypes)
        |> Seq.map (fun x -> x, (x.Fields |> toSeq |> Seq.map map |> Seq.choose id |> List.ofSeq))
        |> List.ofSeq

    /// <summary>
    /// Creates a new TypeMap instance, using a sequence of NamedDef's to fill it.
    /// </summary>
    /// <param name="defs">The NamedDef sequence that has the NamedDef's that will be filled into the TypeMap.</param>
    static member FromSeq(defs : NamedDef seq) =
        let map = TypeMap()
        defs |> Seq.iter (fun def -> map.AddType(def))
        map

module Tags =
    let from (x : #Tag) : Tag seq = Seq.singleton (upcast x)
    let fromSeq x : Tag seq = x |> Seq.map (fun t -> upcast t)

[<AutoOpen>]
module SubscriptionExtensions =
    type ISubscriptionProvider with
        member this.Register subscription =
            this.AsyncRegister subscription |> Async.RunSynchronously

        member this.Publish<'T> name subType =
            this.AsyncPublish name subType |> Async.RunSynchronously

        member this.PublishTag<'T> name index subType =
            this.AsyncPublishTag name index subType |> Async.RunSynchronously

    type ILiveFieldSubscriptionProvider with
        member this.Register subscription =
            this.AsyncRegister subscription |> Async.RunSynchronously

        member this.Publish<'T> typeName fieldName subType =
            this.AsyncPublish typeName fieldName subType |> Async.RunSynchronously

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Resolve =
    type private Marker = class end

    let private (|FSharpFunc|_|) (typ : Type) =
        if FSharpType.IsFunction typ then
            let d,c = FSharpType.GetFunctionElements typ
            Some(d,c)
        else None

    let private (|FSharpOption|_|) (typ : Type) =
        if typ.GetTypeInfo().IsGenericType && typ.GetGenericTypeDefinition() = typedefof<option<_>> then
            Some(typ.GenericTypeArguments |> Array.head)
        else None
    
    let private (|FSharpAsync|_|) (typ: Type) =
        if typ.GetTypeInfo().IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Async<_>> then
            Some(typ.GenericTypeArguments |> Array.head)
        else None
        
    let private boxify<'T,'U>(f:ResolveFieldContext -> 'T -> 'U) : ResolveFieldContext -> obj -> obj =
        <@@ fun ctx (x:obj) -> f ctx (x :?> 'T)  |> box  @@>
        |> LeafExpressionConverter.EvaluateQuotation
        |> unbox

    let private boxifyAsync<'T, 'U>(f:ResolveFieldContext -> 'T -> Async<'U>): ResolveFieldContext -> obj -> Async<obj> =
        <@@ fun ctx (x:obj) -> async.Bind(f ctx (x :?> 'T), async.Return << box)  @@>
        |> LeafExpressionConverter.EvaluateQuotation
        |> unbox

    let private boxifyFilter<'Root, 'Input, 'Output>(f:ResolveFieldContext -> 'Root -> 'Input -> 'Output option): ResolveFieldContext -> obj -> obj -> obj option =
        <@@ fun ctx (r:obj) (i:obj) -> f ctx (r :?> 'Root) (i :?> 'Input) |> Option.map(box)@@>
        |> LeafExpressionConverter.EvaluateQuotation
        |> unbox 

    let private boxifyAsyncFilter<'Root, 'Input, 'Output>(f:ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>): ResolveFieldContext -> obj -> obj -> Async<obj option> =
        <@@ fun ctx (r:obj) (i:obj) -> async.Bind(f ctx (r :?> 'Root) (i :?> 'Input), async.Return << Option.map(box))@@>
        |> LeafExpressionConverter.EvaluateQuotation
        |> unbox

    let private getRuntimeMethod name =
        let methods = typeof<Marker>.DeclaringType.GetRuntimeMethods() 
        methods |> Seq.find (fun m -> m.Name.Equals name)
        
    let private runtimeBoxify = getRuntimeMethod "boxify"
    
    let private runtimeBoxifyAsync = getRuntimeMethod "boxifyAsync"

    let private runtimeBoxifyFilter = getRuntimeMethod "boxifyFilter"

    let private runtimeBoxifyAsyncFilter = getRuntimeMethod "boxifyAsyncFilter"

    let private unwrapExpr = function
        | WithValue(resolver, _, _) -> (resolver, resolver.GetType())
        | expr -> failwithf "Could not extract resolver from Expr: '%A'" expr 
        
    let inline private resolveUntyped f d c (methodInfo:MethodInfo) = 
        let result = methodInfo.GetGenericMethodDefinition().MakeGenericMethod(d,c).Invoke(null, [|f|])
        result |> unbox

    let inline private resolveUntypedFilter f r i o (methodInfo:MethodInfo) =
        let result = methodInfo.GetGenericMethodDefinition().MakeGenericMethod(r, i, o).Invoke(null, [|f|])
        result |> unbox

    let private boxifyExpr expr : ResolveFieldContext -> obj -> obj =
        match unwrapExpr expr with
        | resolver, FSharpFunc(_,FSharpFunc(d,c)) -> 
            resolveUntyped resolver d c runtimeBoxify
        | resolver, _ -> failwithf "Unsupported signature for Resolve %A" (resolver.GetType())
    
    let private boxifyExprAsync expr : ResolveFieldContext -> obj -> Async<obj> =
        match unwrapExpr expr with
        | resolver, FSharpFunc(_,FSharpFunc(d,FSharpAsync(c))) -> 
            resolveUntyped resolver d c runtimeBoxifyAsync
        | resolver, _ -> failwithf "Unsupported signature for Async Resolve %A" (resolver.GetType())

    let private boxifyFilterExpr expr: ResolveFieldContext -> obj -> obj -> obj option =
        match unwrapExpr expr with
        | resolver, FSharpFunc(_,FSharpFunc(r,FSharpFunc(i,FSharpOption(o)))) ->
            resolveUntypedFilter resolver r i o runtimeBoxifyFilter
        | resolver, _ -> failwithf "Unsupported signature for Subscription Filter Resolve %A" (resolver.GetType()) 
        
    let private boxifyAsyncFilterExpr expr: ResolveFieldContext -> obj -> obj -> Async<obj option> =
        match unwrapExpr expr with
        | resolver, FSharpFunc(_,FSharpFunc(r,FSharpFunc(i,FSharpAsync(FSharpOption(o))))) ->
            resolveUntypedFilter resolver r i o runtimeBoxifyAsyncFilter
        | resolver, _ -> failwithf "Unsupported signature for Async Subscription Filter Resolve %A" (resolver.GetType()) 

    let (|BoxedSync|_|) = function 
        | Sync(d,c,expr) -> Some(d,c,boxifyExpr expr)
        | _ -> None
       
    let (|BoxedAsync|_|) = function
        | Async(d,c,expr) -> Some(d,c,boxifyExprAsync expr)
        | _ -> None 

    let (|BoxedExpr|_|) = function
        | ResolveExpr(e) -> Some(boxifyExpr e)
        | _ -> None 

    let (|BoxedFilterExpr|_|) = function
        | Filter(r,i,o,expr) -> Some(r,i,o,boxifyFilterExpr expr)
        | _ -> None

    let (|BoxedAsyncFilterExpr|_|) = function
        | AsyncFilter(r,i,o,expr) -> Some(r,i,o,boxifyAsyncFilterExpr expr)
        | _ -> None

    let private genMethodResolve<'Val, 'Res> (typeInfo: TypeInfo) (methodInfo: MethodInfo) = 
        let argInfo = typeof<ResolveFieldContext>.GetTypeInfo().GetDeclaredMethod("Arg")
        let valueVar = Var("value", typeof<'Val>)
        let ctxVar = Var("ctx", typeof<ResolveFieldContext>)
        let argExpr (arg : ParameterInfo) = 
            Expr.Call(Expr.Var(ctxVar), argInfo.MakeGenericMethod(arg.ParameterType), [ Expr.Value(arg.Name) ])        
        let args = 
            methodInfo.GetParameters()
            |> Array.map argExpr
            |> Array.toList        
        let expr = 
            Expr.Lambda
                (ctxVar, Expr<'Val -> 'Res>.Lambda(valueVar, Expr.Call(Expr.Var(valueVar), methodInfo, args)))
        let compiled = expr |> LeafExpressionConverter.EvaluateQuotation 
        let exprWithVal = Expr.WithValue(compiled, typeof<ResolveFieldContext -> 'Val -> 'Res>, expr)
        Sync(typeof<'Val>, typeof<'Res>, exprWithVal)
        
    let private genPropertyResolve<'Val, 'Res> (typeInfo: TypeInfo) property = 
        let valueVar = Var("value", typeof<'Val>)
        let ctxVar = Var("ctx", typeof<ResolveFieldContext>)
        let expr = 
            Expr.Lambda
                (ctxVar, 
                 Expr<'Val -> 'Res>.Lambda(valueVar, Expr.PropertyGet(Expr.Var(valueVar), property)))
        let compiled = expr |> LeafExpressionConverter.EvaluateQuotation 
        let exprWithVal = Expr.WithValue(compiled, typeof<ResolveFieldContext -> 'Val -> 'Res>, expr)
        Sync(typeof<'Val>, typeof<'Res>, exprWithVal)
            
    let internal defaultResolve<'Val, 'Res> (fieldName : string) : Resolve = 
        let typeInfo = typeof<'Val>.GetTypeInfo()
        let property = typeInfo.GetDeclaredProperty(fieldName, ignoreCase = true)
        match property with
        | null -> 
            let methodInfo = typeInfo.GetDeclaredMethod(fieldName, ignoreCase = true)
            genMethodResolve<'Val, 'Res> typeInfo methodInfo      
        | p -> genPropertyResolve<'Val, 'Res> typeInfo p

module Patterns =

    /// Active pattern to match GraphQL type defintion with Scalar.
    let (|Scalar|_|) (tdef : TypeDef) =
        match tdef with
        | :? ScalarDef as x -> Some x
        | _ -> None

    /// Active pattern to match GraphQL type defintion with Object.
    let (|Object|_|) (tdef : TypeDef) =
        match tdef with
        | :? ObjectDef as x -> Some x
        | _ -> None

    /// Active pattern to match GraphQL type defintion with Interface.
    let (|Interface|_|) (tdef : TypeDef) =
        match tdef with
        | :? InterfaceDef as x -> Some x
        | _ -> None

    /// Active pattern to match GraphQL type defintion with Union.
    let (|Union|_|) (tdef : TypeDef) =
        match tdef with
        | :? UnionDef as x -> Some x
        | _ -> None

    /// Active pattern to match GraphQL type defintion with Enum.
    let (|Enum|_|) (tdef : TypeDef) =
        match tdef with
        | :? EnumDef as x -> Some x
        | _ -> None

    /// Active pattern to match GraphQL type defintion with input object.
    let (|InputObject|_|) (tdef : TypeDef) =
        match tdef with
        | :? InputObjectDef as x -> Some x
        | _ -> None

    /// Active patter to match GraphQL subscription object definitions

    let (|SubscriptionObject|_|) (tdef : TypeDef) =
        match tdef with
        | :? SubscriptionObjectDef as x -> Some x
        | _ -> None

    /// Active pattern to match GraphQL type defintion with List.
    let (|List|_|) (tdef : TypeDef) =
        match tdef with
        | :? ListOfDef as x -> Some x.OfType
        | _ -> None

    /// Active pattern to match GraphQL type defintion with nullable / optional types.
    let (|Nullable|_|) (tdef : TypeDef) =
        match tdef with
        | :? NullableDef as x -> Some x.OfType
        | _ -> None

    /// Active pattern to match GraphQL type defintion with non-null types.
    let (|NonNull|_|) (tdef : TypeDef) =
        match tdef with
        | :? NullableDef -> None
        | other -> Some other

    /// Active pattern to match GraphQL type defintion with valid input types.
    let (|Input|_|) (tdef : TypeDef) =
        match tdef with
        | :? InputDef as i -> Some i
        | _ -> None

    /// Active pattern to match GraphQL type defintion with valid output types.
    let (|Output|_|) (tdef : TypeDef) =
        match tdef with
        | :? OutputDef as o -> Some o
        | _ -> None

    /// Active pattern to match GraphQL type defintion with valid leaf types.
    let (|Leaf|_|) (tdef : TypeDef) =
        match tdef with
        | :? LeafDef as ldef -> Some ldef
        | _ -> None

    /// Active pattern to match GraphQL type defintion with valid composite types.
    let (|Composite|_|) (tdef : TypeDef) =
        match tdef with
        | :? ObjectDef | :? InterfaceDef | :? UnionDef -> Some tdef
        | _ -> None

    /// Active pattern to match GraphQL type defintion with valid abstract types.
    let (|Abstract|_|) (tdef : TypeDef) =
        match tdef with
        | :? InterfaceDef | :? UnionDef -> Some(tdef :?> AbstractDef)
        | _ -> None

    let rec private named (tdef : TypeDef) =
        match tdef with
        | :? NamedDef as n -> Some n
        | Nullable inner -> named inner
        | List inner -> named inner
        | _ -> None

    /// Active pattern to match GraphQL type defintion with named types.
    let rec (|Named|_|) (tdef : TypeDef) = named tdef

[<AutoOpen>]
module SchemaDefinitions =
    open System.Globalization
    open System.Reflection

    /// Tries to convert any value to int.
    let private coerceIntValue (x : obj) : int option =
        match x with
        | null -> None
        | :? int as i -> Some i
        | :? int64 as l -> Some(int l)
        | :? double as d -> Some(int d)
        | :? string as s ->
            match Int32.TryParse(s) with
            | true, i -> Some i
            | false, _ -> None
        | :? bool as b ->
            Some(if b then 1
                 else 0)
        | other ->
            try
                Some(System.Convert.ToInt32 other)
            with _ -> None

    /// Tries to convert any value to int64.
    let private coerceLongValue (x : obj) : int64 option =
        match x with
        | null -> None
        | :? int as i -> Some (int64 i)
        | :? int64 as l -> Some(l)
        | :? double as d -> Some(int64 d)
        | :? string as s ->
            match Int64.TryParse(s) with
            | true, i -> Some i
            | false, _ -> None
        | :? bool as b ->
            Some(if b then 1L else 0L)
        | other ->
            try
                Some(System.Convert.ToInt64 other)
            with _ -> None

    /// Tries to convert any value to double.
    let private coerceFloatValue (x : obj) : double option =
        match x with
        | null -> None
        | :? int as i -> Some(double i)
        | :? int64 as l -> Some(double l)
        | :? double as d -> Some d
        | :? string as s ->
            match Double.TryParse(s) with
            | true, i -> Some i
            | false, _ -> None
        | :? bool as b ->
            Some(if b then 1.
                 else 0.)
        | other ->
            try
                Some(System.Convert.ToDouble other)
            with _ -> None

    /// Tries to convert any value to bool.
    let private coerceBoolValue (x : obj) : bool option =
        match x with
        | null -> None
        | :? int as i -> Some(i <> 0)
        | :? int64 as l -> Some(l <> 0L)
        | :? double as d -> Some(d <> 0.)
        | :? string as s ->
            match Boolean.TryParse(s) with
            | true, i -> Some i
            | false, _ -> None
        | :? bool as b -> Some b
        | other ->
            try
                Some(System.Convert.ToBoolean other)
            with _ -> None

    /// Tries to convert any value to URI.
    let private coerceUriValue (x : obj) : Uri option =
        match x with
        | null -> None
        | :? Uri as u -> Some u
        | :? string as s ->
            match Uri.TryCreate(s, UriKind.RelativeOrAbsolute) with
            | true, uri -> Some uri
            | false, _ -> None
        | other -> None

    /// Tries to convert any value to DateTime.
    let private coerceDateValue (x : obj) : DateTime option =
        match x with
        | null -> None
        | :? DateTime as d -> Some d
        | :? string as s ->
            match DateTime.TryParse(s) with
            | true, date -> Some date
            | false, _ -> None
        | other -> None

    /// Tries to convert any value to Guid.
    let private coerceGuidValue (x : obj) : Guid option =
        match x with
        | null -> None
        | :? Guid as g -> Some g
        | :? string as s ->
            match Guid.TryParse(s) with
            | true, guid -> Some guid
            | false, _ -> None
        | other -> None

    /// Check if provided obj value is an Option and extract its wrapped value as object if possible
    let private (|Option|_|) (x : obj) =
        if x = null then None
        else
            let t = x.GetType().GetTypeInfo()
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
                t.GetDeclaredProperty("Value").GetValue(x) |> Some
            else None

    /// Tries to convert any value to string.
    let coerceStringValue (x : obj) : string option =
        match x with
        | null -> None
        | :? string as s -> Some s
        | :? bool as b ->
            Some(if b then "true"
                 else "false")
        | Option o -> Some(o.ToString())
        | _ -> Some(x.ToString())

    /// Tries to convert any value to generic type parameter.
    let private coerceIDValue (x : obj) : 't option =
        match x with
        | null -> None
        | :? string as s -> Some (downcast Convert.ChangeType(s, typeof<'t>))
        | Option o -> Some(downcast Convert.ChangeType(o, typeof<'t>))
        | _ -> Some(downcast Convert.ChangeType(x, typeof<'t>))

    /// Tries to resolve AST query input to int.
    let private coerceIntInput =
        function
        | IntValue i -> Some (int i)
        | FloatValue f -> Some(int f)
        | StringValue s ->
            match Int32.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, i -> Some i
            | false, _ -> None
        | BooleanValue b ->
            Some(if b then 1
                 else 0)
        | _ -> None

    /// Tries to resolve AST query input to int64.
    let private coerceLongInput =
        function
        | IntValue i -> Some (int64 i)
        | FloatValue f -> Some(int64 f)
        | StringValue s ->
            match Int64.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, i -> Some i
            | false, _ -> None
        | BooleanValue b ->
            Some(if b then 1L else 0L)
        | _ -> None

    /// Tries to resolve AST query input to double.
    let private coerceFloatInput =
        function
        | IntValue i -> Some(double i)
        | FloatValue f -> Some f
        | StringValue s ->
            match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, i -> Some i
            | false, _ -> None
        | BooleanValue b ->
            Some(if b then 1.
                 else 0.)
        | _ -> None

    /// Tries to resolve AST query input to string.
    let coerceStringInput =
        function
        | IntValue i -> Some(i.ToString(CultureInfo.InvariantCulture))
        | FloatValue f -> Some(f.ToString(CultureInfo.InvariantCulture))
        | StringValue s -> Some s
        | BooleanValue b ->
            Some(if b then "true"
                 else "false")
        | _ -> None

    let coerceEnumInput =
        function
        | EnumValue e -> Some e
        | _ -> None

    /// Tries to resolve AST query input to bool.
    let coerceBoolInput =
        function
        | IntValue i ->
            Some(if i = 0L then false
                 else true)
        | FloatValue f ->
            Some(if f = 0. then false
                 else true)
        | StringValue s ->
            match Boolean.TryParse(s) with
            | true, i -> Some i
            | false, _ -> None
        | BooleanValue b -> Some b
        | _ -> None

    /// Tries to resolve AST query input to provided generic type.
    let private coerceIdInput input : 't option=
        match input with
        | IntValue i -> Some(downcast Convert.ChangeType(i, typeof<'t>))
        | StringValue s -> Some(downcast Convert.ChangeType(s, typeof<'t>))
        | _ -> None

    /// Tries to resolve AST query input to URI.
    let private coerceUriInput =
        function
        | StringValue s ->
            match Uri.TryCreate(s, UriKind.RelativeOrAbsolute) with
            | true, uri -> Some uri
            | false, _ -> None
        | _ -> None

    /// Tries to resolve AST query input to DateTime.
    let private coerceDateInput =
        function
        | StringValue s ->
            match DateTime.TryParse(s) with
            | true, date -> Some date
            | false, _ -> None
        | _ -> None

    /// Tries to resolve AST query input to Guid.
    let private coerceGuidInput =
        function
        | StringValue s ->
            match Guid.TryParse(s) with
            | true, guid -> Some guid
            | false, _ -> None
        | _ -> None

    /// Wraps a GraphQL type definition, allowing defining field/argument
    /// to take option of provided value.
    let Nullable(innerDef : #TypeDef<'Val>) : NullableDef<'Val> = upcast { NullableDefinition.OfType = innerDef }

    /// Wraps a GraphQL type definition, allowing defining field/argument
    /// to take collection of provided value.
    let ListOf(innerDef : #TypeDef<'Val>) : ListOfDef<'Val, 'Seq> = upcast { ListOfDefinition.OfType = innerDef }

    let private ignoreInputResolve (_ : unit) (input : 'T) = ()

    let variableOrElse other value variables =
        match value with
        | Variable variableName -> Map.tryFind variableName variables |> Option.toObj
        | v -> other v

    /// GraphQL type of int
    let Int : ScalarDefinition<int> =
        { Name = "Int"
          Description =
              Some
                  "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1."
          CoerceInput = coerceIntInput
          CoerceValue = coerceIntValue }

    /// GraphQL type of long
    let Long : ScalarDefinition<int64> =
        { Name = "Long"
          Description =
              Some
                  "The `Long` scalar type represents non-fractional signed whole numeric values. Long can represent values between -(2^63) and 2^63 - 1."
          CoerceInput = coerceLongInput
          CoerceValue = coerceLongValue }

    /// GraphQL type of boolean
    let Boolean : ScalarDefinition<bool> =
        { Name = "Boolean"
          Description = Some "The `Boolean` scalar type represents `true` or `false`."
          CoerceInput = coerceBoolInput
          CoerceValue = coerceBoolValue }

    /// GraphQL type of float
    let Float : ScalarDefinition<double> =
        { Name = "Float"
          Description =
              Some
                  "The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
          CoerceInput = coerceFloatInput
          CoerceValue = coerceFloatValue }

    /// GraphQL type of string
    let String : ScalarDefinition<string> =
        { Name = "String"
          Description =
              Some
                  "The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text."
          CoerceInput = coerceStringInput
          CoerceValue = coerceStringValue }

    /// GraphQL type for custom identifier
    let ID<'Val> : ScalarDefinition<'Val> =
        { Name = "ID"
          Description =
              Some
                  "The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID."
          CoerceInput = coerceIdInput
          CoerceValue = coerceIDValue }

    let Obj : ScalarDefinition<obj> = {
            Name = "Object"
            Description =
               Some
                  "The `Object` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text."
            CoerceInput = (fun (o: Value) -> Some (o:>obj))
            CoerceValue = (fun (o: obj) -> Some (o))
        }

    /// GraphQL type for System.Uri
    let Uri : ScalarDefinition<Uri> =
        { Name = "URI"
          Description =
              Some
                  "The `URI` scalar type represents a string resource identifier compatible with URI standard. The URI type appears in a JSON response as a String."
          CoerceInput = coerceUriInput
          CoerceValue = coerceUriValue }

    /// GraphQL type for System.DateTime
    let Date : ScalarDefinition<DateTime> =
        { Name = "Date"
          Description =
              Some
                  "The `Date` scalar type represents a Date value with Time component. The Date type appears in a JSON response as a String representation compatible with ISO-8601 format."
          CoerceInput = coerceDateInput
          CoerceValue = coerceDateValue }

    /// GraphQL type for System.Guid
    let Guid : ScalarDefinition<Guid> =
        { Name = "Guid"
          Description =
              Some
                  "The `Guid` scalar type represents a Globaly Unique Identifier value. It's a 128-bit long byte key, that can be serialized to string."
          CoerceInput = coerceGuidInput
          CoerceValue = coerceGuidValue }

    /// GraphQL @include directive.
    let IncludeDirective : DirectiveDef =
        { Name = "include"
          Description =
              Some "Directs the executor to include this field or fragment only when the `if` argument is true."
          Locations =
              DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT
          Args =
              [| { InputFieldDefinition.Name = "if"
                   Description = Some "Included when true."
                   TypeDef = Boolean
                   DefaultValue = None
                   ExecuteInput =
                       variableOrElse (coerceBoolInput
                                       >> Option.map box
                                       >> Option.toObj) } |] }

    /// GraphQL @skip directive.
    let SkipDirective : DirectiveDef =
        { Name = "skip"
          Description = Some "Directs the executor to skip this field or fragment when the `if` argument is true."
          Locations =
              DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT
          Args =
              [| { InputFieldDefinition.Name = "if"
                   Description = Some "Skipped when true."
                   TypeDef = Boolean
                   DefaultValue = None
                   ExecuteInput =
                       variableOrElse (coerceBoolInput
                                       >> Option.map box
                                       >> Option.toObj) } |] }

    /// GraphQL @defer directive.
    let DeferDirective : DirectiveDef =
        { Name = "defer"
          Description = Some "Defers the resolution of this field or fragment"
          Locations =
            DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT ||| DirectiveLocation.FRAGMENT_DEFINITION
          Args = [||] }

    /// GraphQL @stream directive.
    let StreamDirective : DirectiveDef =
        { Name = "stream"
          Description = Some "Streams the resolution of this field or fragment"
          Locations =
            DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT ||| DirectiveLocation.FRAGMENT_DEFINITION
          Args = [||] }

    /// GraphQL @live directive.
    let LiveDirective : DirectiveDef =
        { Name = "live"
          Description = Some "Subscribes for live updates of this field or fragment"
          Locations =
            DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT ||| DirectiveLocation.FRAGMENT_DEFINITION
          Args = [||] }

    let internal matchParameters (methodInfo : MethodInfo) (ctx : ResolveFieldContext) =
        methodInfo.GetParameters() |> Array.map (fun param -> ctx.Arg<obj>(param.Name))
    let inline internal strip (fn : 'In -> 'Out) : obj -> obj = fun i -> upcast fn (i :?> 'In)

    /// Common space for all definition helper methods.
    [<AbstractClass; Sealed>]
    type Define =

        /// <summary>
        /// Creates GraphQL type definition for user defined scalars.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="coerceInput">Function used to resolve .NET object from GraphQL query AST.</param>
        /// <param name="coerceValue">Function used to cross cast to .NET types.</param>
        /// <param name="description">Optional scalar description. Usefull for generating documentation.</param>
        static member Scalar(name : string, coerceInput : Value -> 'T option,
                             coerceValue : obj -> 'T option, ?description : string) : ScalarDefinition<'T> =
            { Name = name
              Description = description
              CoerceInput = coerceInput
              CoerceValue = coerceValue }

        /// <summary>
        /// Creates GraphQL type definition for user defined enums.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="options">List of enum value cases.</param>
        /// <param name="description">Optional enum description. Usefull for generating documentation.</param>
        static member Enum(name : string, options : EnumValue<'Val> list, ?description : string) : EnumDef<'Val> =
            upcast { EnumDefinition.Name = name
                     Description = description
                     Options = options |> List.toArray }

        /// <summary>
        /// Creates a single enum option to be used as argument in <see cref="Schema.Enum"/>.
        /// </summary>
        /// <param name="name">Enum value name. Must be unique in scope of the defining enum.</param>
        /// <param name="value">
        /// .NET value associated with target enum value. All enum values are represented as strings in GraphQL type system.
        /// Enum value will be stringified using `ToString()` method when passing to a client.
        /// </param>
        /// <param name="description">Optional enum value description. Usefull for generating documentation.</param>
        /// <param name="deprecationReason">If set, marks an enum value as deprecated.</param>
        static member EnumValue(name : string, value : 'Val, ?description : string, ?deprecationReason : string) : EnumValue<'Val> =
            { Name = name
              Description = description
              Value = value
              DeprecationReason = deprecationReason }

        /// <summary>
        /// Creates GraphQL custom output object type. It can be used as a valid output but not an input object
        /// (see <see cref="InputObject"/> for more details).
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fieldsFn">
        /// Function which generates a list of fields defined by the current object. Usefull, when object defines recursive dependencies.
        /// </param>
        /// <param name="description">Optional object description. Usefull for generating documentation.</param>
        /// <param name="interfaces">
        /// List of implemented interfaces. Object must explicitly define all fields from all interfaces it implements.
        /// </param>
        /// <param name="isTypeOf">
        /// Optional function used to determine if provided .NET object instance matches current object definition.
        /// </param>
        static member Object(name : string, fieldsFn : unit -> FieldDef<'Val> list, ?description : string,
                             ?interfaces : InterfaceDef list, ?isTypeOf : obj -> bool) : ObjectDef<'Val> =
            upcast { ObjectDefinition.Name = name
                     Description = description
                     FieldsFn =
                         lazy (fieldsFn()
                               |> List.map (fun f -> f.Name, f)
                               |> Map.ofList)
                     Implements = defaultArg (Option.map List.toArray interfaces) [||]
                     IsTypeOf = isTypeOf }

        /// <summary>
        /// Creates GraphQL custom output object type. It can be used as a valid output but not an input object
        /// (see <see cref="InputObject"/> for more details).
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fields">List of fields defined by the current object. </param>
        /// <param name="description">Optional object description. Usefull for generating documentation.</param>
        /// <param name="interfaces">
        /// List of implemented interfaces. Object must explicitly define all fields from all interfaces it implements.
        /// </param>
        /// <param name="isTypeOf">
        /// Optional function used to determine if provided .NET object instance matches current object definition.
        /// </param>
        static member Object(name : string, fields : FieldDef<'Val> list, ?description : string,
                             ?interfaces : InterfaceDef list, ?isTypeOf : obj -> bool) : ObjectDef<'Val> =
            upcast { ObjectDefinition.Name = name
                     Description = description
                     FieldsFn =
                         lazy (fields
                               |> List.map (fun f -> f.Name, f)
                               |> Map.ofList)
                     Implements = defaultArg (Option.map List.toArray interfaces) [||]
                     IsTypeOf = isTypeOf }
        
        /// <summary>
        /// A reference to an Object in the schema. Useful when declaring mutually recursive types across files.
        /// Pass types directly whenever possible.
        /// </summary>
        /// <param name="name">Type name to reference.</param>
        static member ObjectRef(name : string) : ObjectRef<'Val> =
            upcast { ObjectReference.Name = name }

        /// <summary>
        /// Creates a custom GraphQL input object type. Unlike GraphQL objects, input objects are valid input types,
        /// that can be included in GraphQL query strings. Input object maps to a .NET type, which can be strandard
        /// .NET class or struct, or a F# record.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fieldsFn">
        /// Function which generates a list of input fields defined by the current input object. Usefull, when object defines recursive dependencies.
        /// </param>
        /// <param name="description">Optional input object description. Usefull for generating documentation.</param>
        static member InputObject(name : string, fieldsFn : unit -> InputFieldDef list, ?description : string) : InputObjectDefinition<'Out> =
            { Name = name
              FieldsFn = fun () -> fieldsFn() |> List.toArray
              Description = description }

        /// <summary>
        /// Creates a custom GraphQL input object type. Unlike GraphQL objects, input objects are valid input types,
        /// that can be included in GraphQL query strings. Input object maps to a .NET type, which can be strandard
        /// .NET class or struct, or a F# record.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fields">List of input fields defined by the current input object. </param>
        /// <param name="description">Optional input object description. Usefull for generating documentation.</param>
        static member InputObject(name : string, fields : InputFieldDef list, ?description : string) : InputObjectDefinition<'Out> =
            { Name = name
              Description = description
              FieldsFn = fun () -> fields |> List.toArray }

        /// <summary>
        /// Creates the top level subscription object that holds all of the possible subscriptions as fields.
        /// </summary>
        /// <param name="name">Top level name. Must be unique in scope of the current schema.</param>
        /// <param name="fields">List of subscription fields to be defined for the schema. </param>
        /// <param name="description">Optional description. Usefull for generating documentation.</param>
        static member SubscriptionObject<'Val>(name: string, fields: SubscriptionFieldDef<'Val> list, ?description: string):SubscriptionObjectDefinition<'Val> =
            { Name = name
              Fields = (fields |> List.map (fun f -> f.Name, f) |> Map.ofList)
              Description = description }

        /// <summary>
        /// Creates field defined inside object types with automatically generated field resolve function.
        /// Field name must match object's property or field.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">Optional list of arguments used to parametrize field resolution.</param>
        /// <param name="deprecationReason">If set, marks current field as deprecated.</param>
        static member AutoField(name : string, typedef : #OutputDef<'Res>, ?description: string, ?args: InputFieldDef list, ?deprecationReason: string) : FieldDef<'Val> =
            upcast { FieldDefinition.Name = name
                     Description = description
                     TypeDef = typedef
                     Resolve = Resolve.defaultResolve<'Val, 'Res> name
                     Args = defaultArg args [] |> Array.ofList
                     DeprecationReason = deprecationReason
                     Metadata = Metadata.Empty
                     }

        /// <summary>
        /// Creates field defined inside interfaces. When used for objects may cause runtime exceptions due to
        /// lack of resolve function supplied. To use auto generated resolvers use <see cref="AutoField"/>.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        static member Field(name : string, typedef : #OutputDef<'Res>) : FieldDef<'Val> = 
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = typedef
                     Resolve = Undefined
                     Args = [||]
                     DeprecationReason = None
                     Metadata = Metadata.Empty
                     }

        /// <summary>
        /// Creates field defined inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        static member Field(name : string, typedef : #OutputDef<'Res>, 
                            [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res>) : FieldDef<'Val> =
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = typedef
                     Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                     Args = [||]
                     DeprecationReason = None
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string, 
                            [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res>) : FieldDef<'Val> = 

            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                     Args = [||]
                     DeprecationReason = None
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        static member Field(name : string, typedef : #OutputDef<'Res>, args : InputFieldDef list, 
                            [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res>) : FieldDef<'Val> = 
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = typedef
                     Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                     Args = args |> List.toArray
                     DeprecationReason = None
                     Metadata = Metadata.Empty }
        
        /// <summary>
        /// Creates field defined inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string, args : InputFieldDef list, 
                            [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res>) : FieldDef<'Val> =
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                     Args = args |> List.toArray
                     DeprecationReason = None
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type. Fields is marked as deprecated.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string, args : InputFieldDef list, 
                            [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res>, 
                            deprecationReason : string) : FieldDef<'Val> = 
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                     Args = args |> List.toArray
                     DeprecationReason = Some deprecationReason
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, 
                                 [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res>>) : FieldDef<'Val> =
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = typedef
                     Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                     Args = [||]
                     DeprecationReason = None
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                 [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res>>) : FieldDef<'Val> =
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                     Args = [||]
                     DeprecationReason = None
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                 args : InputFieldDef list, 
                                 [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res>>) : FieldDef<'Val> =
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                     Args = args |> List.toArray
                     DeprecationReason = None
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates field defined inside object type with asynchronously resolved value. Fields is marked as deprecated.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="typedef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="resolve">Expression used to resolve value from defining object.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                 args : InputFieldDef list, 
                                 [<ReflectedDefinition(true)>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res>>, 
                                 deprecationReason : string) : FieldDef<'Val> =
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                     Args = args |> List.toArray
                     DeprecationReason = Some deprecationReason
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates a custom defined field using a custom field execution function.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="execField">Expression used to execute the field.</param>
        static member CustomField(name : string, [<ReflectedDefinition(true)>] execField : Expr<ExecuteField>) : FieldDef<'Val> =
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = Obj
                     Resolve = ResolveExpr(execField)
                     Args = [||]
                     DeprecationReason = None
                     Metadata = Metadata.Empty }

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = None
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty } 

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                        tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = None
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver } 

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        description: string,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        description: string,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                        tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver } 

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        description: string,
                                        args: InputFieldDef list,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = args |> List.toArray
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        description: string,
                                        args: InputFieldDef list,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                        tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = args |> List.toArray
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver } 

        /// <summary>
        /// Creates a subscription field inside object type. Field is marked as deprecated.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        description: string,
                                        args: InputFieldDef list,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                        deprecationReason : string): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = Some deprecationReason
                     Args = args |> List.toArray
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member SubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                        description: string,
                                        args: InputFieldDef list,
                                        [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> 'Output option>,
                                        tagsResolver : TagsResolver,
                                        deprecationReason : string): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = Some deprecationReason
                     Args = args |> List.toArray
                     Filter = Resolve.Filter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver } 

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = None
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                             tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = None
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             description: string,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             description: string,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                             tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = [||]
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             description: string,
                                             args: InputFieldDef list,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = args |> List.toArray
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             description: string,
                                             args: InputFieldDef list,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                             tagsResolver : TagsResolver): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = None
                     Args = args |> List.toArray
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value. Field is marked as deprecated.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             description: string,
                                             args: InputFieldDef list,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                             deprecationReason : string): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = Some deprecationReason
                     Args = args |> List.toArray
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = fun _ -> Seq.empty }

        /// <summary>
        /// Creates a subscription field inside object type, with asynchronously resolved value.
        /// </summary>
        /// <param name="name">Field name. Must be unique in scope of the defining object.</param>
        /// <param name="rootdef">GraphQL type definition of the root field's type.</param>
        /// <param name="outputdef">GraphQL type definition of the current field's type.</param>
        /// <param name="description">Optional field description. Usefull for generating documentation.</param>
        /// <param name="args">List of field arguments used to parametrize resolve expression output.</param>
        /// <param name="filter">A filter function which decides if the field should be published to clients or not, by returning it as Some or None.</param>
        /// <param name="tagsResolver">A function that resolves subscription tags, used to choose which filter functions will be used when publishing to subscribers.</param>
        /// <param name="deprecationReason">Deprecation reason.</param>
        static member AsyncSubscriptionField(name: string, rootdef: #OutputDef<'Root>, outputdef: #OutputDef<'Output>,
                                             description: string,
                                             args: InputFieldDef list,
                                             [<ReflectedDefinition(true)>] filter: Expr<ResolveFieldContext -> 'Root -> 'Input -> Async<'Output option>>,
                                             tagsResolver : TagsResolver,
                                             deprecationReason : string): SubscriptionFieldDef<'Root, 'Input, 'Output> =
            upcast { Name = name
                     Description = Some description
                     RootTypeDef = rootdef
                     OutputTypeDef = outputdef
                     DeprecationReason = Some deprecationReason
                     Args = args |> List.toArray
                     Filter = Resolve.AsyncFilter(typeof<'Root>, typeof<'Input>, typeof<'Output>, filter)
                     Metadata = Metadata.Empty
                     TagsResolver = tagsResolver }
        

        /// <summary>
        /// Creates an input field. Input fields are used like ordinary fileds in case of <see cref="InputObject"/>s,
        /// and can be used to define arguments to objects and interfaces fields.
        /// </summary>
        /// <param name="name">
        /// Field name. Must be unique in scope of the defining input object or withing field's argument list.
        /// </param>
        /// <param name="typedef">GraphQL type definition of the current input type</param>
        /// <param name="defaultValue">If defined, this value will be used when no matching input has been provided by the requester.</param>
        /// <param name="description">Optional input description. Usefull for generating documentation.</param>
        static member Input(name : string, typedef : #InputDef<'In>, ?defaultValue : 'In, ?description : string) : InputFieldDef =
            upcast { InputFieldDefinition.Name = name
                     Description = description
                     TypeDef = typedef
                     DefaultValue = defaultValue
                     ExecuteInput = Unchecked.defaultof<ExecuteInput> }

        /// <summary>
        /// Creates a custom GraphQL interface type. It's needs to be implemented by object types and should not be used alone.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fieldsFn">
        /// Function which generates a list of fields defined by the current interface. Usefull, when object defines recursive dependencies.
        /// </param>
        /// <param name="description">Optional input description. Usefull for generating documentation.</param>
        /// <param name="resolveType">Optional function used to resolve actual Object definition of the .NET object provided as an input.</param>
        static member Interface(name : string, fieldsFn : unit -> FieldDef<'Val> list, ?description : string,
                                ?resolveType : obj -> ObjectDef) : InterfaceDef<'Val> =
            upcast { InterfaceDefinition.Name = name
                     Description = description
                     FieldsFn = fun () -> fieldsFn() |> List.toArray
                     ResolveType = resolveType }

        /// <summary>
        /// Creates a custom GraphQL interface type. It's needs to be implemented by object types and should not be used alone.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="fields">List of fields defined by the current interface.</param>
        /// <param name="description">Optional input description. Usefull for generating documentation.</param>
        /// <param name="resolveType">Optional function used to resolve actual Object definition of the .NET object provided as an input.</param>
        static member Interface(name : string, fields : FieldDef<'Val> list, ?description : string,
                                ?resolveType : obj -> ObjectDef) : InterfaceDef<'Val> =
            upcast { InterfaceDefinition.Name = name
                     Description = description
                     FieldsFn = fun () -> fields |> List.toArray
                     ResolveType = resolveType }

        /// <summary>
        /// Creates a custom GraphQL union type, materialized as one of the types defined. It can be used as interface/object type field.
        /// In order to work with F# discriminated unions, <paramref name="resolveValue"/> function may be used to unwrap objects
        /// nested as discriminated union cases.
        /// </summary>
        /// <param name="name">Type name. Must be unique in scope of the current schema.</param>
        /// <param name="options">Interfaces or objects used as union cases.</param>
        /// <param name="resolveValue">Given F# discriminated union as input, returns .NET object valid with one of the defined GraphQL union cases.</param>
        /// <param name="resolveType">Resolves an Object definition of one of possible types, give input object.</param>
        /// <param name="description">Optional union description. Usefull for generating documentation.</param>
        static member Union(name : string, options : ObjectDef list, resolveValue : 'In -> 'Out,
                            ?resolveType : 'In -> ObjectDef, ?description : string) : UnionDef<'In> =
            upcast { UnionDefinition.Name = name
                     Description = description
                     Options = options |> List.toArray
                     ResolveType = resolveType
                     ResolveValue = resolveValue }

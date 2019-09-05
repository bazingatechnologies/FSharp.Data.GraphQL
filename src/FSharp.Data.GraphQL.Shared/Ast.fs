/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
/// Copyright (c) 2019 Logibit AB
namespace FSharp.Data.GraphQL.Ast

open System

/// There are three types of operations that GraphQL models:
///
/// - query – a read‐only fetch.
/// - mutation – a write followed by a fetch.
/// - subscription – a long‐lived request that fetches data in response to source events.
///
/// Each operation is represented by an optional operation name and a selection set.
/// https://graphql.github.io/graphql-spec/June2018/#OperationType
type OperationType =
    | Query
    | Mutation
    | Subscription

/// 2.9 Input Values
type Value =
    /// 2.9.1 Int Value
    | IntValue of int64
    /// 2.9.2 Float Value
    | FloatValue of double
    /// 2.9.3 Boolean Value
    | BooleanValue of bool
    /// 2.9.4 String Value
    | StringValue of string
    /// 2.9.5 Null Value
    | NullValue
    /// 2.9.6 Enum Value
    | EnumValue of string
    /// 2.9.7 List Value
    | ListValue of Value list
    /// 2.9.8 Input Object Values
    | ObjectValue of Map<string, Value>
    /// 2.10 Variables
    | Variable of string

/// 2.6 Arguments
/// Fields are conceptually functions which return values, and occasionally accept arguments which alter their behavior. These arguments often map directly to function arguments within a GraphQL server’s implementation.
/// https://graphql.github.io/graphql-spec/June2018/#sec-Language.Arguments
type Argument = {
    Name: string
    Value: Value
}

/// 2.2.10 Directives
type Directive =
    {
        Name: string
        Arguments: Argument list
    }
    member x.If = x.Arguments |> List.find (fun arg -> arg.Name = "if")

/// 2.2.6 Fragments
type FragmentSpread = {
    Name: string
    Directives: Directive list
}

type FragmentDefinition = {
    Name: string option
    /// 2.2.6.1 Type Conditions
    TypeCondition: string option
    Directives: Directive list
    SelectionSet: Selection list
}


/// 2.2.2 Selection Sets
and Selection =
    | Field of Field
    | FragmentSpread of FragmentSpread
    /// 2.2.6.2 Inline Fragments
    | InlineFragment of FragmentDefinition
    member x.Directives =
        match x with
        | Field f -> f.Directives
        | FragmentSpread s -> s.Directives
        | InlineFragment f -> f.Directives

/// 2.2.3 Fields
and Field =
    {
        /// 2.2.5 Field Alias
        Alias: string option
        Name: string
        Arguments: Argument list
        Directives: Directive list
        SelectionSet: Selection list
    }
    member x.AliasOrName =
        match x.Alias with
        | Some alias -> alias
        | None -> x.Name


/// 2.2.9 Input Types
type InputType =
    | NamedType of string
    | ListType of InputType
    | NonNullType of InputType
    override x.ToString() =
        let rec str = function
            | NamedType name -> name
            | ListType inner -> "[" + (str inner) + "]"
            | NonNullType inner -> (str inner) + "!"
        str x



/// 2.2.8 Variables
type VariableDefinition =
    { VariableName: string
      Type: InputType
      DefaultValue: Value option }

//NOTE: For references, see https://facebook.github.io/graphql/
/// 2.3 Operations
type OperationDefinition = {
    /// Defaults to `query`; "query shorthand"
    OperationType: OperationType
    Name: string option
    /// May be empty
    VariableDefinitions: VariableDefinition list
    /// May be empty
    Directives: Directive list
    /// May not be empty
    SelectionSet: Selection list
}

type InputDefinition = string

/// GraphQL field input definition. Can be used as fields for
/// input objects or as arguments for any ordinary field definition.
type InputFieldDefinition =
    {
        /// Name of the input field / argument.
        Name : string
        /// Optional input field / argument description.
        Description : string option
        /// GraphQL type definition of the input type.
        TypeDef : InputDefinition
        /// Optional default input value - used when no input was provided.
        DefaultValue : Value option
    }

/// https://graphql.github.io/graphql-spec/June2018/#sec-Language.Operations
type OperationTypeDefinition = {
    NamedType: string
    Operation: OperationType
}

/// A GraphQL service’s collective type system capabilities are referred to as that service’s “schema”. A schema is defined in terms of the types and directives it supports as well as the root operation types for each kind of operation: query, mutation, and subscription; this determines the place in the type system where those operations begin.
/// A GraphQL schema must itself be internally valid. This section describes the rules for this validation process where relevant.
/// https://graphql.github.io/graphql-spec/June2018/#SchemaDefinition
type SchemaDefinition = {
    Directives: Directive list
    OperationTypes: OperationTypeDefinition
}

type InputValueDefinition = {
    Description: string option
    Name: string
    Type: InputType
    DefaultValue: Value option
}

/// https://graphql.github.io/graphql-spec/June2018/#FieldDefinition
type FieldDefinition = {
    Description: string option
    Name: string
    Arguments: InputValueDefinition []
    Type: InputType
    Directives: Directive list
}

// TypeDefinitions:

/// Scalar types represent primitive leaf values in a GraphQL type system. GraphQL responses take the form of a hierarchical tree; the leaves on these trees are GraphQL scalars.
/// https://graphql.github.io/graphql-spec/June2018/#ScalarTypeDefinition
type ScalarTypeDefinition = {
    Description: string option
    Name: string
    Directives: Directive list
}

type ScalarTypeExtension = {
    Name: string
    Directives: Directive list
}

/// GraphQL queries are hierarchical and composed, describing a tree of information. While Scalar types describe the leaf values of these hierarchical queries, Objects describe the intermediate levels.
/// https://graphql.github.io/graphql-spec/June2018/#sec-Objects
type ObjectTypeDefinition = {
    Description: string option
    Name: string
    /// May be empty
    Interfaces: string []
    /// May be empty
    Directives: Directive list
    Fields: FieldDefinition []
}

/// Object type extensions are used to represent a type which has been extended from some original type. For example, this might be used to represent local data, or by a GraphQL service which is itself an extension of another GraphQL service.
///
/// Either:
/// - `Fields` is non-empty, and all others may be, xor
/// - `Directives` is non-empty, and all others may be, xor
/// - `Interfaces` is non-empty, and all others may be.
///
/// https://graphql.github.io/graphql-spec/June2018/#ObjectTypeExtension
type ObjectTypeExtension = {
    /// May be empty
    Interfaces: string []
    /// May be empty
    Directives: Directive list
    /// May be empty
    Fields: FieldDefinition []
}

/// GraphQL interfaces represent a list of named fields and their arguments. GraphQL objects can then implement these interfaces which requires that the object type will define all fields defined by those interfaces.
/// https://graphql.github.io/graphql-spec/June2018/#InterfaceTypeDefinition
type InterfaceTypeDefinition = {
    Description: string option
    Name: string
    Directives: Directive list
    Fields: FieldDefinition []
}

type InterfaceTypeExtension = {
    Name: string
    Directives: Directive list
    Fields: FieldDefinition []
}

type UnionMemberType = string

/// GraphQL Unions represent an object that could be one of a list of GraphQL Object types, but provides for no guaranteed fields between those types. They also differ from interfaces in that Object types declare what interfaces they implement, but are not aware of what unions contain them.
/// https://graphql.github.io/graphql-spec/June2018/#UnionTypeDefinition
type UnionTypeDefinition = {
    Description: string option
    Name: string
    Directives: Directive list
    Types: UnionMemberType []
}

type UnionTypeExtension = {
    Name: string
    Directives: Directive list
    Types: UnionMemberType []
}

/// Enums are not references for a numeric value, but are unique values in their own right. They may serialize as a string: the name of the represented value.
/// Most often a name like "BLUE" or "RED".
/// https://graphql.github.io/graphql-spec/June2018/#EnumValueDefinition
type EnumValueDefinition = {
    Description: string option
    Name: string
    Directives: Directive list
}

/// GraphQL Enum types, like scalar types, also represent leaf values in a GraphQL type system. However Enum types describe the set of possible values.
/// https://graphql.github.io/graphql-spec/June2018/#EnumTypeDefinition
type EnumTypeDefinition = {
    Description: string option
    Name: string
    Directives: Directive list
    Values: EnumValueDefinition[]
}

/// Enum type extensions are used to represent an enum type which has been extended from some original enum type. For example, this might be used to represent additional local data, or by a GraphQL service which is itself an extension of another GraphQL service.
/// https://graphql.github.io/graphql-spec/June2018/#sec-Enum-Extensions
type EnumTypeExtension = {
    Name: string
    Directives: Directive list
    Values: EnumValueDefinition[]
}


/// Fields may accept arguments to configure their behavior. These inputs are often scalars or enums, but they sometimes need to represent more complex values.
/// https://graphql.github.io/graphql-spec/June2018/#InputObjectTypeDefinition
type InputObjectTypeDefinition = {
    Description: string option
    Name: string
    Directives: Directive list
    Fields: InputValueDefinition []
}

/// 3.10.1 https://graphql.github.io/graphql-spec/June2018/#sec-Input-Object-Extensions
/// Input object type extensions are used to represent an input object type which has been extended from some original input object type. For example, this might be used by a GraphQL service which is itself an extension of another GraphQL service.
type InputObjectTypeExtension = {
    Name: string
    Directives: Directive list
    Fields: InputValueDefinition []
}

type TypeDefinition =
    | ScalarTypeDefinition of ScalarTypeDefinition
    | ObjectTypeDefinition of ObjectTypeDefinition
    | InterfaceTypeDefinition of InterfaceTypeDefinition
    | UnionTypeDefinition of UnionTypeDefinition
    | EnumTypeDefinition of EnumTypeDefinition
    | InputObjectTypeDefinition of InputObjectTypeDefinition

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

/// 3.13
/// DirectiveDefinition:
///  Description opt  directive @ `Name` `ArgumentsDefinition` opt  on `DirectiveLocations`
///
/// https://graphql.github.io/graphql-spec/June2018/#DirectiveDefinition
/// https://graphql.github.io/graphql-spec/June2018/#sec-Type-System.Directives
type DirectiveDefinition =
    { /// Directive's name - it's NOT '@' prefixed.
      Name : string
      /// Optional directive description.
      Description : string option
      /// Directive location - describes, which part's of the query AST
      /// are valid places to include current directive to.
      Locations : DirectiveLocation
      /// Array of arguments defined within that directive.
      Args : InputFieldDefinition []
    }

type TypeSystemDefinition =
    | SchemaDefinition of SchemaDefinition
    | TypeDefinition of TypeDefinition
    | DirectiveDefinition of DirectiveDefinition

/// Schema extensions are used to represent a schema which has been extended from an original schema. For example, this might be used by a GraphQL service which adds additional operation types, or additional directives to an existing schema.
/// https://graphql.github.io/graphql-spec/June2018/#SchemaExtension
type SchemaExtension =
    {
        /// May be empty. Any directives provided must not already apply to the original Schema.
        Directives: Directive list
        /// May be empty
        OperationTypes: OperationTypeDefinition list
    }

/// Type extensions are used to represent a GraphQL type which has been extended from some original type. For example, this might be used by a local service to represent additional fields a GraphQL client only accesses locally.
/// https://graphql.github.io/graphql-spec/June2018/#TypeExtension
type TypeExtension =
    | ScalarTypeExtension of ScalarTypeExtension
    | ObjectTypeExtension of ObjectTypeExtension
    | InterfaceTypeExtension of InterfaceTypeExtension
    | UnionTypeExtension of UnionTypeExtension
    | EnumTypeExtension of EnumTypeExtension
    | InputObjectTypeExtension of InputObjectTypeExtension

/// Type system extensions are used to represent a GraphQL type system which has been extended from some original type system. For example, this might be used by a local service to represent data a GraphQL client only accesses locally, or by a GraphQL service which is itself an extension of another GraphQL service.
/// 3.1 https://graphql.github.io/graphql-spec/June2018/#sec-Type-System-Extensions
type TypeSystemExtension =
    | SchemaExtension of SchemaExtension
    | TypeExtension of TypeExtension

type Definition =
    | OperationDefinition of OperationDefinition
    | FragmentDefinition of FragmentDefinition
    | TypeSystemDefinition of TypeSystemDefinition
    | TypeSystemExtension of TypeSystemExtension

    member x.Name =
        match x with
        | OperationDefinition op -> op.Name
        | FragmentDefinition frag -> frag.Name
        | TypeSystemDefinition _ -> None
        | TypeSystemExtension _ -> None

    member x.Directives =
        match x with
        | OperationDefinition op -> op.Directives
        | FragmentDefinition frag -> frag.Directives
        | _ -> []

    member x.SelectionSet =
        match x with
        | OperationDefinition op -> op.SelectionSet
        | FragmentDefinition frag -> frag.SelectionSet
        | _ -> []

/// 2.2 Query Document
/// A GraphQL Document describes a complete file or request string operated on by a GraphQL service or client. A document contains multiple definitions, either executable or representative of a GraphQL type system.
///
/// Documents are only executable by a GraphQL service if they contain an OperationDefinition and otherwise only contain ExecutableDefinition. However documents which do not contain OperationDefinition or do contain TypeSystemDefinition or TypeSystemExtension may still be parsed and validated to allow client tools to represent many GraphQL uses which may appear across many individual files.
///
/// If a Document contains only one operation, that operation may be unnamed or represented in the shorthand form, which omits both the query keyword and operation name. Otherwise, if a GraphQL Document contains multiple operations, each operation must be named. When submitting a Document with multiple operations to a GraphQL service, the name of the desired operation to be executed must also be provided.
///
/// GraphQL services which only seek to provide GraphQL query execution may choose to only include ExecutableDefinition and omit the TypeSystemDefinition and TypeSystemExtension rules from Definition.
///
/// https://graphql.github.io/graphql-spec/June2018/#sec-Language.Document
type Document =
    {
        Definitions: Definition list
    }

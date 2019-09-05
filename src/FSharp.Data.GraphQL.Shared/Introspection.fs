/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Introspection

#nowarn "40"

open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Extensions
open System.Reflection

let internal getFieldValue name o =
    let property = o.GetType().GetTypeInfo().GetDeclaredProperty(name, ignoreCase=true)
    if isNull property then null else property.GetValue(o, null)

/// Common GraphQL query that may be used to retrieve overall data 
/// about schema type system itself.
let [<Literal>] IntrospectionQuery = """query IntrospectionQuery {
__schema {
  queryType {
    name
  }
  mutationType {
    name
  }
  subscriptionType {
    name
  }
  types {
    ...FullType
  }
  directives {
    name
    description
    locations
    args {
      ...InputValue
    }
  }
}
}

fragment FullType on __Type {
kind
name
description
fields(includeDeprecated: true) {
  name
  description
  args {
    ...InputValue
  }
  type {
    ...TypeRef
  }
  isDeprecated
  deprecationReason
}
inputFields {
  ...InputValue
}
interfaces {
  ...TypeRef
}
enumValues(includeDeprecated: true) {
  name
  description
  isDeprecated
  deprecationReason
}
possibleTypes {
  ...TypeRef
}
}

fragment InputValue on __InputValue {
name
description
type {
  ...TypeRef
}
defaultValue
}

fragment TypeRef on __Type {
kind
name
ofType {
  kind
  name
  ofType {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
            }
          }
        }
      }
    }
  }
}
}"""
    
/// GraphQL enum describing kind of the GraphQL type definition.
/// Can be one of: SCALAR, OBJECT, INTERFACE, UNION, ENUM, LIST,
/// NON_NULL or INPUT_OBJECT.
let __TypeKind =
  Define.Enum(
    name = "__TypeKind", 
    description = "An enum describing what kind of type a given __Type is.",
    options = [
        Define.EnumValue("SCALAR", TypeKind.SCALAR, "Indicates this type is a scalar.")
        Define.EnumValue("OBJECT", TypeKind.OBJECT, "Indicates this type is an object. `fields` and `interfaces` are valid fields.")
        Define.EnumValue("INTERFACE", TypeKind.INTERFACE, "Indicates this type is an interface. `fields` and `possibleTypes` are valid fields.")
        Define.EnumValue("UNION", TypeKind.UNION, "Indicates this type is a union. `possibleTypes` is a valid field.")
        Define.EnumValue("ENUM", TypeKind.ENUM, "Indicates this type is an enum. `enumValues` is a valid field.")
        Define.EnumValue("INPUT_OBJECT", TypeKind.INPUT_OBJECT, "Indicates this type is an input object. `inputFields` is a valid field.")
        Define.EnumValue("LIST", TypeKind.LIST, "Indicates this type is a list. `ofType` is a valid field.")
        Define.EnumValue("NON_NULL", TypeKind.NON_NULL, "Indicates this type is a non-null. `ofType` is a valid field.")
    ])
    
/// GraphQL enum describing kind of location for a particular directive
/// to be used in. Can be one of: QUERY, MUTATION, SUBSCRIPTION, FIELD,
/// FRAGMENT_DEFINITION, FRAGMENT_SPREAD or INLINE_FRAGMENT.
/// FRAGMENT_DEFINITION, FRAGMENT_SPREAD, INLINE_FRAGMENT, SCHEMA,
/// SCALAR, OBJECT, FIELD_DEFINITION, ARGUMENT_DEFINITION,
/// INTERFACE, UNION, ENUM, ENUM_VALUE, INPUT_OBJECT or
/// INPUT_FIELD_DEFINITION.
let __DirectiveLocation =
  Define.Enum(
    name = "__DirectiveLocation",
    description = "A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies.",
    options = [
        Define.EnumValue("QUERY", DirectiveLocation.QUERY, "Location adjacent to a query operation.")
        Define.EnumValue("MUTATION", DirectiveLocation.MUTATION, "Location adjacent to a mutation operation.")
        Define.EnumValue("SUBSCRIPTION", DirectiveLocation.SUBSCRIPTION, "Location adjacent to a subscription operation.")
        Define.EnumValue("FIELD", DirectiveLocation.FIELD, "Location adjacent to a field.")
        Define.EnumValue("FRAGMENT_DEFINITION", DirectiveLocation.FRAGMENT_DEFINITION, "Location adjacent to a fragment definition.")
        Define.EnumValue("FRAGMENT_SPREAD", DirectiveLocation.FRAGMENT_SPREAD, "Location adjacent to a fragment spread.")
        Define.EnumValue("INLINE_FRAGMENT", DirectiveLocation.INLINE_FRAGMENT, "Location adjacent to an inline fragment.")
        Define.EnumValue("SCHEMA", DirectiveLocation.SCHEMA, "Location adjacent to a schema IDL definition.")
        Define.EnumValue("SCALAR", DirectiveLocation.SCALAR, "Location adjacent to a scalar IDL definition.")
        Define.EnumValue("OBJECT", DirectiveLocation.OBJECT, "Location adjacent to an object IDL definition.")
        Define.EnumValue("FIELD_DEFINITION", DirectiveLocation.FIELD_DEFINITION, "Location adjacent to a field IDL definition.")
        Define.EnumValue("ARGUMENT_DEFINITION", DirectiveLocation.ARGUMENT_DEFINITION, "Location adjacent to a field argument IDL definition.")
        Define.EnumValue("INTERFACE", DirectiveLocation.INTERFACE, "Location adjacent to an interface IDL definition.")
        Define.EnumValue("UNION", DirectiveLocation.UNION, "Location adjacent to an union IDL definition.")
        Define.EnumValue("ENUM", DirectiveLocation.ENUM, "Location adjacent to an enum IDL definition.")
        Define.EnumValue("ENUM_VALUE", DirectiveLocation.ENUM_VALUE, "Location adjacent to an enum value definition.")
        Define.EnumValue("INPUT_OBJECT", DirectiveLocation.INPUT_OBJECT, "Location adjacent to an input object IDL definition.")
        Define.EnumValue("INPUT_FIELD_DEFINITION", DirectiveLocation.INPUT_FIELD_DEFINITION, "Location adjacent to an input object field IDL definition.")
    ])
    
let inline private findIntrospected (ctx: ResolveFieldContext) name = ctx.Schema.Introspected.Types |> Seq.find (fun x -> x.Name = name)

/// The fundamental unit of any GraphQL Schema is the type. There are many 
/// kinds of types in GraphQL as represented by the `__TypeKind` enum. 
/// Depending on the kind of a type, certain fields describe information about 
/// that type. Scalar types provide no information beyond a name and description, 
/// while Enum types provide their values. Object and Interface types provide 
/// the fields they describe. Abstract types, Union and Interface, provide 
/// the Object types possible at runtime. List and NonNull types compose other types.
let rec __Type =
  Define.Object<IntrospectionTypeRef>(
    name = "__Type",
    description = """The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum. Depending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name and description, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types.""",
    fieldsFn = fun () ->
    [
        Define.Field("kind", __TypeKind, fun _ t -> t.Kind)
        Define.Field("name", Nullable String, resolve = fun _ t -> t.Name)
        Define.Field("description", Nullable String, resolve = fun _ t -> t.Description)
        Define.Field("fields", Nullable (ListOf __Field),
            args = [Define.Input("includeDeprecated", Boolean, false) ],
            resolve = fun ctx t ->
                match t.Name with
                | None -> None
                | Some name ->
                    let found = findIntrospected ctx name
                    match ctx.TryArg "includeDeprecated" with
                    | Some true ->  found.Fields |> Option.map Array.toSeq
                    | _ -> found.Fields |> Option.map (fun x -> upcast Array.filter (fun f -> not f.IsDeprecated) x))
        Define.Field("interfaces", Nullable (ListOf __Type), resolve = fun ctx t -> 
            match t.Name with 
            | None -> None
            | Some name ->
                let found = findIntrospected ctx name
                found.Interfaces |> Option.map Array.toSeq )
        Define.Field("possibleTypes", Nullable (ListOf __Type), resolve = fun ctx t -> 
            match t.Name with 
            | None -> None
            | Some name ->
                let found = findIntrospected ctx name
                found.PossibleTypes |> Option.map Array.toSeq)
        Define.Field("enumValues", Nullable (ListOf __EnumValue),
            args = [Define.Input("includeDeprecated", Boolean, false) ], resolve = fun ctx t ->
            match t.Name with 
            | None -> None
            | Some name ->
                let found = findIntrospected ctx name
                match ctx.TryArg "includeDeprecated" with
                | None | Some false -> found.EnumValues |> Option.map Array.toSeq
                | Some true -> found.EnumValues |> Option.map (fun x -> upcast  (x |> Array.filter (fun f -> not f.IsDeprecated))))
        Define.Field("inputFields", Nullable (ListOf __InputValue), resolve = fun ctx t ->
            match t.Name with 
            | None -> None
            | Some name ->
                let found = findIntrospected ctx name
                found.InputFields |> Option.map Array.toSeq)
        Define.Field("ofType", Nullable __Type, resolve = fun _ t -> t.OfType)
    ])
   
/// Arguments provided to Fields or Directives and the input fields of an 
/// InputObject are represented as Input Values which describe their type
/// and optionally a default value.
and __InputValue =
  Define.Object<IntrospectionInputVal>(
    name = "__InputValue",
    description = "Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value.",
    fieldsFn = fun () ->
    [
        Define.Field("name", String, resolve = fun _ f -> f.Name)
        Define.Field("description", Nullable String, resolve = fun _ f -> f.Description)
        Define.Field("type", __Type, resolve = fun _ f -> f.Type)
        Define.Field("defaultValue", Nullable String, fun _ f -> f.DefaultValue)
    ])
    
/// Object and Interface types are described by a list of Fields, each of 
/// which has a name, potentially a list of arguments, and a return type.
and __Field =
  Define.Object<IntrospectionField>(
    name = "__Field",
    description = "Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type.",
    fieldsFn = fun () ->
    [
        Define.Field("name", String, fun _ f -> f.Name)
        Define.Field("description", Nullable String, fun _ f -> f.Description)
        Define.Field("args", ListOf __InputValue, fun _ f -> f.Args)
        Define.Field("type", __Type, fun _ f -> f.Type)
        Define.Field("isDeprecated", Boolean, resolve = fun _ f -> f.IsDeprecated)
        Define.Field("deprecationReason", Nullable String, fun _ f -> f.DeprecationReason)
    ])
    
/// One possible value for a given Enum. Enum values are unique values, 
/// not a placeholder for a string or numeric value. However an Enum value 
/// is returned in a JSON response as a string.
and __EnumValue =
  Define.Object<IntrospectionEnumVal>(
    name = "__EnumValue",
    description = "One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string.",
    fieldsFn = fun () ->
    [
        Define.Field("name", String, resolve = fun _ e -> e.Name)
        Define.Field("description", Nullable String, resolve = fun _ e -> e.Description)
        Define.Field("isDeprecated", Boolean, resolve = fun _ e -> Option.isSome e.DeprecationReason)
        Define.Field("deprecationReason", Nullable String, resolve = fun _ e -> e.DeprecationReason)
    ])

and private oneOf (compared: DirectiveLocation []) (comparand: DirectiveLocation) =
    let c = int comparand
    compared |> Array.exists (fun cc -> c &&& (int cc) <> 0)
    
/// A GraphQL Directive provides a way to describe alternate runtime execution 
/// and type validation behavior in a GraphQL document. In some cases, you need 
/// to provide options to alter GraphQL’s execution behavior in ways field 
/// arguments will not suffice, such as conditionally including or skipping a field. 
/// Directives provide this by describing additional information to the executor.
and __Directive = 
  Define.Object<IntrospectionDirective>(
    name = "__Directive",
    description = """A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document. In some cases, you need to provide options to alter GraphQL’s execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor.""",
    fieldsFn = fun () ->
    [
        Define.Field("name", String, resolve = fun _ directive -> directive.Name)
        Define.Field("description", Nullable String, resolve = fun _ directive -> directive.Description)
        Define.Field("locations", ListOf __DirectiveLocation, resolve = fun _ directive -> directive.Locations)
        Define.Field("args", ListOf __InputValue, resolve = fun _ directive -> directive.Args)
        Define.Field("onOperation", Boolean, resolve = fun _ d -> d.Locations |> Seq.exists (oneOf [| DirectiveLocation.QUERY; DirectiveLocation.MUTATION; DirectiveLocation.SUBSCRIPTION |]))
        Define.Field("onFragment", Boolean, resolve = fun _ d -> d.Locations |> Seq.exists (oneOf [| DirectiveLocation.FRAGMENT_SPREAD; DirectiveLocation.INLINE_FRAGMENT; DirectiveLocation.FRAGMENT_DEFINITION |]))
        Define.Field("onField", Boolean, resolve = fun _ d -> d.Locations |> Seq.exists (oneOf [| DirectiveLocation.FIELD |]))
    ])
    
/// GraphQL object defining capabilities of GraphQL server. It exposes 
/// all available types and directives on the server, as well as the 
/// entry points for query, mutation, and subscription operations.
and __Schema =
  Define.Object<IntrospectionSchema>(
    name = "__Schema",
    description = "A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations.",
    fieldsFn = fun () ->
    [
        Define.Field("types", ListOf __Type, description = "A list of all types supported by this server.", resolve = fun _ schema -> schema.Types |> Array.map IntrospectionTypeRef.Named)
        Define.Field("queryType", __Type, description = "The type that query operations will be rooted at.", resolve = fun _ schema -> schema.QueryType)
        Define.Field("mutationType", Nullable __Type, description = "If this server supports mutation, the type that mutation operations will be rooted at.", resolve = fun _ schema -> schema.MutationType)
        Define.Field("subscriptionType", Nullable __Type, description = "If this server support subscription, the type that subscription operations will be rooted at.", resolve = fun _ schema -> schema.SubscriptionType)
        Define.Field("directives", ListOf __Directive, description = "A list of all directives supported by this server.", resolve = fun _  schema -> schema.Directives)
    ])
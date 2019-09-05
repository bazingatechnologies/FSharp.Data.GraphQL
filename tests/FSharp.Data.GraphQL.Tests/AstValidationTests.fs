/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.AstValidationTests

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL.Validation.Ast
open Xunit
open FSharp.Data.GraphQL.Types

type Command =
    | SIT = 1
    | HEEL = 2
    | JUMP = 3
    | CLEAN_UP_HOUSE = 4

type IPet =
    abstract Name : string

type ISentient =
    abstract Name : string

type Human =
    { Name : string }
    interface ISentient with
        member x.Name = x.Name

type Alien =
    { Name : string }
    interface ISentient with
        member x.Name = x.Name

type Dog = 
    { Name : string
      BarkVolume : int
      Nickname : string
      KnownCommands : Command list
      IsHouseTrainedAtOtherHomes : bool
      IsHouseTrainedAtOwnHome : bool }
    member x.DoesKnowCommand(cmd : Command) = x.KnownCommands |> List.contains cmd
    member x.IsHouseTrained(atOtherHomes : bool) = if atOtherHomes then x.IsHouseTrainedAtOtherHomes else x.IsHouseTrainedAtOwnHome
    interface IPet with
        member x.Name = x.Name

type ComplexInput =
    { Name : string option
      Owner : string option }

type Cat = 
    { Name : string
      MeowVolume : int
      Nickname : string
      KnownCommands : Command list }
    member x.DoesKnowCommand(cmd : Command) = x.KnownCommands |> List.contains cmd
    interface IPet with
        member x.Name = x.Name

type CatOrDog =
    | Cat of Cat
    | Dog of Dog

type HumanOrAlien =
    | Human of Human
    | Alien of Alien

type DogOrHuman =
    | Dog of Dog
    | Human of Human

type Root =
    { CatOrDog : CatOrDog
      HumanOrAlien : HumanOrAlien
      DogOrHuman : DogOrHuman
      Pet : IPet
      Sentient : ISentient
      Human : Human }
    member __.FindDog(_ : ComplexInput) : Dog option = None
    member __.BooleanList(_ : bool list option) : bool option = None

let Command =
    Define.Enum<Command>(
        name = "Command",
        options = 
            [ Define.EnumValue("SIT", Command.SIT)
              Define.EnumValue("HEEL", Command.HEEL)
              Define.EnumValue("JUMP", Command.JUMP) ])

let Pet = Define.Interface<IPet>("Pet", [ Define.Field("name", String) ])

let Sentient = Define.Interface<ISentient>("Sentient", [ Define.Field("name", String) ])

let ComplexInput =
    Define.InputObject<ComplexInput>(
        name = "ComplexInput",
        fields = [ Define.Input("name", Nullable String); Define.Input("owner", Nullable String) ])

let Dog = 
    Define.Object<Dog>(
        name = "Dog", 
        fields = 
            [ Define.AutoField("name", String)
              Define.AutoField("barkVolume", Int)
              Define.AutoField("nickname", String)
              Define.Field("doesKnowCommand", Boolean, [ Define.Input("dogCommand", Command) ], fun ctx (dog : Dog) -> dog.DoesKnowCommand(ctx.Arg("dogCommand")))
              Define.Field("isHouseTrained", Boolean, [ Define.Input("atOtherHomes", Boolean) ], fun ctx (dog : Dog) -> dog.IsHouseTrained(ctx.Arg("atOtherHomes"))) ], 
        interfaces = [ Pet ])

let Cat = 
    Define.Object<Cat>(
        name = "Cat", 
        fields = 
            [ Define.AutoField("name", String)
              Define.AutoField("meowVolume", Int)
              Define.AutoField("nickname", String)
              Define.Field("doesKnowCommand", Boolean, [ Define.Input("catCommand", Command) ], fun ctx (dog : Cat) -> dog.DoesKnowCommand(ctx.Arg("catCommand")))], 
        interfaces = [ Pet ])

let CatOrDog =
    Define.Union(
        name = "CatOrDog",
        options = [ Cat; Dog ],
        resolveValue = (function | CatOrDog.Cat c -> box c | CatOrDog.Dog d -> upcast d),
        resolveType = (function | CatOrDog.Cat _ -> upcast Cat | CatOrDog.Dog _ -> upcast Dog))

let Human =
    Define.Object<Human>(
        name = "Human",
        fields = [ Define.AutoField("name", String) ],
        interfaces = [ Sentient ])

let Alien =
    Define.Object<Alien>(
        name = "Alien",
        fields = [ Define.AutoField("name", String) ],
        interfaces = [ Sentient ])

let HumanOrAlien =
    Define.Union(
        name = "HumanOrAlien",
        options = [ Human; Alien ],
        resolveValue = (function | HumanOrAlien.Human h -> box h | HumanOrAlien.Alien a -> upcast a),
        resolveType = (function | HumanOrAlien.Human _ -> upcast Human | HumanOrAlien.Alien _ -> upcast Alien))

let DogOrHuman =
    Define.Union(
        name = "DogOrHuman",
        options = [ Dog; Human ],
        resolveValue = (function | DogOrHuman.Human h -> box h | DogOrHuman.Dog d -> upcast d),
        resolveType = (function | DogOrHuman.Human _ -> upcast Human | DogOrHuman.Dog _ -> upcast Dog))

let Arguments =
    Define.Object<obj>(
        name = "Arguments",
        fields =
            [ Define.Field("multipleReqs", Int, [ Define.Input("x", Int); Define.Input("y", Int) ], fun ctx _ -> ctx.Arg("x") + ctx.Arg("y"))
              Define.Field("booleanArgField", Nullable Boolean, [ Define.Input("booleanArg", Nullable Boolean) ], fun ctx _ -> ctx.Arg("booleanArg"))
              Define.Field("floatArgField", Nullable Float, [ Define.Input("floatArg", Nullable Float) ], fun ctx _ -> ctx.Arg("floatArg"))
              Define.Field("intArgField", Nullable Int, [ Define.Input("intArg", Nullable Int) ], fun ctx _ -> ctx.Arg("intArg"))
              Define.Field("nonNullBooleanListField", ListOf Boolean, [ Define.Input("nonNullBooleanListArg", ListOf Boolean) ], fun ctx _ -> ctx.Arg("nonNullBooleanListArg"))
              Define.Field("nonNullBooleanArgField", Boolean, [ Define.Input("nonNullBooleanArg", Boolean) ], fun ctx _ -> ctx.Arg("nonNullBooleanArg"))
              Define.Field("booleanListArgField", Nullable (ListOf (Nullable Boolean)), [ Define.Input("booleanListArg", ListOf (Nullable Boolean)) ], fun ctx _ -> ctx.Arg("booleanListArg") |> Some)
              Define.Field("optionalNonNullBooleanArgField", Boolean, [ Define.Input("optionalBooleanArg", Boolean, false) ], fun ctx _ -> ctx.Arg("optionalBooleanArg")) ])

let Query = 
    Define.Object<Root>(
        name = "Root", 
        fields =
            [ Define.AutoField("catOrDog", CatOrDog)
              Define.AutoField("humanOrAlien", HumanOrAlien)
              Define.AutoField("dogOrHuman", DogOrHuman)
              Define.AutoField("pet", Pet)
              Define.AutoField("human", Human)
              Define.Field("arguments", Arguments)
              Define.Field("findDog", Nullable Dog, [ Define.Input("complex", ComplexInput) ], fun ctx (r : Root) -> r.FindDog(ctx.Arg("complex")))
              Define.Field("booleanList", Nullable Boolean, [ Define.Input("booleanListArg", Nullable (ListOf Boolean)) ], fun ctx (r : Root) -> r.BooleanList(ctx.Arg("booleanListArg"))) ])

let Mutation =
    Define.Object<Root>(
        name = "Mutation",
        fields =
            [ Define.Field("convert", String, [ Define.Input("value", Int) ], fun ctx _ -> ctx.Arg<int>("value").ToString()) ])

let Subscription =
    Define.SubscriptionObject<Root>(
        name = "Subscription",
        fields = [ Define.SubscriptionField("ping", Query, String, filter = fun _ _ _ -> Some "pong") ])

let directives =
    [ { Name = "queryOnly"; Description = None; Locations = DirectiveLocation.QUERY; Args = [||] }
      { Name = "mutationOnly"; Description = None; Locations = DirectiveLocation.MUTATION; Args = [||] }
      { Name = "subscriptionOnly"; Description = None; Locations = DirectiveLocation.SUBSCRIPTION; Args = [||] }
      { Name = "fragSpreadOnly"; Description = None; Locations = DirectiveLocation.FRAGMENT_SPREAD; Args = [||] }
      { Name = "inlineFragOnly"; Description = None; Locations = DirectiveLocation.INLINE_FRAGMENT; Args = [||] }
      { Name = "fieldOnly"; Description = None; Locations = DirectiveLocation.FIELD; Args = [||] } ]
    |> List.append SchemaConfig.Default.Directives

let schemaConfig =
    { SchemaConfig.Default with Directives = directives }

let schema : ISchema = upcast Schema(Query, Mutation, Subscription, schemaConfig)

let schemaInfo = Validation.Ast.SchemaInfo.FromIntrospectionSchema(schema.Introspected)

let getContext = Parser.parse >> Validation.Ast.getValidationContext schemaInfo

[<Fact>]
let ``Validation should grant that each operation name is unique in the document`` () =
    let query =
        """query dogOperation {
  dog {
    name
  }
}

mutation dogOperation {
  mutateDog {
    id
  }
}"""
    let actual = getContext query |> validateOperationNameUniqueness
    let expected = ValidationError [ {Message = "Operation 'dogOperation' has 2 definitions. Each operation name must be unique."; Path = None; } ]
    actual |> equals expected

[<Fact>]
let ``Validation should grant that, if the document has anonymous operations, all of its operations should be a set of one`` () =
    let query =
        """{
  dog {
    name
  }
}

query getName {
  dog {
    owner {
      name
    }
  }
}"""
    let actual = getContext query |> validateLoneAnonymousOperation
    let expected = ValidationError [ { Message = "An anonymous operation must be the only operation in a document. This document has at least one anonymous operation and more than one operation."
                                       Path = None } ]
    actual |> equals expected

[<Fact>]
let ``Validation should grant that subscription operations contains only one root field`` () =
    let query1 =
        """subscription sub1 {
  newMessage {
    body
    sender
  }
  disallowedSecondRootField
}"""
    let query2 =
        """subscription sub2 {
  ...multipleSubscriptions
}

fragment multipleSubscriptions on Subscription {
  newMessage {
    body
    sender
  }
  disallowedSecondRootField
}"""
    let query3 =
        """subscription sub3 {
  newMessage {
    body
    sender
  }
  __typename
}"""
    let expected =
        ValidationError [ { Message = "Subscription operations should have only one root field. Operation 'sub1' has 2 fields (disallowedSecondRootField, newMessage)."; Path = None }
                          { Message = "Subscription operations should have only one root field. Operation 'sub2' has 2 fields (disallowedSecondRootField, newMessage)."; Path = None }
                          { Message = "Subscription operations should have only one root field. Operation 'sub3' has 2 fields (__typename, newMessage)."; Path = None }]
    let actual = [query1; query2; query3] |> collectResults (getContext >> Validation.Ast.validateSubscriptionSingleRootField)
    actual |> equals expected

[<Fact>]
let ``Validation should grant that selections contains only fields of their scoped types`` () =
    let query1 =
        """fragment fieldNotDefined on Dog {
  meowVolume
}

fragment aliasedLyingFieldTargetNotDefined on Dog {
  barkVolume: kawVolume
}"""
    let query2 =
        """fragment definedOnImplementorsButNotInterface on Pet {
  nickname
}"""
    let query3 =
        """fragment directFieldSelectionOnUnion on CatOrDog {
  name
  barkVolume
}"""
    let expected =
        ValidationError [ { Message = "Field 'meowVolume' is not defined in schema type 'Dog'."; Path = Some ["fieldNotDefined"; "meowVolume"] }
                          { Message = "Field 'kawVolume' is not defined in schema type 'Dog'."; Path = Some ["aliasedLyingFieldTargetNotDefined"; "barkVolume"] }
                          { Message = "Field 'nickname' is not defined in schema type 'Pet'."; Path = Some ["definedOnImplementorsButNotInterface"; "nickname"] }
                          { Message = "Field 'name' is not defined in schema type 'CatOrDog'."; Path = Some ["directFieldSelectionOnUnion"; "name"] }
                          { Message = "Field 'barkVolume' is not defined in schema type 'CatOrDog'."; Path = Some ["directFieldSelectionOnUnion"; "barkVolume"] } ]
    let actual = [query1; query2; query3] |> collectResults (getContext >> Validation.Ast.validateSelectionFieldTypes)
    actual |> equals expected

[<Fact>]
let ``Validation should grant that fields in any selection set can be merged`` () =
    let query1 =
        """fragment conflictingBecauseAlias on Dog {
  name: nickname
  name
}"""
    let query2 =
        """fragment conflictingArgsOnValues on Dog {
  doesKnowCommand(dogCommand: SIT)
  doesKnowCommand(dogCommand: HEEL)
}

fragment conflictingArgsValueAndVar on Dog {
  doesKnowCommand(dogCommand: SIT)
  doesKnowCommand(dogCommand: $dogCommand)
}

fragment conflictingArgsWithVars on Dog {
  doesKnowCommand(dogCommand: $varOne)
  doesKnowCommand(dogCommand: $varTwo)
}

fragment differingArgs on Dog {
  doesKnowCommand(dogCommand: SIT)
  doesKnowCommand
}"""
    let query3 =
        """fragment conflictingDifferingResponses on Pet {
  ... on Dog {
    someValue: nickname
  }
  ... on Cat {
    someValue: meowVolume
  }
}"""
    let expectedFailureResult = 
        ValidationError [ { Message = "Field name or alias 'name' is referring to fields 'nickname' and 'name', but they are different fields in the scope of the parent type."
                            Path = Some ["conflictingBecauseAlias"; "name"] }
                          { Message = "Field name or alias 'doesKnowCommand' refers to field 'doesKnowCommand' two times, but each reference has different argument sets."
                            Path = Some ["conflictingArgsOnValues"; "doesKnowCommand"] }
                          { Message = "Field name or alias 'doesKnowCommand' refers to field 'doesKnowCommand' two times, but each reference has different argument sets."
                            Path = Some ["conflictingArgsValueAndVar"; "doesKnowCommand"] }
                          { Message = "Field name or alias 'doesKnowCommand' refers to field 'doesKnowCommand' two times, but each reference has different argument sets."
                            Path = Some ["conflictingArgsWithVars"; "doesKnowCommand"] }
                          { Message = "Field name or alias 'doesKnowCommand' refers to field 'doesKnowCommand' two times, but each reference has different argument sets."
                            Path = Some ["differingArgs"; "doesKnowCommand"] }
                          { Message = "Field name or alias 'someValue' appears two times, but they do not have the same return types in the scope of the parent type."
                            Path = Some ["conflictingDifferingResponses"; "someValue"] } ]
    let shouldFail = [query1; query2; query3] |> collectResults (getContext >> Validation.Ast.validateFieldSelectionMerging)
    shouldFail |> equals expectedFailureResult
    let query4 =
        """fragment mergeIdenticalFields on Dog {
  name
  name
}

fragment mergeIdenticalAliasesAndFields on Dog {
  otherName: name
  otherName: name
}"""
    let query5 =
        """fragment mergeIdenticalFieldsWithIdenticalArgs on Dog {
  doesKnowCommand(dogCommand: SIT)
  doesKnowCommand(dogCommand: SIT)
}

fragment mergeIdenticalFieldsWithIdenticalValues on Dog {
  doesKnowCommand(dogCommand: $dogCommand)
  doesKnowCommand(dogCommand: $dogCommand)
}"""
    let query6 =
        """fragment safeDifferingFields on Pet {
  ... on Dog {
    volume: barkVolume
  }
  ... on Cat {
    volume: meowVolume
  }
}

fragment safeDifferingArgs on Pet {
  ... on Dog {
    doesKnowCommand(dogCommand: SIT)
  }
  ... on Cat {
    doesKnowCommand(catCommand: JUMP)
  }
}"""
    let shouldPass = [query4; query5; query6] |> collectResults (getContext >> Validation.Ast.validateFieldSelectionMerging)
    shouldPass |> equals Success
    
[<Fact>]
let ``Validation should grant that leaf fields have sub fields when necessary, and do not have if they are scalar or enums`` () =
    let query1 =
        """fragment scalarSelectionsNotAllowedOnInt on Dog {
  barkVolume {
    sinceWhen
  }
}"""
    let query2 =
        """query directQueryOnObjectWithoutSubFields {
  human
}

query directQueryOnInterfaceWithoutSubFields {
  pet
}

query directQueryOnUnionWithoutSubFields {
  catOrDog
}"""
    let expectedFailureResult = 
        ValidationError [ { Message = "Field 'barkVolume' of 'Dog' type is of type kind SCALAR, and therefore should not contain inner fields in its selection."
                            Path = Some ["scalarSelectionsNotAllowedOnInt"; "barkVolume"] }
                          { Message = "Field 'human' of 'Root' type is of type kind OBJECT, and therefore should have inner fields in its selection."
                            Path = Some ["directQueryOnObjectWithoutSubFields"; "human"] }
                          { Message = "Field 'pet' of 'Root' type is of type kind INTERFACE, and therefore should have inner fields in its selection."
                            Path = Some ["directQueryOnInterfaceWithoutSubFields"; "pet"] }
                          { Message = "Field 'catOrDog' of 'Root' type is of type kind UNION, and therefore should have inner fields in its selection."
                            Path = Some ["directQueryOnUnionWithoutSubFields"; "catOrDog"] } ]
    let shouldFail = [query1; query2] |> collectResults (getContext >> Validation.Ast.validateLeafFieldSelections)
    shouldFail |> equals expectedFailureResult
    let query4 =
        """fragment scalarSelection on Dog {
  barkVolume
}"""
    let shouldPass = getContext query4 |> Validation.Ast.validateLeafFieldSelections
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that arguments passed to fields exists in their definition`` () =
    let query1 =
        """fragment invalidArgName on Dog {
  doesKnowCommand(command: CLEAN_UP_HOUSE)
}"""
    let query2 =
        """fragment invalidArgName on Dog {
  isHouseTrained(atOtherHomes: true) @include(unless: false) @skip (jaca: false)
}"""
    let expectedFailureResult =
        ValidationError [ { Message = "Field 'doesKnowCommand' of type 'Dog' does not have an input named 'command' in its definition."
                            Path = Some ["invalidArgName"; "doesKnowCommand"] }
                          { Message = "Directive 'include' of field 'isHouseTrained' of type 'Dog' does not have an argument named 'unless' in its definition."
                            Path = Some ["invalidArgName"; "isHouseTrained"] }
                          { Message = "Directive 'skip' of field 'isHouseTrained' of type 'Dog' does not have an argument named 'jaca' in its definition."
                            Path = Some ["invalidArgName"; "isHouseTrained"] } ]
    let shouldFail = [query1; query2] |> collectResults (getContext >> Validation.Ast.validateArgumentNames)
    shouldFail |> equals expectedFailureResult
    let query3 =
        """fragment argOnRequiredArg on Dog {
  doesKnowCommand(dogCommand: SIT)
}

fragment argOnOptional on Dog {
  isHouseTrained(atOtherHomes: true) @include(if: true)
}"""
    let shouldPass = getContext query3 |> Validation.Ast.validateArgumentNames
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that arguments passed to fields and directives are unique between themselves`` () =
    let query1 =
        """fragment duplicatedArgs on Dog {
  doesKnowCommand(dogCommand: SIT, dogCommand: CLEAN_UP_HOUSE)
}"""
    let query2 =
        """fragment invalidArgName on Dog {
        isHousetrained(atOtherHomes: true) @include(if : true, if: false)
}"""  
    let expectedFailureResult =
        ValidationError [ { Message = "There are 2 arguments with name 'dogCommand' defined in alias or field 'doesKnowCommand'. Field arguments must be unique."
                            Path = Some ["duplicatedArgs"; "doesKnowCommand"] }
                          { Message = "There are 2 arguments with name 'if' defined in directive 'include'. Field arguments must be unique."
                            Path = Some ["invalidArgName"; "isHousetrained"] } ]
    let shouldFail = [query1; query2] |> List.map(getContext >> validateArgumentUniqueness) |> List.reduce (@@)
    shouldFail |> equals expectedFailureResult

[<Fact>]
let ``Validation should grant that required arguments with no default values are passed`` () =
    let query1 =
        """fragment missingRequiredArg on Arguments {
  nonNullBooleanArgField
}"""
    let query2 =
        """fragment missingRequiredArg on Arguments {
  nonNullBooleanArgField(nonNullBooleanArg: null)
}"""
    let expectedFailureResult =
        ValidationError [ { Message = "Argument 'nonNullBooleanArg' of field 'nonNullBooleanArgField' of type 'Arguments' is required and does not have a default value."
                            Path = Some ["missingRequiredArg"; "nonNullBooleanArgField"] }
                          { Message = "Argument 'nonNullBooleanArg' of field 'nonNullBooleanArgField' of type 'Arguments' is required and does not have a default value."
                            Path = Some ["missingRequiredArg"; "nonNullBooleanArgField"] } ]
    let shouldFail = [query1; query2] |> collectResults (getContext >> Validation.Ast.validateRequiredArguments)
    shouldFail |> equals expectedFailureResult
    let query3 =
        """fragment goodBooleanArg on Arguments {
  booleanArgField(booleanArg: true)
}

fragment goodNonNullArg on Arguments {
  nonNullBooleanArgField(nonNullBooleanArg: true)
}"""
    let query4 =
        """fragment goodBooleanArgDefault on Arguments {
  booleanArgField
}"""
    let shouldPass = [query3; query4] |> collectResults (getContext >> Validation.Ast.validateRequiredArguments)
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that fragment definitions have unique names`` () =
    let query1 =
        """{
  dog {
    ...fragmentOne
  }
}

fragment fragmentOne on Dog {
  name
}

fragment fragmentOne on Dog {
  owner {
    name
  }
}"""
    let shouldFail = getContext query1 |> Validation.Ast.validateFragmentNameUniqueness
    shouldFail |> equals (ValidationError [ {Message = "There are 2 fragments with name 'fragmentOne' in the document. Fragment definitions must have unique names."; Path = None } ])
    let query2 =
        """fragment correctType on Dog {
  name
}

fragment inlineFragment on Dog {
  ... on Dog {
    name
  }
}

fragment inlineFragment2 on Dog {
  ... @include(if: true) {
    name
  }
}"""
    let shouldPass = getContext query2 |> Validation.Ast.validateFragmentNameUniqueness
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that type conditions of fragments refers to existing types in the schema`` () =
    let query =
        """fragment notOnExistingType on NotInSchema {
  name
}

fragment inlineNotExistingType on Dog {
  ... on NotInSchema {
    name
  }
}"""
    let expectedFailureResult =
        ValidationError [ { Message = "Fragment 'notOnExistingType' has type condition 'NotInSchema', but that type does not exist in the schema."; Path = None }
                          { Message = "Inline fragment has type condition 'NotInSchema', but that type does not exist in the schema."; Path = Some ["inlineNotExistingType"] } ]
    let shouldFail = getContext query |> validateFragmentTypeExistence
    shouldFail |> equals expectedFailureResult

[<Fact>]
let ``Validation should grant that type fragments are only used in composite types`` () =
    let query =
        """fragment fragOnScalar on Int {
  something
}

fragment inlineFragOnScalar on Dog {
  ... on Boolean {
    somethingElse
  }
}"""
    let expectedFailureResult =
        ValidationError [ {Message = "Fragment 'fragOnScalar' has type kind SCALAR, but fragments can only be defined in UNION, OBJECT or INTERFACE types."; Path = Some ["fragOnScalar"; "something"] }
                          { Message = "Inline fragment has type kind SCALAR, but fragments can only be defined in UNION, OBJECT or INTERFACE types."; Path = Some ["inlineFragOnScalar"; "somethingElse"] } ]
    let shouldFail = getContext query |> validateFragmentsOnCompositeTypes
    shouldFail |> equals expectedFailureResult

[<Fact>]
let ``Validation should grant that fragment definitions are used in at least one operation`` () =
    let query =
        """fragment nameFragment on Dog {
  name
}

{
  dog {
    name
  }
}"""
    let shouldFail = getContext query |> Validation.Ast.validateFragmentsMustBeUsed
    shouldFail |> equals (ValidationError [ {Message = "Fragment 'nameFragment' is not used in any operation in the document. Fragments must be used in at least one operation."; Path = None } ])

[<Fact>]
let ``Validation should grant that fragment spreads exists in document`` () =
    let query =
        """{
  dog {
    ...undefinedFragment
  }
}"""
    let shouldFail = getContext query |> Validation.Ast.validateFragmentSpreadTargetDefined
    shouldFail |> equals (ValidationError [ {Message = "Fragment spread 'undefinedFragment' refers to a non-existent fragment definition in the document."; Path = Some ["dog"] } ])

[<Fact>]
let ``Validation should grant that fragment definitions dont refer themselves`` () =
    let query1 =
        """{
  dog {
    ...nameFragment
  }
}

fragment nameFragment on Dog {
  name
  ...barkVolumeFragment
}

fragment barkVolumeFragment on Dog {
barkVolume
...nameFragment
}"""
    let query2 =
        """{
  dog {
    ...dogFragment
  }
}

fragment dogFragment on Dog {
  name
  owner {
    ...ownerFragment
  }
}

fragment ownerFragment on Dog {
  name
  pets {
    ...dogFragment
  }
}"""
    let expectedFailureResult =
        ValidationError [ { Message = "Fragment 'nameFragment' is making a cyclic reference."; Path = None }
                          { Message = "Fragment 'barkVolumeFragment' is making a cyclic reference."; Path = None }
                          { Message = "Fragment 'dogFragment' is making a cyclic reference."; Path = None }
                          { Message = "Fragment 'ownerFragment' is making a cyclic reference."; Path = None } ]
    let shouldFail = [query1;query2] |> collectResults (getContext >> Validation.Ast.validateFragmentsMustNotFormCycles)
    shouldFail |> equals expectedFailureResult

[<Fact>]
let ``Validation should grant that each fragment spread is possible`` () =
    let query1 =
        """fragment catInDogFragmentInvalid on Dog {
  ... on Cat {
    meowVolume
  }
}"""
    let query2 =
        """fragment sentientFragment on Sentient {
  ... on Dog {
    barkVolume
  }
}

fragment humanOrAlienFragment on HumanOrAlien {
  ... on Cat {
    meowVolume
  }
}"""
    let query3 =
        """fragment nonIntersectingInterfaces on Pet {
  ...sentientFragment
}

fragment sentientFragment on Sentient {
  name
}"""
    let query4 =
        """fragment sentientFragment on Sentient {
  ... on Dog {
    barkVolume
  }
}

fragment humanOrAlienFragment on HumanOrAlien {
  ... on Cat {
    meowVolume
  }
}"""
    let expectedFailureResult =
        ValidationError [ { Message = "Fragment type condition 'Cat' is not applicable to the parent type of the field 'Dog'."
                            Path = Some ["catInDogFragmentInvalid"; "meowVolume"] }
                          { Message = "Fragment type condition 'Dog' is not applicable to the parent type of the field 'Sentient'."
                            Path = Some ["sentientFragment"; "barkVolume"] }
                          { Message = "Fragment type condition 'Cat' is not applicable to the parent type of the field 'HumanOrAlien'."
                            Path = Some ["humanOrAlienFragment"; "meowVolume"] }
                          { Message = "Fragment type condition 'Sentient' is not applicable to the parent type of the field 'Pet'."
                            Path = Some ["nonIntersectingInterfaces"; "name"] }
                          { Message = "Fragment type condition 'Dog' is not applicable to the parent type of the field 'Sentient'."
                            Path = Some ["sentientFragment"; "barkVolume"] }
                          { Message = "Fragment type condition 'Cat' is not applicable to the parent type of the field 'HumanOrAlien'."
                            Path = Some ["humanOrAlienFragment"; "meowVolume"] } ]
    let shouldFail = [query1;query2;query3;query4] |> collectResults (getContext >> Validation.Ast.validateFragmentSpreadIsPossible)
    shouldFail |> equals expectedFailureResult
    let query5 =
        """fragment dogFragment on Dog {
  ... on Dog {
    barkVolume
  }
}"""
    let query6 =
        """fragment petNameFragment on Pet {
  name
}

fragment interfaceWithinObjectFragment on Dog {
  ...petNameFragment
}"""
    let query7 =
        """fragment catOrDogNameFragment on CatOrDog {
  ... on Cat {
    meowVolume
  }
}

fragment unionWithObjectFragment on Dog {
  ...catOrDogNameFragment
}"""
    let query8 =
        """fragment petFragment on Pet {
  name
  ... on Dog {
    barkVolume
  }
}

fragment catOrDogFragment on CatOrDog {
  ... on Cat {
    meowVolume
  }
}"""
    let query9 =
        """fragment unionWithInterface on Pet {
  ...dogOrHumanFragment
}

fragment dogOrHumanFragment on DogOrHuman {
  ... on Dog {
    barkVolume
  }
}"""
    let shouldPass = [query5;query6;query7;query8;query9] |> collectResults (getContext >> Validation.Ast.validateFragmentSpreadIsPossible)
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that each input have a valid value`` () =
    let query1 =
        """fragment stringIntoInt on Arguments {
  intArgField(intArg: "123")
}

query badComplexValue {
  findDog(complex: { name: 123 })
}

fragment nullRequiredBooleanArg on Arguments {
  nonNullBooleanArgField(nonNullBooleanArg: null)
}"""
    let query2 =
        """{
  findDog(complex: { favoriteCookieFlavor: "Bacon" })
}"""
    let expectedFailureResult =
       ValidationError [ { Message = "Argument field or value named 'intArg' can not be coerced. It does not match a valid literal representation for the type."; Path = Some ["stringIntoInt"; "intArgField"] }
                         { Message = "Argument 'nonNullBooleanArg' value can not be coerced. It's type is non-nullable but the argument has a null value."; Path = Some ["nullRequiredBooleanArg"; "nonNullBooleanArgField"] }
                         { Message = "Argument field or value named 'name' can not be coerced. It does not match a valid literal representation for the type."; Path = Some ["badComplexValue"; "findDog"] }
                         { Message = "Can not coerce argument 'complex'. The field 'favoriteCookieFlavor' is not a valid field in the argument definition."; Path = Some ["findDog"] } ]
    let shouldFail = [query1; query2] |> collectResults (getContext >> Validation.Ast.validateInputValues)
    shouldFail |> equals expectedFailureResult
    let query2 =
        """fragment goodBooleanArg on Arguments {
  booleanArgField(booleanArg: true)
}

fragment coercedIntIntoFloatArg on Arguments {
  # Note: The input coercion rules for Float allow Int literals.
  floatArgField(floatArg: 123)
}

query goodComplexDefaultValue($search: ComplexInput = { name: "Fido" }) {
  findDog(complex: $search)
}"""
    let query3 =
        """fragment stringIntoInt on Arguments {
  intArgField(intArg: null)
}"""
    let query4 = """fragment validEmptyList on Arguments {
  booleanListArgField(booleanListArg: [])
}
fragment validList on Arguments {
  booleanListArgField(booleanListArg: [false, null, true])
}"""
    let shouldPass = [query2;query3; query4] |> collectResults (getContext >> Validation.Ast.validateInputValues)
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that directives are supported by the schema`` () =
    let query1 = """query dogOperation {
dog @unknownDirective {
  name
}
}"""
    let shouldFail = getContext query1 |> Validation.Ast.validateDirectivesDefined
    shouldFail |> equals (ValidationError [ {Message = "Directive 'unknownDirective' is not defined in the schema."; Path = Some ["dogOperation"; "dog"] } ])
    let query2 = """query dogOperation {
dog @include(if: true) {
  name
}
}"""
    let shouldPass = getContext query2 |> Validation.Ast.validateDirectivesDefined
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that directives are in valid locations`` () =
    let query1 =
        """query myQuery @skip(if: $foo) {
  field @queryOnly
}"""
    let query2 =
        """mutation myMutation @skip(if: $foo) {
  convert(value: 2)
}"""
    let query3 =
        """subscription mySubscription @skip(if: $foo) {
  ping
}"""
    let query4 =
        """fragment HouseTrainedFragment on Root @skip(if: $foo) {
  dog {
    isHousetrained(atOtherHomes: $atOtherHomes)
  }
}"""
    let query5 =
        """fragment safeDifferingFields on Pet {
  ... on Dog @queryOnly {
    volume: barkVolume
  }
  ... on Cat @mutationOnly {
    volume: meowVolume
  }
}"""
    let expectedFailureResult =
        ValidationError[ { Message = "Query operation 'myQuery' has a directive 'skip', but this directive location is not supported by the schema definition."
                           Path = Some ["myQuery"] }
                         { Message = "Field or alias 'field' has a directive 'queryOnly', but this directive location is not supported by the schema definition."
                           Path = Some ["myQuery"; "field"] }
                         { Message = "Mutation operation 'myMutation' has a directive 'skip', but this directive location is not supported by the schema definition."
                           Path = Some ["myMutation"] }
                         { Message = "Subscription operation 'mySubscription' has a directive 'skip', but this directive location is not supported by the schema definition."
                           Path = Some ["mySubscription"] }
                         { Message = "An inline fragment has a directive 'queryOnly', but this directive location is not supported by the schema definition."
                           Path = Some ["safeDifferingFields"] }
                         { Message = "An inline fragment has a directive 'mutationOnly', but this directive location is not supported by the schema definition."
                           Path = Some ["safeDifferingFields"] } ]
    let shouldFail = [query1;query2;query3;query4;query5] |> List.map (getContext >> Validation.Ast.validateDirectivesAreInValidLocations) |> List.reduce (@@)
    shouldFail |> equals expectedFailureResult

[<Fact>]
let ``Validation should grant that directives are unique in their locations`` () =
    let query1 =
        """query q($foo: Boolean = true, $bar: Boolean = false) {
  field @skip(if: $foo) @skip(if: $bar)
}"""
    let expectedFailureResult =
        ValidationError [ { Message = "Directive 'skip' appears 2 times in the location it is used. Directives must be unique in their locations."
                            Path = Some ["q"; "field"] } ]
    let shouldFail = query1 |> getContext |> Validation.Ast.validateUniqueDirectivesPerLocation
    shouldFail |> equals expectedFailureResult
    let query2 =
        """query q($foo: Boolean = true, $bar: Boolean = false) {
  field @skip(if: $foo) {
    subfieldA
  }
  field @skip(if: $bar) {
    subfieldB
  }
}"""
    let shouldPass = getContext query2 |> Validation.Ast.validateUniqueDirectivesPerLocation
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that variables are unique in their operations`` () =
    let query1 =
        """query houseTrainedQuery($atOtherHomes: Boolean, $atOtherHomes: Boolean) {
  dog {
    isHousetrained(atOtherHomes: $atOtherHomes)
  }
}"""
    let shouldFail = getContext query1 |> Validation.Ast.validateVariableUniqueness
    shouldFail |> equals (ValidationError [ { Message = "Variable 'atOtherHomes' in operation 'houseTrainedQuery' is declared 2 times. Variables must be unique in their operations."; Path = None } ])
    let query2 =
        """query A($atOtherHomes: Boolean) {
  ...HouseTrainedFragment
}

query B($atOtherHomes: Boolean) {
  ...HouseTrainedFragment
}

fragment HouseTrainedFragment on Root {
  dog {
    isHousetrained(atOtherHomes: $atOtherHomes)
  }
}"""
    let shouldPass = getContext query2 |> Validation.Ast.validateUniqueDirectivesPerLocation
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that variables are input types`` () =
    let query1 =
        """query takesCat($cat: Cat) {
  name
}

query takesDogBang($dog: Dog!) {
  name
}

query takesListOfPet($pets: [Pet]) {
  name
}

query takesCatOrDog($catOrDog: CatOrDog) {
  name
}"""
    let expectedFailureResult =
        ValidationError [ { Message = "Variable 'cat' in operation 'takesCat' has a type that is not an input type defined by the schema (Cat)."; Path = None }
                          { Message = "Variable 'dog' in operation 'takesDogBang' has a type that is not an input type defined by the schema (Dog!)."; Path = None }
                          { Message = "Variable 'pets' in operation 'takesListOfPet' has a type that is not an input type defined by the schema ([Pet])."; Path = None }
                          { Message = "Variable 'catOrDog' in operation 'takesCatOrDog' has a type that is not an input type defined by the schema (CatOrDog)."; Path = None } ]
    let shouldFail = getContext query1 |> Validation.Ast.validateVariablesAsInputTypes
    shouldFail |> equals expectedFailureResult
    let query2 =
        """query takesBoolean($atOtherHomes: Boolean) {
  dog {
    isHousetrained(atOtherHomes: $atOtherHomes)
  }
}

query takesComplexInput($complexInput: ComplexInput) {
  findDog(complex: $complexInput) {
    name
  }
}

query TakesListOfBooleanBang($booleans: [Boolean!]) {
  booleanList(booleanListArg: $booleans)
}"""
    let shouldPass = getContext query2 |> Validation.Ast.validateVariablesAsInputTypes
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that all referenced variables are defined variables`` () =
    let query1 = 
        """query variableIsNotDefined {
  dog {
    isHousetrained(atOtherHomes: $atOtherHomes)
  }
}"""
    let shouldFail = getContext query1 |> Validation.Ast.validateVariablesUsesDefined
    shouldFail |> equals (ValidationError [ { Message = "A variable 'atOtherHomes' is referenced in argument 'atOtherHomes' of field with alias or name 'isHousetrained', but that variable is not defined in the operation."; Path = None } ])
    let query2 =
        """query variableIsDefined($atOtherHomes: Boolean) {
  dog {
    isHousetrained(atOtherHomes: $atOtherHomes)
  }
}"""
    let shouldPass = getContext query2 |> Validation.Ast.validateVariablesUsesDefined
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that all defined variables are used in the operation they were defined`` () =
    let query1 =
        """query variableUnused($atOtherHomes: Boolean) {
  dog {
    isHousetrained
  }
}"""
    let query2 =
        """query variableNotUsedWithinFragment($atOtherHomes: Boolean) {
  dog {
    ...isHousetrainedWithoutVariableFragment
  }
}

fragment isHousetrainedWithoutVariableFragment on Dog {
  isHousetrained
}"""
    let query3 =
        """query queryWithUsedVar($atOtherHomes: Boolean) {
  dog {
    ...isHousetrainedFragment
  }
}

query queryWithExtraVar($atOtherHomes: Boolean, $extra: Int) {
  dog {
    ...isHousetrainedFragment
  }
}

fragment isHousetrainedFragment on Dog {
  isHousetrained(atOtherHomes: $atOtherHomes)
}"""
    let query4 =
        """query unusedCyclic($extra: Int) {
  dog {
      ...isHousetrainedFragment
  }
}

fragment isHousetrainedFragment on Dog {
  ...isHouseTrainedCyclic
}

fragment isHouseTrainedCyclic on Dog {
  ...isHousetrainedFragment
}""" 
    let expectedFailureResult =
        ValidationError [ { Message = "Variable definition 'atOtherHomes' is not used in operation 'variableUnused'. Every variable must be used."; Path = None }
                          { Message = "Variable definition 'atOtherHomes' is not used in operation 'variableNotUsedWithinFragment'. Every variable must be used."; Path = None }
                          { Message = "Variable definition 'extra' is not used in operation 'queryWithExtraVar'. Every variable must be used."; Path = None }
                          { Message = "Variable definition 'extra' is not used in operation 'unusedCyclic'. Every variable must be used."; Path = None } ]

    [query1; query2; query3; query4]
    |> collectResults (getContext >> Validation.Ast.validateAllVariablesUsed)
    |> equals expectedFailureResult

    let query4 =
        """query variableUsedInFragment($atOtherHomes: Boolean) {
  dog {
    ...isHousetrainedFragment
  }
}

fragment isHousetrainedFragment on Dog {
  isHousetrained(atOtherHomes: $atOtherHomes)
}"""
    let query5 =
        """
query nestedVariables($name: String) {
    findDog(complex : { name : $name, owner : "Bob" }) {
        name
    }
}
"""

    [query4; query5]
    |> collectResults (getContext >> Validation.Ast.validateAllVariablesUsed)
    |> equals Success

[<Fact>]
let ``Validation should grant that all variables can be used`` () =
    let query1 =
        """query intCannotGoIntoBoolean($intArg: Int) {
        arguments {
          booleanArgField(booleanArg: $intArg)
        }
}

query booleanListCannotGoIntoBoolean($booleanListArg: [Boolean]) {
        arguments {
          booleanArgField(booleanArg: $booleanListArg)
        }
}

query booleanArgQuery($booleanArg: Boolean) {
        arguments {
          nonNullBooleanArgField(nonNullBooleanArg: $booleanArg)
        }
}

query listToNonNullList($booleanList: [Boolean]) {
        arguments {
          nonNullBooleanListField(nonNullBooleanListArg: $booleanList)
        }
}"""
    let expectedFailureResult =
        ValidationError [ { Message = "Variable 'intArg' can not be used in its reference. The type of the variable definition is not compatible with the type of its reference."
                            Path = Some ["intCannotGoIntoBoolean"; "arguments"; "booleanArgField"] }
                          { Message = "Variable 'booleanListArg' can not be used in its reference. The type of the variable definition is not compatible with the type of its reference."
                            Path = Some ["booleanListCannotGoIntoBoolean"; "arguments"; "booleanArgField"] }
                          { Message = "Variable 'booleanArg' can not be used in its reference. The type of the variable definition is not compatible with the type of its reference."
                            Path = Some ["booleanArgQuery"; "arguments"; "nonNullBooleanArgField"] }
                          { Message = "Variable 'booleanList' can not be used in its reference. The type of the variable definition is not compatible with the type of its reference."
                            Path = Some ["listToNonNullList"; "arguments"; "nonNullBooleanListField"] } ]
    let shouldFail = getContext query1 |> Validation.Ast.validateVariableUsagesAllowed
    shouldFail |> equals expectedFailureResult
    let query2 =
        """query nonNullListToList($nonNullBooleanList: [Boolean]!) {
  arguments {
    booleanListArgField(booleanListArg: $nonNullBooleanList)
  }
}

query booleanArgQueryWithDefault($booleanArg: Boolean) {
  arguments {
    optionalNonNullBooleanArgField(optionalBooleanArg: $booleanArg)
  }
}

query booleanArgQueryWithDefault($booleanArg: Boolean = true) {
  arguments {
    nonNullBooleanArgField(nonNullBooleanArg: $booleanArg)
  }
}"""
    let shouldPass = getContext query2 |> Validation.Ast.validateVariableUsagesAllowed
    shouldPass |> equals Success

[<Fact>]
let ``Validation be able to run all validations in a cyclyc reference document`` () =
    let query1 =
        """{
  dog {
    ...nameFragment
  }
}

fragment nameFragment on Dog {
  name
  ...barkVolumeFragment
}

fragment barkVolumeFragment on Dog {
  barkVolume
  ...nameFragment
}"""
    let query2 =
        """{
  dog {
    ...dogFragment
  }
}

fragment dogFragment on Dog {
  name
  owner {
    ...ownerFragment
  }
}

fragment ownerFragment on Dog {
  name
  pets {
    ...dogFragment
  }
}"""
    let expectedFailureResult =
        ValidationError [ { Message = "Fragment 'nameFragment' is making a cyclic reference."
                            Path = None }
                          { Message = "Fragment 'barkVolumeFragment' is making a cyclic reference."
                            Path = None }
                          { Message = "Field 'dog' is not defined in schema type 'Root'."
                            Path = Some ["dog"] }
                          { Message = "Fragment 'dogFragment' is making a cyclic reference."
                            Path = None }
                          { Message = "Fragment 'ownerFragment' is making a cyclic reference."
                            Path = None }
                          { Message = "Field 'owner' is not defined in schema type 'Dog'."
                            Path = Some ["dogFragment"; "owner"] }
                          { Message = "Field 'pets' is not defined in schema type 'Dog'."
                            Path = Some ["ownerFragment"; "pets"] }
                          { Message = "Field 'dog' is not defined in schema type 'Root'."
                            Path = Some ["dog"] } ]
    let shouldFail = [query1; query2] |> collectResults(Parser.parse >> Validation.Ast.validateDocument schema.Introspected)
    shouldFail |> equals expectedFailureResult

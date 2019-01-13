# FSharp.Data.GraphQL

F# implementation of Facebook [GraphQL query language specification](https://facebook.github.io/graphql).

[![Build Status](https://travis-ci.org/fsprojects/FSharp.Data.GraphQL.svg?branch=dev)](https://travis-ci.org/fsprojects/FSharp.Data.GraphQL)
[![Build status](https://ci.appveyor.com/api/projects/status/qnra6sshi2ywrbig/branch/dev?svg=true)](https://ci.appveyor.com/project/bazingatechnologies/fsharp-data-graphql-tx31m/branch/dev)

## Quick start

```fsharp
type Person =
    { FirstName: string
      LastName: string }

// Define GraphQL type 
let PersonType = Define.Object(
    name = "Person",
    fields = [
        // Property resolver will be auto-generated
        Define.AutoField("firstName", String)
        // Asynchronous explicit member resolver
        Define.AsyncField("lastName", String, resolve = fun context person -> async { return person.LastName })
    ])

// Include person as a root query of a schema
let schema = Schema(query = PersonType)
// Create an Exector for the schema
let executor = Executor(schema)

// Retrieve person data
let johnSnow = { FirstName = "John"; LastName = "Snow" }
let reply = executor.AsyncExecute(parse "{ firstName, lastName }", johnSnow) |> Async.RunSynchronously
// #> { data: { "firstName", "John", "lastName", "Snow" } } 
```

It's type safe. Things like invalid fields or invalid return types will be checked at compile time.

## Demos

### GraphiQL client

Go to [GraphiQL sample directory](https://github.com/bazingatechnologies/FSharp.Data.GraphQL/tree/dev/samples/graphiql-client). In order to run it, build and run `FSharp.Data.GraphQL.Samples.GiraffeServer` project on Debug settings - this will create a Giraffe server compatible with GraphQL spec, running on port 8084. Then what you need is to run node.js graphiql frontend. To do so, run `npm i` to get all dependencies, and then run `npm run serve | npm run dev` - this will start a webpack server running on [http://localhost:8090/](http://localhost:8090/) . Visit this link, and GraphiQL editor should appear. You may try it by applying following query:

```graphql
{
  hero(id:"1000") {
    id,
    name,
    appearsIn,
    homePlanet,
    friends {
      ... on Human {
        name
      }
      ... on Droid {
        name
      }
    }
  }
}
```

### Relay.js starter kit

A [second sample](https://github.com/bazingatechnologies/FSharp.Data.GraphQL/tree/dev/samples/relay-starter-kit) is a F#-backed version of of popular Relay Starter Kit - an example application using React.js + Relay with Relay-compatible server API.

To run it, build `FSharp.Data.GraphQL` and `FSharp.Data.GraphQL.Relay` projects using Debug settings. Then start server by running `server.fsx` script in your FSI - this will start relay-compatible F# server on port 8083. Then build node.js frontend by getting all dependencies (`npm i`) and running it (`npm run serve | npm run dev`) - this will start webpack server running React application using Relay for managing application state. You can visit it on [http://localhost:8083/](http://localhost:8083/) .

In order to update client schema, visit [http://localhost:8083/](http://localhost:8083/) and copy-paste the response (which is introspection query result from current F# server) into *data/schema.json*.

## Stream features

Stream directive now has additional features, like batching (buffering) by interval and/or batch size. To make it work, a custom stream directive must be placed inside the `SchemaConfig.Directives` list, this custom directive containing two optional arguments called `interval` and `preferredBatchSize`:

```fsharp
let customStreamDirective =
    let args = [|
        Define.Input(
            "interval",
            Nullable Int,
            defaultValue = Some 2000,
            description = "An optional argument used to buffer stream results. ")
        Define.Input(
            "preferredBatchSize",
            Nullable Int,
            defaultValue = None,
            description = "An optional argument used to buffer stream results. ") |]
    { StreamDirective with Args = args }
let schemaConfig =
    { SchemaConfig.Default with
        Directives = [
            IncludeDirective
            SkipDirective
            DeferDirective
            customStreamDirective
            LiveDirective ] }
```

This boilerplate code can be easily reduced with a built-in implementation:

```fsharp
let streamOptions =
    { Interval = Some 2000; PreferredBatchSize = None }
let schemaConfig =
    SchemaConfig.DefaultWithBufferedStream(streamOptions)
```

## Live queries

Live directive is now supported by the server component. To support live queries, each field of each type of the schema needs to be configured as a live field. This is done by using `ILiveFieldSubscription` and `ILiveQuerySubscriptionProvider`, which can be configured in the `SchemaConfig`:

```fsharp
type ILiveFieldSubscription =
    interface
        abstract member Identity : obj -> obj
        abstract member TypeName : string
        abstract member FieldName : string
    end

and ILiveFieldSubscription<'Object, 'Identity> =
    interface
        inherit ILiveFieldSubscription
        abstract member Identity : 'Object -> 'Identity
    end

and ILiveFieldSubscriptionProvider =
    interface
        abstract member HasSubscribers : string -> string -> bool
        abstract member IsRegistered : string -> string -> bool
        abstract member AsyncRegister : ILiveFieldSubscription -> Async<unit>
        abstract member TryFind : string -> string -> ILiveFieldSubscription option
        abstract member Add : obj -> string -> string -> IObservable<obj>
        abstract member AsyncPublish<'T> : string -> string -> 'T -> Async<unit>
    end
```

To set a field as a live field, call `Register` extension method. Each subscription needs to know an object identity, so it must be configured on the Identity function of the `ILiveFieldSubscription`. Also, the name of the Type and the field inside the `ObjectDef` needs to be passed along:

```fsharp
let schemaConfig = SchemaConfig.Default
let schema = Schema(root, config = schemaConfig)
let subscription =
    { Identity = fun (x : Human) -> x.Id
      TypeName = "Hero"
      FieldName = "name" }

schemaConfig.LiveFieldSubscriptionProvider.Register subscription
```

With that, the field name of the hero is now able to go live, being updated to clients whenever is queried with live directive. To push updates to subscribers, just call Publish method, passing along the type name, the field name and the updated object:

```fsharp
let updatedHero = { hero with Name = "Han Solo - Test" }
schemaConfig.LiveFieldSubscriptionProvider.Publish "Hero" "name" updatedHero
```

## File upload and multipart requests

Server component now supports multipart request, which enables features like file upload in mutation queries. This feature is still experimental, and is built based on [this specification](https://github.com/jaydenseric/graphql-multipart-request-spec).

To support it, first install `FSharp.Data.GraphQL.Server.Middlewares.AspNetCore` package. The component has a special GraphQL input type called `Upload` which can be used on mutation queries. This input type is based on a scalar definition of the `System.IO.Stream` type, which can not be parsed as an input by normal means - only by a multipart request reader.

```fsharp
type File =
    { Name : string
      ContentType : string
      Content : Stream }

let Upload : InputObjectDefinition<File> =
    { Name = "Upload"
        Description = Some "The `Upload` type is used to represent a file upload to the server."
        FieldsFn = fun () ->
        [|
            Define.Input("name", String, description = "Gets the name of the file.")
            Define.Input("contentType", String, description = "Gets the MIME content type of the file.")
            Define.Input("content", Stream, description = "Gets the content of the file.")
        |] }

type Operation =
    { Query : string
      Variables : Map<string, obj> }

type MultipartRequest =
    { Operations : Operation list }
```

To be able to run a query using multipart request, that request must be read using a `Microsoft.AspNetCore.WebUtilities.MultipartReader`. After that, it can be parsed into a `MultipartRequest` object, using the `MultipartRequest` module. This parsing will automatically converts uploaded files unsing the spec into the `File` object.

```fsharp
let reader = MultipartReader(boundary, requestStream)
let request = MultipartRequest.read reader |> Async.AwaitTask |> Async.RunSynchronously
```

An example on how to use this feature can be seen in the Giraffe sample project.

## Middlewares

You can create and use middlewares on top of the `Executor<'Root>` object.

The query execution process through the use of the Executor involves three phases:

- *Schema compile phase:* this phase happens when the `Executor<'Root>` class is instantiated. In this phase, the Schema map of types is used to build a field execute map, which contains all field definitions alongside their field resolution functions. This map is used later on the planning and execution phases to retrieve the values of the queried fields of the schema.

- *Operation planning phase:* this phase happens before running a query that has no execution plan. This phase is responsible to analyze the AST document generated by the query, and build an ExecutionPlan to execute it.

- *Operation execution phase:* this phase is actually the phase that executes the query itself. It needs an execution plan, so, it commonly happens after the operation planning phase.

All those phases wraps needed data to do the phase job inside an Context object. They are expressed internally by functions:

```fsharp
let internal compileSchema (ctx : SchemaCompileContext) : unit =
  // ...

let internal planOperation (ctx: PlanningContext) : ExecutionPlan =
  // ...

let internal executeOperation (ctx : ExecutionContext) : AsyncVal<GQLResponse> =
  // ...
```

That way, in the compile schema phase, the Schema is modified and execution maps are generated inside the `SchemaCompileContext` object. On the operation planning phase, values of the `PlanningContext` object are used to generate an execution plan, and finally, this plan is passed alongside other values in the `ExecutionContext` object to the operation execution phase, wich finally uses them to execute the query and generate a `GQLResponse`.

With that being said, a middleware can be used to intercept each of those phases, and make customizations to them, modifying operations as needed. Each middleware must be implemented as a function with specific signature, and wrapped inside an `IExecutorMiddleware` interface:

```fsharp
type SchemaCompileMiddleware =
    SchemaCompileContext -> (SchemaCompileContext -> unit) -> unit

type OperationPlanningMiddleware =
    PlanningContext -> (PlanningContext -> ExecutionPlan) -> ExecutionPlan

type OperationExecutionMiddleware =
    ExecutionContext -> (ExecutionContext -> AsyncVal<GQLResponse>) -> AsyncVal<GQLResponse>

type IExecutorMiddleware =
    abstract CompileSchema : SchemaCompileMiddleware option
    abstract PlanOperation : OperationPlanningMiddleware option
    abstract ExecuteOperationAsync : OperationExecutionMiddleware option
```

Optionally, for ease of implementation, concrete class to derive from can be used, receiving only the optional sub-middleware functions in the constructor:

```fsharp
type ExecutorMiddleware(?compile, ?plan, ?execute) =
    interface IExecutorMiddleware with
        member __.CompileSchema = compile
        member __.PlanOperation = plan
        member __.ExecuteOperationAsync = execute
```

Each of the middleware functions acts like an intercept function, with two parameters: the context of the phase, the function of the next middleware (or the actual phase itself, wich is the last to run), and the return value. Those functions can be passed as an argument to the constructor of the `Executor<'Root>` object:

```fsharp
let middlewares = [ ExecutorMiddleware(compileFn, planningFn, executionFn) ]
let executor = Executor(schema, middlewares)
```

A simple example of a practical middleware can be one that measures the time needed to plan a query, and returns it on the Metadata of the planning context. The metadata object is a `Map<string, obj>` implementation that acts like a bag of information to be passed through each phase, until it is returned inside the `GQLResponse` object. You can use it to thread custom information through middlewares:

```fsharp
let planningMiddleware (ctx : PlanningContext) (next : PlanningContext -> ExecutionPlan) =
    let watch = Stopwatch()
    watch.Start()
    let result = next ctx
    watch.Stop()
    let metadata = result.Metadata.Add("planningTime", watch.ElapsedMilliseconds)
    { result with Metadata = metadata }
```

### Built-in middlewares

There are some built-in middlewares inside `FSharp.Data.GraphQL.Server.Middlewares` package:

#### QueryWeightMiddleware

This middleware can be used to place weights on fields of the schema. Those weightened fields can now be used to protect the server from complex queries that otherwise could be used to create things like a DDoS attack.

When defining a field, we use the extension method `WithQueryWeight` to place a weight on it:

```fsharp
let resolveFn (h : Human) =
  h.Friends |> List.map getCharacter |> List.toSeq

let field =
  Define.Field("friends", ListOf (Nullable CharacterType),
    resolve = resolveFn).WithQueryWeight(0.5)
```

Then we define the threshold middleware for the Executor. If we execute a query that ask for "friends of friends" in a recursive way, the executor will only accept nesting them 4 times before the query exceeds the weight threshold of 2.0:

```fsharp
let middlewares = [ Define.QueryWeightMiddleware(2.0) ]
```

#### ObjectListFilterMiddleware

This middleware can be used to automatically generate a filter for list fields inside an object of the schema. This filter can be passed as an argument for the field on the query, and recovered in the `ResolveFieldContext` argument of the resolve function of the field.

For example, we can create a middleware for filtering list fields of an `Human` object, that are of the type `Character option`:

```fsharp
let middlewares = [ Define.ObjectListFilterMiddleware<Human, Character option>() ]
```

The filter argument is an object that is mapped through a JSON definition inside an `filter` argument on the field. A simple example would be filtering friends of a hero that have their names starting with the letter A:

```graphql
query TestQuery {
    hero(id:"1000") {
        id
        name
        appearsIn
        homePlanet
        friends (filter : { name_starts_with: "A" }) {
            id
            name
        }
    }
}
```

This filter is mapped by the middleware inside an `ObjectListFilter` definition:

```fsharp
type FieldFilter<'Val> =
    { FieldName : string
      Value : 'Val }

type ObjectListFilter =
    | And of ObjectListFilter * ObjectListFilter
    | Or of ObjectListFilter * ObjectListFilter
    | Not of ObjectListFilter
    | Equals of FieldFilter<System.IComparable>
    | GreaterThan of FieldFilter<System.IComparable>
    | LessThan of FieldFilter<System.IComparable>
    | StartsWith of FieldFilter<string>
    | EndsWith of FieldFilter<string>
    | Contains of FieldFilter<string>
    | FilterField of FieldFilter<ObjectListFilter>
```

And the value recovered by the filter in the query is usable in the `ResolveFieldContext` of the resolve function of the field. To easy access it, you can use the extension method `Filter`, wich returns an `ObjectListFilter option` (it does not have a value if the object doesn't implement a list with the middleware generic definition, or if the user didn't provide a filter on the query).

```fsharp
Define.Field("friends", ListOf (Nullable CharacterType),
    resolve = fun ctx (d : Droid) -> 
        ctx.Filter |> printfn "Droid friends filter: %A"
        d.Friends |> List.map getCharacter |> List.toSeq)
```

By retrieving this filter on the field resolution context, it is possible to use client code to customize the query against a database, for example, and extend your GraphQL API features.

### LiveQueryMiddleware

This middleware can be used to quickly allow your schema fields to be able to be queried with a `live` directive, assuming that all of them have an identity property name that can be discovered by a function, `IdentityNameResolver`:

```fsharp
/// A function that resolves an identity name for a schema object, based on a object definition of it.
type IdentityNameResolver = ObjectDef -> string
```

For example, if all of our schema objects have an identity field named `Id`, we could use our middleware like this:

```fsharp
let schema = Schema(query = queryType)

let middlewares = [ Define.LiveQueryMiddleware(fun _ -> "Id") ]

let executor = Executor(schema, middlewares)
```

The `IdentityNameResolver` is optional, though. If no resolver function is provided, this default implementation of is used. Also, notifications to subscribers must be done via `Publish` of `ILiveFieldSubscriptionProvider`, like explained above.

### Using extensions to build your own middlewares

You can use extension methods provided by the `FSharp.Data.GraphQL.Shared` package to help building your own middlewares. When making a middleware, often you will need to modify schema definitions to add features to the schema defined by the user code. The `ObjectListFilter` middleware is an example, where all fields that implements lists of a certain type needs to be modified, by accepting an argument called `filter`.

As field definitions are immutable by default, generating copies of them with improved features can be a hard work sometimes. This is where the extension methods can help: for example, if you need to add an argument to an already defined field inside the schema compile phase, you can use the method `WithArgs` of the `FieldDef<'Val>` interface:

```fsharp
let field : FieldDef<'Val> = // Search for field inside ISchema
let arg : Define.Input("id", String)
let fieldWithArg = field.WithArgs([ arg ])
```

To see the complete list of extensions used for improve definitions, you can take a look at the `TypeSystemExtensions` module of the `FSharp.Data.GraphQL.Shared` package.
# Relay Modern Starter Kit with Pagination

This kit includes an app server, a GraphQL server, and a transpiler that you can use to get started building an app with Relay. For a walkthrough, see the [Relay tutorial](https://facebook.github.io/relay/docs/tutorial.html).

Few changes have been made to the original starter kit. It is now compatible with the [Relay Modern](https://facebook.github.io/relay/docs/relay-modern.html) API version. It also uses a [PaginationContainer](https://facebook.github.io/relay/docs/PaginationContainer.html) for item retrieving.

## Installation

First install required npm modules:

```
npm install
```

Generate JavaScript sources of Relay-wrapped components by executing:

```
npm run relay
```

## Running

Start the GraphQL server by executing:

```
fsharpi server.fsx
```

Start the Relay application server:

```
npm start
```

Visit `localhost:8090` in your browser in order to access the application.

## Developing

### GraphQL Server

Following information supposed to get you started with modification of the GraphQL server.

#### Debugging

The sample contains a `server.fs` copy of the server and an F# project file which can be used for debugging with mono:

```
xbuild && mono --debug --debugger-agent=transport=dt_socket,server=y,address=127.0.0.1:55555 ./bin/Debug/mono/FSharp.Data.GraphQL.ModernRelay.exe 
```

This will start the server awaiting for the mono debugger to be attached.

#### Uri

By default the server will start on `localhost:8083`. This can be modified by changing `address` and `port` in `server.fsx` or `server.fs` respectively. Changing this settings requires adjustment of `graphqlServerUri` in `./src/relayEnvironment.jsx`.

### Relay Application

#### Schema modification

Each time the schema is modified in the `server.fsx` GraphQL server, a new schema file has to be generated on the Relay application side.

Accessing the `localhost:8083` in your browser will yield an introspection reply containing the schema of the database. Save this response to `./data/schema.json` whenever the schema is getting changed.

Relay API has to know the path to the schema file. `.babelrc` contains this information by setting the `relay` plugin with `schema` option. Adjust this if the path to schema should change.

The generated `./data/schema.json` or `./data/schema.graphql` (see optional below) file is used for the ahead-of-time compilation by Relay Modern compiler. The compilation process should be triggered by executing:

```
npm run relay
```

The command checks relevant `.jsx` and `.js` files in `./src` folder and generates their compiled  in `__generated__` for defined containers. Those are e.g. [FragmentContainer](https://facebook.github.io/relay/docs/fragment-container.html) or the root Relay tree [QueryRenderer](https://facebook.github.io/relay/docs/query-renderer.html). 

#### Optional

There are two accepted schema formats: `.json` and `.graphql`. The conversion to the latter can be done by executing `npm run json-to-graphql`. Input and output files can be adjusted in `./scripts/jsonToGraphql.js`.

## License

Relay Starter Kit is [BSD licensed](./LICENSE). We also provide an additional [patent grant](./PATENTS).

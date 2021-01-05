const {
  Environment,
  Network,
  RecordSource,
  Store,
} = require('relay-runtime');

const graphqlServerUri = 'http://127.0.0.1:8083'

// Define a function that fetches the results of an operation (query/mutation/etc)
// and returns its results as a Promise:
function fetchQuery(
  operation,
  variables
) {
  return fetch(graphqlServerUri, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      query: operation.text,
      variables: JSON.stringify(variables)
    }),
  }).then(response => {
    return response.json();
  });
}

export default new Environment({
  // Create a network layer from the fetch function
  network: Network.create(fetchQuery),
  store: new Store(new RecordSource()),
});
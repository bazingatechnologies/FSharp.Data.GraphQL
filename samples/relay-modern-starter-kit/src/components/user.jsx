const {
  createPaginationContainer, 
  graphql
} = require('react-relay/compat');

class UserComponent extends React.Component {
  render() {
    return (
      <div>
        <h1>Widget list</h1>
        <ul>
          {this.props.viewer.widgets.edges.map(
            edge => <li key={edge.node.id}>{edge.node.name} (Node ID: {edge.node.id})</li>
          )}
        </ul>
        <button onClick={() => this._loadMore()}>Load one more</button>
      </div>
    );
  }
  _loadMore() {
    if (!this.props.relay.hasMore() || this.props.relay.isLoading()) {
      return;
    }

    this.props.relay.loadMore(
      1, // Fetch the next widget
      e => {
        // console.log(e);
      },
    );
  }
}

// Wrapping the React component into a Relay container.
export default createPaginationContainer(
  UserComponent,
  {
    viewer: graphql`
      fragment user_viewer on User {
        widgets(
          first: $count
          after: $cursor
        ) @connection(key: "User_widgets") {
          pageInfo {
                hasNextPage
                hasPreviousPage
                startCursor
                endCursor
             }
          edges {
            node {
              id,
              name
            }
          }
        }
      }
    `,
  },
  {
    direction: 'forward',
    getConnectionFromProps(props) {
        return props.viewer && props.viewer.widgets;
    },
    getFragmentVariables(prevVars, totalCount) {
      return {
        ...prevVars,
        count: totalCount,
      };
    },
    getVariables(props, {count, cursor}, fragmentVariables) {
      return {
        count,
        cursor
      };
    },
    query: graphql`
      query userPaginationQuery(
        $count: Int!
        $cursor: String
      ) {
        viewer {
          # You could reference the fragment defined previously.
          ...user_viewer
        }
      }
    `
  }
);
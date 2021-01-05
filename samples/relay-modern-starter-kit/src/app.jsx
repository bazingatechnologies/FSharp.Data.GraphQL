import User from "./components/user"
import environment from "./relayEnvironment"

const {
  QueryRenderer,
  graphql,
} = require('react-relay');
const ReactDOM = require('react-dom');

const mountNode = document.getElementById('root');

ReactDOM.render(
  <QueryRenderer
    environment={environment}
    query={graphql`
      query appQuery($count: Int!, $cursor: String) {
        viewer {
          ...user_viewer
        }
      }
    `}
    variables={{cursor: null, count: 1}}
    render={({error, props}) => {
      if(error) { console.log(error.source); return <div>{error.message}</div> }
      if (props) {
        return <User viewer={props.viewer} />;
      } else {
        return <div>Loading</div>;
      }
    }}
  />,
  mountNode
);
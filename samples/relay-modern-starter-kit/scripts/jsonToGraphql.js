var fs = require('fs');

const {
  buildClientSchema,
  printSchema
} = require('graphql/utilities');

fs.readFile('./data/schema.json', "utf8", (err, str) => {
    if (err) throw err;
    fs.writeFileSync('./data/schema.graphql', printSchema(buildClientSchema(JSON.parse(str).data)));
})
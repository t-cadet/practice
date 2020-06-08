const cors          = require('cors')
const express       = require('express')
const graphqlHTTP   = require('express-graphql')
const mongoose      = require('mongoose')
const schema        = require('./schema/schema')



const ATLAS_URI = "mongodb+srv://test:test@gql-ninja-nr0r0.mongodb.net/gql-ninja?retryWrites=true&w=majority"
const PORT = 4000

const app = express()

app.use(cors())
app.use('/graphql', graphqlHTTP({
    schema,
    graphiql: true
}))

mongoose.connect(ATLAS_URI, {useNewUrlParser: true, useCreateIndex: true, useUnifiedTopology: true, useFindAndModify: false})
mongoose.connection.once('open', () => console.log('DB connection opened'))

app.listen(PORT, () => console.log("Now listening for requests on http://localhost:" + PORT))
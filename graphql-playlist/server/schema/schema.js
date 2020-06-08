const Author    = require('../models/author')
const Book      = require('../models/book')
const graphql   = require('graphql')
const _         = require('lodash')

const {GraphQLObjectType, GraphQLString, GraphQLSchema, GraphQLID, GraphQLInt, GraphQLList, GraphQLNonNull} = graphql

// dummy data
/*var books = [
      {name: 'Name of the Wind', genre: 'Fantasy', id: '1', authorId: '1'}
    , {name: 'The Final Empire', genre: 'Fantasy', id: '2', authorId: '2'}
    , {name: 'The Long Earth', genre: 'Sci-Fi', id: '3', authorId: '3'}
    , {name: 'The Hero of Ages', genre: 'Fantasy', id: '4', authorId: '2'}
    , {name: 'The Colour of Magic', genre: 'Fantasy', id: '5', authorId: '3'}
    , {name: 'The Light Fantastic', genre: 'Fantasy', id: '6', authorId: '3'}
]

var authors = [
    {name: 'Patrick Rothfuss', age: 44, id: '1'},
    {name: 'Brandon Sanderson', age: 42, id: '2'},
    {name: 'Terry Pratchett', age: 66, id: '3'}
]*/

const BookType = new GraphQLObjectType({
    name: 'Book',
    fields: () => ({
          id: {type: GraphQLID}
        , name: {type: GraphQLString}
        , genre: {type: GraphQLString}
        , author: {
            type: AuthorType,
            resolve(parent, args) { // TODO combine (reuse) with RootQuery author
                // return _.find(authors, {id: parent.authorId})
                return Author.findById(parent.authorId)
            }
        }
    })
})

const AuthorType = new GraphQLObjectType({
    name: 'Author',
    fields: () => ({
          id: {type: GraphQLID}
        , name: {type: GraphQLString}
        , age: {type: GraphQLInt}
        , books: {
            type: new GraphQLList(BookType)
            , resolve(parent, args) {
                // return _.filter(books, {authorId: parent.id})
                return Book.find({authorId: parent.id})
            }
        }
    })
})

const RootQuery = new GraphQLObjectType({
    name: 'RootQueryType',
    fields: {
        book: {
            type: BookType
            , args: {id: {type: GraphQLID}} // FEATURE SUGGESTION: infer the type for me (DRY) 
            , resolve(parent, args) { // FEATURE SUGGESTION: infer the request for me (I don't want to care about the DB, create the schema and the request for me, like mongoose would)
                // code to get data from db
                // return _.find(books, {id: args.id})
                return Book.findById(args.id)
            }
        }
        , books: {
            type: GraphQLList(BookType) // TODO did not use new, why does this work ?
            , resolve(parent, args) {
                // return books
                return Book.find()
            }
        }
        , author: {
            type: AuthorType
            , args: {id: {type: GraphQLID}}
            , resolve (parent, args) {
                // return _.find(authors, {id: args.id})
                return Author.findById(args.id)
            }
        }
        , authors: {
            type: new GraphQLList(AuthorType)
            , resolve() {
                // return authors
                return Author.find()
            }
        }
    }
})


const Mutation = new GraphQLObjectType({
    name: 'Mutation'
    , fields: {
        addAuthor: {
            type: AuthorType
            , args: {
                name: { type: new GraphQLNonNull(GraphQLString)}
                , age: { type: new GraphQLNonNull(GraphQLInt)}
            }
            , resolve(parent, args) {
                return Author.create(args)
            }
        }
        , addBook: {
            type: BookType
            , args: {
                name: {type: new GraphQLNonNull(GraphQLString)}
                , genre: {type: new GraphQLNonNull(GraphQLString)}
                , authorId: {type: new GraphQLNonNull(GraphQLID)}
            }
            , resolve(parent, args) {
                return Book.create(args)
            }
        }
    }
})

module.exports = new GraphQLSchema({
    query: RootQuery
    , mutation: Mutation
})
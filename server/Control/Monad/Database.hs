{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Database where

-- | A simple monad to abstract over the database implementation.
--
-- Given the operations being performed are rather simple the underlying
-- database can be MySQL, Mongo, DynamoDB or whatever else.
--
-- In this abstract view we have the following hierarchy with each arrow being
-- in a one to many relationship.
--
--    database -> collection -> record -> field

-- | Parse the value form the database
class FromRecord m a where
    fromRecord :: Record m -> a

-- | Transform the value into something the database can store
class ToRecord m a where
    toRecord :: a -> Record m

class MonadDB m where
    data Record m :: *
    retrieve :: FromRecord m a => Query a -> m a
    store :: ToRecord m a => Insert a -> m ()

data Target = Target Database Collection

-- | A database is a group of collections
newtype Database = Database String

database :: String -> Database
database = Database

-- | A collection of records
newtype Collection = Collection String

collection :: String -> Collection
collection = Collection

type FieldValue = FieldValue String String

data Query a
    = SelectMany [FieldValue]
    | SelectOne [FieldValue]

data Insert a
    = Insert a
    | Update [FieldValue] a

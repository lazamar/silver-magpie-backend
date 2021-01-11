{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Monad.Database
    ( FromRecord (..),
      ToRecord (..),
      IsValue (..),
      MonadDB (..),
      Target (..),
      Collection (..),
      Database (..),
      FieldValue (..),
      Delete (..),
      Insert (..),
      (=:)
    )
where

-- | A simple monad to abstract over the database implementation.
--
-- Given the operations being performed are rather simple the underlying
-- database can be MySQL, Mongo, DynamoDB or whatever else.
--
-- In this abstract view we have the following hierarchy with each arrow being
-- in a one to many relationship.
--
--    database -> collection -> record -> field

class MonadDB m where
    type Record m :: * -- ^ A way to represent data in a collection
    type Value m :: * -- ^ A way to represent attributes of data in a collection
    store :: ToRecord m a => Target -> Insert m a -> m ()
    retrieveOne :: FromRecord m a => Target -> [FieldValue m] -> m (Maybe a)
    delete :: Target -> Delete m -> m ()

-- | Parse value from a database collection
class FromRecord m a where
    fromRecord :: Record m -> a

-- | Transform the value into something the database can store
class ToRecord m a where
    toRecord :: a -> Record m

-- | A type that represents attributes of a collection's content
class IsValue m a where
    toValue :: a -> Value m

-- The target of a database operation
data Target = Target Database Collection

-- | A database is a group of collections
newtype Database = Database String

-- | A collection of records
newtype Collection = Collection String

-- | Label and content of a field
data FieldValue m where
    FieldValue :: IsValue m a => String -> a -> FieldValue m

infix 0 =:
(=:) :: IsValue m a => String -> a -> FieldValue m
(=:) = FieldValue

data Insert m a
    = Insert a
    | Update [FieldValue m] a

data Delete m
    = DeleteMany [FieldValue m]

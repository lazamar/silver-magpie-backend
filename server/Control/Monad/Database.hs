{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Database
    ( FromRecord (..),
      ToRecord (..),
      ToValue (..),
      MonadDB (..),
      Target (..),
      Collection (..),
      Database (..),
      FieldValue (..),
      Delete (..),
      Insert (..),
      (=:),
    )
where

import Data.Kind (Constraint)
import Data.Text (Text)

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
    type ToRecord m a :: Constraint
    type FromRecord m a :: Constraint
    type ToValue m a :: Constraint
    store :: ToRecord m a => Target -> Insert m a -> m ()
    retrieveOne :: FromRecord m a => Target -> [FieldValue m] -> m (Maybe a)
    delete :: Target -> Delete m -> m ()

-- The target of a database operation
data Target = Target Database Collection

-- | A database is a group of collections
newtype Database = Database Text

-- | A collection of records
newtype Collection = Collection Text

-- | Label and content of a field
data FieldValue m where
    FieldValue :: ToValue m a => Text -> a -> FieldValue m

infix 0 =:

(=:) :: ToValue m a => Text -> a -> FieldValue m
(=:) = FieldValue

data Insert m a
    = Insert a
    | Update [FieldValue m] a

data Delete m
    = DeleteMany [FieldValue m]

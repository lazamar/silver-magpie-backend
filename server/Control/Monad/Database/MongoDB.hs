{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Database.MongoDB where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Control.Monad.Database as DB
import qualified Data.Bson as Bson
import qualified Data.Text as Text
import qualified Database.MongoDB as Mongo

-- The class

class MonadMongoDB m where
    execMongo :: DB.Target -> MongoAction a -> m a

data MongoAction a where
    FindOne :: Mongo.Query -> MongoAction (Maybe Mongo.Document)
    Insert :: Mongo.Collection -> Mongo.Document -> MongoAction ()
    Upsert :: Mongo.Selection -> Mongo.Document -> MongoAction ()
    Delete :: Mongo.Selection -> MongoAction ()

instance forall m. (MonadMongoDB m , MonadIO m) => DB.MonadDB m where
    type Record m = Bson.Document
    type Value m = Bson.Value

    store target = \case
        DB.Insert value ->
            execMongo target
                . Insert (collection target)
                $ DB.toRecord @m value
        DB.Update fields value ->
            execMongo target
                . Upsert (Mongo.select (map toSelector fields) $ collection target)
                $ DB.toRecord @m value

    retrieve target = \case
        DB.SelectOne fields -> do
            let selection = Mongo.select (map toSelector fields) $ collection target
            found <- execMongo target $ FindOne selection
            return $ DB.fromRecord @m <$> maybe [] pure found

    delete target = \case
        DB.DeleteMany fields -> do
            let selection = Mongo.select (map toSelector fields) $ collection target
            execMongo target $ Delete selection

-- The implementation

-- | Transformer to run MongoDB queries
newtype MongoT m a = MongoT (ReaderT Mongo.Pipe m a)
    deriving newtype (MonadIO , Monad , Functor , Applicative , MonadTrans)

runMongoDB :: Mongo.Pipe -> MongoT m a -> m a
runMongoDB pipe (MongoT m) = runReaderT m pipe

instance (MonadIO m) => MonadMongoDB (MongoT m) where
    execMongo (DB.Target (DB.Database db) _) act = do
        pipe <- MongoT ask
        Mongo.access pipe Mongo.master (Text.pack db) $ case act of
            FindOne q -> Mongo.findOne q
            Insert c d -> void $ Mongo.insert c d
            Upsert s d -> Mongo.upsert s d
            Delete s -> Mongo.delete s

instance {-# OVERLAPPABLE #-} (MonadMongoDB m , Monad m , MonadTrans t) => MonadMongoDB (t m) where
    execMongo target action = lift $ execMongo target action

collection :: DB.Target -> Mongo.Collection
collection (DB.Target _ (DB.Collection col)) = Text.pack col

toSelector :: forall m. DB.FieldValue m -> Bson.Field
toSelector (DB.FieldValue field content) = Text.pack field Bson.:= DB.toValue @m content

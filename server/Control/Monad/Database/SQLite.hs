{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Database.SQLite where

import Control.Concurrent.MVar (MVar, newMVar, swapMVar, withMVar, takeMVar, putMVar)
import Control.Monad (void)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask, finally, mask, onException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..), ask, local, reader, runReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.List (intercalate)
import Data.String (fromString)
import Database.SQLite.Simple (FromRow, NamedParam, Query, SQLData, ToRow)
import qualified Control.Monad.Database as DB
import qualified Data.Bson as Bson
import qualified Data.Text as Text
import qualified Database.MongoDB as Mongo
import qualified Database.SQLite.Simple as SQL

-- | Access an SQL database
class Monad m => MonadSQL m where
    execute :: ToRow q => Query -> q -> m ()
    executeNamed :: Query -> [NamedParam] -> m ()
    execute_ :: Query -> m ()
    query :: (ToRow q , FromRow r) => Query -> q -> m [r]
    query_ :: (FromRow r) => Query -> m [r]
    queryNamed :: FromRow r => Query -> [NamedParam] -> m [r]
    withTransaction :: m a -> m a

runMonadSQL :: (MonadMask m, MonadIO m) => FilePath -> MonadSQLT m a -> m a
runMonadSQL path (MonadSQLT m) = do
    conn <- liftIO $ SQL.open path
    writeLock <- liftIO $ newMVar True
    let state = DBState conn writeLock
    finally
        (runReaderT m state)
        (liftIO $ SQL.close conn)

data DBState = DBState
    { conn :: SQL.Connection
    , -- | Is transaction still active
      writeLock :: MVar Bool
    }

instance (Monad m, MonadReader DBState m, MonadMask m , MonadIO m, MonadSQL m) => MonadSQL m where
    execute q v = write $ \conn -> liftIO $ SQL.execute conn q v
    executeNamed q v =  write $ \conn -> liftIO $ SQL.executeNamed conn q v
    execute_ q = write $ \conn -> liftIO $ SQL.execute_ conn q
    query q v = reader conn >>= \conn -> liftIO $ SQL.query conn q v
    query_ q = reader conn >>= \conn -> liftIO $ SQL.query_ conn q
    queryNamed q v = reader conn >>= \conn -> liftIO $ SQL.queryNamed conn q v
    withTransaction action =
        write $ \conn -> do
            transactionWriteLock <- liftIO $ newMVar True
            let closeTransaction = liftIO $ swapMVar transactionWriteLock False
            local (\s -> s {writeLock = transactionWriteLock}) $ runTransaction conn closeTransaction
        where
            runTransaction conn closeTransaction = do
                mask $ \restore -> do
                    begin
                    r <- restore (action <* closeTransaction) `onException` rollback
                    commit
                    return r
                where
                    begin = liftIO $ SQL.execute_ conn "BEGIN TRANSACTION"
                    commit = liftIO $ SQL.execute_ conn "COMMIT TRANSACTION"
                    rollback = liftIO $ SQL.execute_ conn "ROLLBACK TRANSACTION"

newtype MonadSQLT m a = MonadSQLT (ReaderT DBState m a)
    deriving newtype (Functor , Applicative , Monad, MonadIO, MonadReader DBState)

deriving newtype instance (MonadThrow m) => MonadThrow (MonadSQLT m)
deriving newtype instance (MonadCatch m) => MonadCatch (MonadSQLT m)
deriving newtype instance (MonadMask m) => MonadMask (MonadSQLT m)
deriving newtype instance (MonadError e m) => MonadError e (MonadSQLT m)

instance MonadTrans MonadSQLT where
    lift = MonadSQLT . lift

write :: (MonadIO m, MonadMask m, MonadReader DBState m)
      => (SQL.Connection -> m a)
      -> m a
write action = do
    DBState conn writeLock <- ask
    withMVar' writeLock $ \case
        False -> error "Trying to write after transaction was closed"
        True -> action conn

withMVar' :: (MonadMask m, MonadIO m) => MVar a -> (a -> m b) -> m b
withMVar' m act =
  mask $ \restore -> do
    a <- liftIO $ takeMVar m
    b <- restore (act a) `onException` liftIO (putMVar m a)
    liftIO (putMVar m a)
    return b

instance forall m. (MonadMask m , MonadIO m) => DB.MonadDB (MonadSQLT m) where
    type Record (MonadSQLT m) = [SQLData]
    type Value (MonadSQLT m) = SQLData

    store (DB.Target _ (DB.Collection col)) = \case
        DB.Insert val -> do
            let record = DB.toRecord @(MonadSQLT m) val
            execute
                (fromString $ "INSERT OR REPLACE INTO " <> col <> "VALUES (" <> queryRep record <> ")")
                record
        DB.Update q val -> do
            -- check whether the data in q is primary key, if not throw not error
            let record = DB.toRecord @(MonadSQLT m) val
            execute
                (fromString $ "INSERT OR REPLACE INTO " <> col <> "VALUES (" <> queryRep record <> ")")
                record

    retrieve (DB.Target _ (DB.Collection col)) = \case
        DB.SelectOne fields -> do
            let vars = zip varNames fields
                q =
                    fromString $
                        unwords
                            [ "SELECT * FROM " <> col
                            , "WHERE " <> unwords (map asFieldQuery vars)
                            , "LIMIT 1"
                            ]

            records <- queryNamed q (map asNamedParam vars)
            return $ fmap (DB.fromRecord @(MonadSQLT m)) records

    delete (DB.Target _ (DB.Collection col)) = \case
        DB.DeleteMany fields -> do
            let vars = zip varNames fields
                q =
                    fromString $
                        unwords
                            [ "DELETE FROM " <> col
                            , "WHERE " <> unwords (map asFieldQuery vars)
                            ]
            executeNamed q (map asNamedParam vars)

queryRep :: [SQLData] -> String
queryRep xs = intercalate "," $ replicate (length xs) "?"

asFieldQuery :: (VarName , DB.FieldValue m) -> String
asFieldQuery (VarName varName , DB.FieldValue field _) = field <> " = " <> varName

asNamedParam :: forall m. (VarName , DB.FieldValue (MonadSQLT m)) -> NamedParam
asNamedParam (VarName varName , DB.FieldValue _ val) =
    Text.pack varName SQL.:= DB.toValue @(MonadSQLT m) val

newtype VarName = VarName String

-- | Placeholder names for searches
varNames :: [VarName]
varNames = [VarName $ ":" <> pure letter | letter <- ['a' .. 'z']]

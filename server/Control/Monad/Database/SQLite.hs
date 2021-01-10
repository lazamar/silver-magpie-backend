{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Database.SQLite where

import Control.Concurrent.Classy.MVar (MVar, newMVar, swapMVar, withMVar)
import Control.Monad (void)
import Control.Monad.Catch (MonadMask, finally, mask, onException)
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..), ask, local, reader, runReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Database.SQLite.Simple (FromRow, NamedParam, Query, ToRow)
import qualified Control.Monad.Database as DB
import qualified Data.Bson as Bson
import qualified Data.Text as Text
import qualified Database.MongoDB as Mongo
import qualified Database.SQLite.Simple as SQL

-- | Access an SQL database
class Monad m => MonadSQL m where
    execute :: ToRow q => Query -> q -> m ()
    execute_ :: Query -> m ()
    query :: (ToRow q , FromRow r) => Query -> q -> m [r]
    query_ :: (FromRow r) => Query -> m [r]
    queryNamed :: FromRow r => Query -> [NamedParam] -> m [r]
    withTransaction :: m a -> m a

runMonadSQL :: (MonadConc m , MonadIO m) => FilePath -> MonadSQLT m a -> m a
runMonadSQL path (MonadSQLT m) = do
    conn <- liftIO $ SQL.open path
    writeLock <- newMVar True
    let state = DBState conn writeLock
    finally
        (runReaderT m state)
        (liftIO $ SQL.close conn)

data DBState m = DBState
    { conn :: SQL.Connection
    , -- | Is transaction still active
      writeLock :: MVar m Bool
    }

instance (MonadConc m , MonadMask m , MonadIO m) => MonadSQL (MonadSQLT m) where
    execute q v = MonadSQLT $ write $ \conn -> liftIO $ SQL.execute conn q v
    execute_ q = MonadSQLT $ write $ \conn -> liftIO $ SQL.execute_ conn q
    query q v = MonadSQLT $ reader conn >>= \conn -> liftIO $ SQL.query conn q v
    query_ q = MonadSQLT $ reader conn >>= \conn -> liftIO $ SQL.query_ conn q
    queryNamed q v = MonadSQLT $ reader conn >>= \conn -> liftIO $ SQL.queryNamed conn q v
    withTransaction (MonadSQLT action) = MonadSQLT $
        write $ \conn -> do
            transactionWriteLock <- newMVar True
            let closeTransaction = swapMVar transactionWriteLock False
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

newtype MonadSQLT m a = MonadSQLT (ReaderT (DBState m) m a)
    deriving newtype (Functor , Applicative , Monad)

instance MonadTrans MonadSQLT where
    lift = MonadSQLT . lift

write :: MonadConc m => (SQL.Connection -> ReaderT (DBState m) m a) -> ReaderT (DBState m) m a
write action = do
    DBState conn writeLock <- ask
    withMVar writeLock $ \case
        False -> error "Trying to write after transaction was closed"
        True -> action conn

----

-- instance DB.MonadDB (MonadSQLT m) where
--     type Record (MonadSQLT m) = [SQL.SQLData]
--     type Value (MonadSQLT m) = String
--
--     store target = undefined
--     retrieve target = undefined
--     delete target = undefined

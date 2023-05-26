{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Auth.Authentication where

import Database.Persist.Postgresql
import Database.Redis.Sentinel
import qualified Network.Wai as Wai

import Control.Monad.Reader
import Data.IORef
import Control.Monad.IO.Unlift (MonadUnliftIO)

data SessionStore = SessionStore {
    conn :: SentinelConnection
}


newtype Auth a = Auth (ReaderT SessionStore IO a)
    deriving (Monad, MonadIO, Functor, Applicative, MonadUnliftIO)

authRequired :: ConnectionPool -> String -> (ConnectionPool -> Wai.Application) -> Wai.Application
authRequired pool from login req send = do

    login pool req send
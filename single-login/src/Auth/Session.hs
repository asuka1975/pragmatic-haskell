module Auth.Session
    (
        SessionStoreRedis (..)
      , SessionIO (..)
    ) where 

import Database.Redis
import Data.ByteString
import Control.Monad.State (liftIO)

type UserId      = ByteString
type UserSession = ByteString

data SessionStoreRedis = SessionStoreRedis {
    conn :: Connection
}

class SessionIO a where
    verify :: a -> UserId -> UserSession -> IO Bool

instance SessionIO SessionStoreRedis where
    verify rds userId userSessionChallenge = do
        runRedis (conn rds) $ do
            userSession <- get userId
            liftIO $ return $ userSession == (Right $ Just $ userSessionChallenge)


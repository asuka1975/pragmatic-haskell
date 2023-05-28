module Auth.Session
    (
        SessionStoreRedis (..)
      , SessionIO (..)
    ) where 

import Database.Redis
import Data.ByteString
import Codec.Binary.UTF8.String 
import Control.Monad.Cont (MonadIO(liftIO))

type UserSession = ByteString

newtype SessionStoreRedis = SessionStoreRedis {
    conn :: Connection
}

toByteString :: String -> ByteString
toByteString s = pack $ encode s

createSessionId :: IO ByteString
createSessionId = do
    return $ toByteString "sessionwao"

class SessionIO a where
    verify :: a -> UserSession -> IO Bool
    create :: a -> IO UserSession

instance SessionIO SessionStoreRedis where
    verify rds userSession = do
        runRedis (conn rds) $ do
            existed <- get userSession
            return $ existed == (Right $ Just $ toByteString "")
    create rds = do
        runRedis (conn rds) $ do
            sid <- liftIO createSessionId
            _ <- set sid $ toByteString ""
            _ <- expire sid 60
            return sid


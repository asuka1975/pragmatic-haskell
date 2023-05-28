{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Auth.Authentication
    (
        authRequired
    ) where

import Database.Persist.Postgresql
import Database.Redis.Sentinel
import qualified Network.Wai     as Wai
import qualified Data.Map        as M
import qualified Data.ByteString as B

import Control.Monad.Reader
import Data.IORef
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Network.HTTP.Types       as HTypes
import Network.URI

import Auth.Session


tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)



authRequired :: SessionIO a => a -> B.ByteString -> B.ByteString -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived -> IO Wai.ResponseReceived  
authRequired session from login req send app = do
    authRequired'' $ authRequired' userId userSession
    where
        headers     = M.fromList $ Wai.requestHeaders req
        cookie      = headers M.!? HTypes.hCookie
        makeCookieMap Nothing  = []
        makeCookieMap (Just c) = map (tuplify . B.split 61) $ map (B.dropWhile (==32)) $ B.split 59 c
        cookieMap   = M.fromList $ makeCookieMap cookie
        userId      = cookieMap M.!? "userid"
        userSession = cookieMap M.!? "sessionid"
        authRequired' uidEith usEith = do
            uid <- uidEith
            us  <- usEith
            return (uid, us)
        authRequired'' (Just (uid, us)) = do
            authenticated <- verify session uid us
            if authenticated
                then app
                else send $ Wai.responseBuilder HTypes.status307 [(HTypes.hLocation, login)] ""
        authRequired'' Nothing = send $ Wai.responseBuilder HTypes.status307 [(HTypes.hLocation, login)] ""

        



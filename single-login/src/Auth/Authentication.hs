{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Auth.Authentication
    (
        AuthApplication
      , withAuth
      , authRequired
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

type AuthApplication = Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> Either (IO Wai.ResponseReceived) (IO Wai.ResponseReceived)

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)

withAuth :: Either (IO Wai.ResponseReceived) (IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
withAuth (Left app)  = app
withAuth (Right app) = app

authRequired :: SessionIO a => a -> B.ByteString -> B.ByteString -> AuthApplication
authRequired session from login req send = do
    Left $ send $ Wai.responseBuilder HTypes.status307 [(HTypes.hLocation, login)] ""
    where
        headers   = M.fromList $ Wai.requestHeaders req
        cookie    = headers M.!? HTypes.hCookie
        makeCookieMap Nothing  = []
        makeCookieMap (Just c) = map (tuplify . B.split 61) $ map (B.dropWhile (==32)) $ B.split 59 c
        cookieMap = M.fromList $ makeCookieMap cookie


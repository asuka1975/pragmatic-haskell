{-# LANGUAGE OverloadedStrings #-}
module Application 
    (
        app
    ) where

import Database.Persist.Postgresql
import Data.Yaml.Config
import Data.UUID.V4   (nextRandom)
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding
import Text.EDE

import Database.Redis

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai              as Wai
import Network.Wai.Session
import qualified Network.HTTP.Types       as HTypes
import Network.HTTP.Types.Method (methodGet, methodPost)

import Model
import Auth.Password
import Auth.Session
import Auth.Authentication

app :: Int -> IO ()
app port = do
    -- print $ hashedPassword "password"
    -- uuid <- nextRandom
    -- session <- pgsession
    -- insertUser session $ User { _userUid      = show uuid
    --                        , _userEmail    = "sample@example.com"
    --                        , _userPassword = "password" }
    -- users <- getUsers session
    -- print users
    session <- checkedConnect redisConnInfo
    putStrLn $ "Running on http://localhost:" ++ show port
    Warp.run port $ router $ SessionStoreRedis { conn = session }
    where
        redisConnInfo = defaultConnectInfo {
            connectHost = "localhost"
          , connectPort = PortNumber 6379
        }

router :: SessionIO a => a -> Wai.Application
router session req = \send -> withAuth $ case Wai.pathInfo req of
    []           -> index session req send
    ["login"]    -> login session req send
    ["register"] -> register session req  send
    _            -> notFound session req  send

index :: SessionIO a => a -> AuthApplication
index session req send = case Wai.requestMethod req of
    methodGet  -> do
        authRequired session "/" "/login" req send
        indexImpl session req send
    _          -> notFound session req send
    where
        indexImpl session req send = Right $ do
            print $ Wai.rawPathInfo req
            tpl <- eitherParseFile "public/templates/index.html"
            let env  = fromPairs []
                body = either error toStrict $ tpl >>= (`eitherRender` env)
                in send $ Wai.responseBuilder HTypes.status200 [("Content-Type", "text/html")] $ encodeUtf8Builder body

login :: SessionIO a => a -> AuthApplication
login session req send = case Wai.requestMethod req of 
    methodGet -> do Right $ send $ Wai.responseBuilder HTypes.status200 [] "login"

register :: SessionIO a => a -> AuthApplication
register session req send = Right $ send $ Wai.responseBuilder HTypes.status200 [] ""

notFound :: SessionIO a => a -> AuthApplication
notFound session req send = Right $ send $ Wai.responseBuilder HTypes.status404 [] "Not Found"
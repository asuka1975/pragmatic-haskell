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

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai              as Wai
import Network.Wai.Session
import qualified Network.HTTP.Types       as HTypes
import Network.HTTP.Types.Method (methodGet, methodPost)

import Model
import Auth.Password

app :: Int -> IO ()
app port = do
    print $ hashedPassword "password"
    uuid <- nextRandom
    pool <- pgPool
    insertUser pool $ User { _userUid      = show uuid
                           , _userEmail    = "sample@example.com"
                           , _userPassword = "password" }
    users <- getUsers pool
    print users
    putStrLn $ "Running on http://localhost:" ++ show port
    Warp.run port $ router pool

router :: ConnectionPool -> Wai.Application
router pool req = case Wai.pathInfo req of
    []           -> index pool req
    ["login"]    -> login pool req
    ["register"] -> register pool req
    _            -> notFound pool req

index :: ConnectionPool -> Wai.Application
index pool req send = case Wai.requestMethod req of
    methodGet  -> indexImpl pool req send
    _          -> notFound pool req send
    where
        indexImpl pool req send = do
            tpl <- eitherParseFile "public/templates/index.html"
            let env  = fromPairs []
                body = either error toStrict $ tpl >>= (`eitherRender` env)
                in send $ Wai.responseBuilder HTypes.status200 [("Content-Type", "text/html")] $ encodeUtf8Builder body

login :: ConnectionPool -> Wai.Application
login pool req send = case Wai.requestMethod req of 
    methodGet -> do send $ Wai.responseBuilder HTypes.status200 [] ""

register :: ConnectionPool -> Wai.Application
register pool req send = send $ Wai.responseBuilder HTypes.status200 [] ""

notFound :: ConnectionPool -> Wai.Application
notFound pool req send = send $ Wai.responseBuilder HTypes.status404 [] "Not Found"
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
    Warp.run port router

router :: Wai.Application
router req = case Wai.pathInfo req of
    []           -> index req
    ["login"]    -> login req 
    ["register"] -> register req 
    _            -> notFound req 



index :: Wai.Application
index req send = case Wai.requestMethod req of
    methodGet  -> do
        tpl <- eitherParseFile "public/templates/index.html"
        let env  = fromPairs []
            body = either error toStrict $ tpl >>= (`eitherRender` env)
            in send $ Wai.responseBuilder HTypes.status200 [("Content-Type", "text/html")] $ encodeUtf8Builder body
    _          -> notFound req send

login :: Wai.Application
login req send = case Wai.requestMethod req of 
    methodGet -> do send $ Wai.responseBuilder HTypes.status200 [] ""

register :: Wai.Application
register req send = send $ Wai.responseBuilder HTypes.status200 [] ""

notFound ::Wai.Application
notFound req send = send $ Wai.responseBuilder HTypes.status404 [] "Not Found"
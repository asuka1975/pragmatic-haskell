{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai  
import qualified Network.HTTP.Types as HTypes

import Application
import Model
import Auth.Password

import Data.UUID.V4 (nextRandom)

main :: IO ()
main = do
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
    where
        port = 8080

router :: Wai.Application
router req = case Wai.pathInfo req of
    []           -> index req
    ["login"]    -> login req
    ["register"] -> register req
    _            -> notFound req


index :: Wai.Application
index req send = send $ Wai.responseBuilder HTypes.status200 [] ""

login :: Wai.Application
login req send = send $ Wai.responseBuilder HTypes.status200 [] ""

register :: Wai.Application
register req send = send $ Wai.responseBuilder HTypes.status200 [] ""

notFound ::Wai.Application
notFound req send = send $ Wai.responseBuilder HTypes.status404 [] "Not Found"

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Codec.Binary.UTF8.String
import qualified Data.ByteString as S

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTypes

import OAuth2.AuthEndpoint

main :: IO ()
main = do 
    print "Running on http://127.0.0.1:8080"
    Warp.run 8080 router

router :: Wai.Application
router req = 
    case Wai.pathInfo req of
        ["auth"]        -> auth req
        ["authcheck"]   -> authcheck req
        ["token"]       -> token req
        _               -> notFound req

auth :: Wai.Application
auth req send = do
    print $ createRequestFromQuery $ Wai.queryString req
    send $ Wai.responseBuilder 
           HTypes.status200 
           [("Content-Type", "text/plain")] 
           ""

authcheck :: Wai.Application
authcheck req send = send $ Wai.responseBuilder HTypes.status200 [] ""

token :: Wai.Application
token req send = send $ Wai.responseBuilder HTypes.status200 [] ""

notFound :: Wai.Application
notFound req send = send $ Wai.responseBuilder HTypes.status404 [] "Not Found"
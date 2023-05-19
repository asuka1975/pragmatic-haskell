{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTypes

main :: IO ()
main = Warp.run 8080 router

router :: Wai.Application
router req = 
    case Wai.pathInfo req of
        ["auth"]        -> auth req
        ["authcheck"]   -> authcheck req
        ["token"]       -> token req
        _               -> notFound req

auth :: Wai.Application
auth req send = send $ Wai.responseBuilder HTypes.status200 [] ""

authcheck :: Wai.Application
authcheck req send = send $ Wai.responseBuilder HTypes.status200 [] ""

token :: Wai.Application
token req send = send $ Wai.responseBuilder HTypes.status200 [] ""

notFound :: Wai.Application
notFound req send = send $ Wai.responseBuilder HTypes.status404 [] "Not Found"
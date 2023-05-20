{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai 
import qualified Network.HTTP.Types as HTypes

main :: IO ()
main = do
    putStrLn $ "Running on http://localhost:" ++ show port
    Warp.run port router
    where
        port = 8080

router :: Wai.Application
router req =
    case Wai.pathInfo req of
        []        -> index req
        ["login"] -> login req
        _         -> notFound req

index :: Wai.Application
index req send = send $ Wai.responseBuilder HTypes.status200 [] "app"

login :: Wai.Application
login req send = send $ Wai.responseBuilder HTypes.status200 [] "login"

notFound :: Wai.Application
notFound req send = send $ Wai.responseBuilder HTypes.status404 [] "Not Found"

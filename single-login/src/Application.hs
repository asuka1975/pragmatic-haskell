{-# LANGUAGE OverloadedStrings #-}
module Application 
    (
        app
    ) where

import Data.Yaml.Config
import Data.UUID.V4   (nextRandom)
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding
import Text.EDE
import qualified Data.Map as M
import qualified Data.ByteString as B
import Codec.Binary.UTF8.String 

import Database.Redis
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai              as Wai
import Network.Wai.Parse
import Network.Wai.Session
import qualified Network.HTTP.Types       as HTypes
import Network.HTTP.Types.Header  (hSetCookie)
import Network.HTTP.Types.Method (methodGet, methodPost)

import Model
import Auth.Password
import Auth.Session
import Auth.Authentication

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

app :: Int -> IO ()
app port = do
    print $ hashedPassword "password"
    uuid <- nextRandom
    pool <- pgPool
    -- insertUser pool $ User { _userUid      = show uuid
    --                           , _userEmail    = "sample@example.com"
    --                           , _userPassword = hashedPassword "password" }
    session <- checkedConnect redisConnInfo
    putStrLn $ "Running on http://localhost:" ++ show port
    Warp.run port $ router pool $ SessionStoreRedis { conn = session }
    where
        redisConnInfo = defaultConnectInfo {
            connectHost = "localhost"
          , connectPort = PortNumber 6379
        }

router :: SessionIO a => ConnectionPool -> a -> Wai.Application
router pool session req = do
    case Wai.pathInfo req of
        []           -> index pool session req
        ["login"]    -> login pool session req
        ["register"] -> register pool session req
        _            -> notFound pool session req

index :: SessionIO a => ConnectionPool -> a -> Wai.Application
index pool session req send = do
        authRequired session "/" "/login" req send
            $ indexImpl session req send
    where
        indexImpl session req send = do
            tpl <- eitherParseFile "public/templates/index.html"
            let env  = fromPairs []
                body = either error toStrict $ tpl >>= (`eitherRender` env)
                in send $ Wai.responseBuilder HTypes.status200 [("Content-Type", "text/html")] $ encodeUtf8Builder body

login :: SessionIO a => ConnectionPool -> a -> Wai.Application
login pool session req send = case Wai.requestMethod req of 
    "GET" -> do 
        tpl <- eitherParseFile "public/templates/login.html"
        let env  = fromPairs []
            body = either error toStrict $ tpl >>= (`eitherRender` env)
            in send $ Wai.responseBuilder HTypes.status200 [("Content-Type", "text/html")] $ encodeUtf8Builder body
    "POST" -> do
        (params, files) <- parseRequestBody lbsBackEnd req
        let paramMap = M.fromList params
            ep       = getUserAndPassword (paramMap M.!? "email") (paramMap M.!? "password")
        b <- authenticate ep
        if b
            then send $ Wai.responseBuilder HTypes.status303 [(HTypes.hLocation, "/"), (hSetCookie, "userid=uid1"), (hSetCookie, "sessionid=session1")] ""
            else send $ Wai.responseBuilder HTypes.status303 [(HTypes.hLocation, "/login")] ""
        where
            getUserAndPassword emailMaybe passwordMaybe = do
                email <- emailMaybe
                password <- passwordMaybe
                return (email, password)
            authenticate (Just (email, password)) = do
                users <- getUsers pool
                let authInfo = map (\u -> (B.pack $ encode $ _userEmail u, _userPassword u)) $ map (entityVal) users
                return $ Just (email, hashedPassword password) == (head' authInfo)
            authenticate Nothing                  = return False


register :: SessionIO a => ConnectionPool -> a -> Wai.Application
register pool session req send = send $ Wai.responseBuilder HTypes.status200 [] ""

notFound :: SessionIO a => ConnectionPool -> a -> Wai.Application
notFound pool session req send = send $ Wai.responseBuilder HTypes.status404 [] "Not Found"
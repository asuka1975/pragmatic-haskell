{-# LANGUAGE OverloadedStrings #-}
module Application
    (
        app
    ) where

import Data.UUID.V4   (nextRandom)
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding
import Text.EDE
import qualified Data.Map as M
import qualified Data.ByteString as B
import Codec.Binary.UTF8.String

import Control.Monad.State (liftIO)

import Database.Redis
import Database.Persist hiding (get)
import Database.Persist.Postgresql hiding (get)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai              as Wai
import Network.Wai.Parse
import qualified Network.HTTP.Types       as HTypes
import Network.HTTP.Types.Header  (hSetCookie)

import Model
import Auth.Password
import Auth.Session
import Auth.Authentication

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

app :: Int -> IO ()
app port = do
    uuid <- nextRandom
    pool <- pgPool
    _ <- insertUser pool $ User { _userUid      = show uuid
                                , _userEmail    = "sample@example.com"
                                , _userPassword = hashedPassword "password" }
    session <- checkedConnect redisConnInfo
    putStrLn $ "Running on http://localhost:" ++ show port
    Warp.run port $ router pool $ SessionStoreRedis { conn = session }
    where
        redisConnInfo = defaultConnectInfo {
            connectHost = "localhost"
          , connectPort = PortNumber 6379
        }

router :: SessionIO a => ConnectionPool -> a -> Wai.Application
router pool session req send = do
    withAuth session $ do
        case Wai.pathInfo req of
            []           -> index pool session req send
            ["login"]    -> login pool session req send
            ["register"] -> register pool session req send
            _            -> notFound pool session req send

index :: SessionIO a => ConnectionPool -> a -> AuthApplication a
index _ session req send = do
    _ <- authRequired "login" req send
    indexImpl session req send
    where
        indexImpl _ _ send' = do
            tpl <- liftIO $ eitherParseFile "public/templates/index.html"
            let env  = fromPairs []
                body = either error toStrict $ tpl >>= (`eitherRender` env)
                in liftIO $ send' $ Wai.responseBuilder HTypes.status200 [("Content-Type", "text/html")] $ encodeUtf8Builder body

login :: SessionIO a => ConnectionPool -> a -> AuthApplication a
login pool session req send = case Wai.requestMethod req of
    "GET" -> do
        tpl <- liftIO $ eitherParseFile "public/templates/login.html"
        let env  = fromPairs []
            body = either error toStrict $ tpl >>= (`eitherRender` env)
            in liftIO $ send $ Wai.responseBuilder HTypes.status200 [("Content-Type", "text/html")] $ encodeUtf8Builder body
    "POST" -> do
        (params, _) <- liftIO $ parseRequestBody lbsBackEnd req
        let paramMap = M.fromList params
            ep       = getUserAndPassword (paramMap M.!? "email") (paramMap M.!? "password")
        b <- liftIO $ authenticate ep
        if b
            then do
                s <- liftIO $ create session
                liftIO $ send $ Wai.responseBuilder HTypes.status303 [(HTypes.hLocation, "/"), (hSetCookie, "sessionid=" <> s)] ""
            else liftIO $ send $ Wai.responseBuilder HTypes.status303 [(HTypes.hLocation, "/login")] ""
        where
            getUserAndPassword emailMaybe passwordMaybe = do
                email <- emailMaybe
                password <- passwordMaybe
                return (email, password)
            authenticate (Just (email, password)) = do
                users <- getUsers pool
                let authInfo = map ((\u -> (B.pack $ encode $ _userEmail u, _userPassword u)) . entityVal) users
                return $ Just (email, hashedPassword password) == head' authInfo
            authenticate Nothing                  = return False
    _      -> notFound pool session req send


register :: ConnectionPool -> a -> AuthApplication a
register _ _ _ send = liftIO $ send $ Wai.responseBuilder HTypes.status200 [] ""

notFound :: ConnectionPool -> a -> AuthApplication a
notFound _ _ _ send = liftIO $ send $ Wai.responseBuilder HTypes.status404 [] "Not Found"
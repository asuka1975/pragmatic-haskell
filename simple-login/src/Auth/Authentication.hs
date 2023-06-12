{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Auth.Authentication
    (
        withAuth
      , authRequired
      , AuthCtx (..)
      , AuthApplication
    ) where

import qualified Network.Wai            as Wai
import qualified Network.Wai.Internal   as WaiI
import qualified Data.Map        as M
import qualified Data.ByteString as B

import Control.Monad.Reader
import qualified Network.HTTP.Types       as HTypes

import Auth.Session
import Data.ByteString (ByteString)

newtype AuthCtx ctx a b = AuthCtx  (ReaderT ctx IO (Either a b))
type AuthApplication a = Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> AuthCtx a Wai.ResponseReceived Wai.ResponseReceived

instance Functor (AuthCtx ctx c) where
    fmap f (AuthCtx a) = AuthCtx $ do
        b <- a
        liftIO $ do
            return $ fmap f b

instance Applicative (AuthCtx ctx c) where
    pure = AuthCtx . return . Right
    (AuthCtx fReaderT) <*> (AuthCtx a) = AuthCtx $ do
        fIO <- fReaderT
        b   <- a
        liftIO $ do
            return $ fIO <*> b

instance Monad (AuthCtx ctx c) where
    return = pure
    (AuthCtx x) >>= f = AuthCtx $ do
        e <- x
        let (AuthCtx r) = f' e
        r
        where
            f' (Left l)  = AuthCtx $ return $ Left l
            f' (Right r) = f r

instance MonadIO (AuthCtx ctx c) where
    liftIO rio = AuthCtx $ do
        liftIO $ do
            Right <$> rio

tuplify :: [ByteString] -> (ByteString, ByteString)
tuplify []     = ("", "")
tuplify [x]    = (x, "")
tuplify [x, y] = (x, y)
tuplify (x:xs) = (x, B.concat xs)

withAuth :: a -> AuthCtx a Wai.ResponseReceived Wai.ResponseReceived -> IO Wai.ResponseReceived
withAuth x ctx = do
    a <- (`runReaderT` x) r
    return $ withAuth' a
    where
        (AuthCtx r)         = ctx
        withAuth' (Left y)  = y
        withAuth' (Right y) = y

authRequired :: SessionIO a => B.ByteString -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> AuthCtx a Wai.ResponseReceived Wai.ResponseReceived
authRequired login req send = AuthCtx $ do
    session <- ask
    liftIO $ do
        authRequired' session userSession loginPage
    where
        headers     = M.fromList $ Wai.requestHeaders req
        cookie      = headers M.!? HTypes.hCookie
        makeCookieMap Nothing  = []
        makeCookieMap (Just c) = map ((tuplify . B.split 61) . B.dropWhile (==32)) (B.split 59 c)
        cookieMap   = M.fromList $ makeCookieMap cookie
        userSession = cookieMap M.!? "sessionid"
        loginPage   = Wai.responseBuilder HTypes.status307 [(HTypes.hLocation, login)] ""
        authRequired' session (Just s) p1  = do
            ok <- verify session s
            if ok
                then do
                    return $ Right WaiI.ResponseReceived
                else do
                    p <- send p1
                    return $ Left p
        authRequired' _ Nothing p1 = do
            p <- send p1
            return $ Left p



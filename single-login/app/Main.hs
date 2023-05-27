{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Application

import Data.IORef           (IORef, newIORef, readIORef, writeIORef)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State  (liftIO)
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B

data Info = Info {
    password  :: String
  , path      :: String
  , sessionId :: String
  , userId    :: String
} 

data Res = Res {
    redirect :: String
} deriving Show

type App = Info -> IO Res

class SessionIO a where
    verify :: a -> String -> String -> IO Bool 

data MemorySession = MemorySession {
    content :: IORef (M.Map String String)
}

instance SessionIO MemorySession where
    verify s key sid = do
        sessionMap <- readIORef $ content s
        return $ (Just sid) == (sessionMap M.!? key)
        
withAuth :: SessionIO a => a -> Either (IO Res) (IO Res) -> IO Res
withAuth session r  = runReaderT (withAuth' r) session
    where
        withAuth' (Left a)  = do
            p <- ask
            liftIO a
        withAuth' (Right a) = liftIO a

authRequired :: Info -> String -> Either (IO Res) (IO ())
authRequired info p = case password info of
    "password" -> Right $ return ()
    _          -> Left  $ return $ Res { redirect = p }

index :: Info -> Either (IO Res) (IO Res)
index info = do
    authRequired info "login"
    Right $ return $ Res { redirect = path info }

other :: Info -> Either (IO Res) (IO Res)
other info = do
    Right $ return $ Res { redirect = path info }


main :: IO ()
main = do
    c <- newIORef $ M.fromList [("id", "10"), ("id_", "20")]
    session <- return $ MemorySession { content = c }
    r <- withAuth session $ case (path info) of
            "/"  -> index info
            _    -> other info
    print r
    app 8080
    where
        info = Info { 
            password  = "password"
          , path      = "/"
          , sessionId = "10"
          , userId    = "id"
        }
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}  
module Model where

import Control.Monad.Logger         (runStdoutLoggingT)
import Control.Monad.Trans.Reader   (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Lens                 ((^.))

import Data.Yaml.Config             (loadYamlSettings
                                   , useEnv
                                    )
                                    
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql
import Data.Text                    (Text)

share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "migrateAll"] 
    $ [persistLowerCase|
User 
    uid         String
    email       String
    password    String
    UniqueUserEmail email
    deriving Eq Show
|]

getUsers :: ConnectionPool -> IO [Entity User]
getUsers pool = flip runSqlPool pool $ selectList [] []

getUser :: ConnectionPool -> Key User -> IO (Maybe (Entity User))
getUser pool = flip runSqlPool pool . getEntity

insertUser :: ConnectionPool -> User -> IO (Maybe (Entity User))
insertUser pool user = flip runSqlPool pool $ do 
    mInDb <- getBy $ UniqueUserEmail $ user^.userEmail 
    case mInDb of
        Just inDb -> pure Nothing
        Nothing   -> do
            key <- insert user
            pure $ Just $ Entity key user

migrateDb :: IO ()
migrateDb = doMigration migrateAll

pgConf :: IO PostgresConf
pgConf = loadYamlSettings ["conf/database-setting.yml"] [] useEnv

pgPool :: IO ConnectionPool
pgPool = do
    conf <- pgConf
    runStdoutLoggingT $ createPostgresqlPool (pgConnStr conf) (pgPoolSize conf)

doMigration :: Migration -> IO ()
doMigration action = do
    conf <- pgConf
    runStdoutLoggingT 
      $ runResourceT 
      $ withPostgresqlConn (pgConnStr conf) 
      $ runReaderT
      $ runMigration action
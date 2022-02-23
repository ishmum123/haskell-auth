{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module DB
    ( initDB
    , initConnectionPool
    ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ


type DBConnectionString = ByteString

checkMigration :: Connection -> IO [Only Bool]
checkMigration conn = query_ conn [sql|
  SELECT EXISTS
    (
      SELECT 1
      FROM information_schema.tables
      WHERE table_schema = 'public'
      AND table_name = 'config'
    );
|]

createStructure :: Connection -> IO ()
createStructure conn = do
  execute_ conn [sql|
    DROP SCHEMA public CASCADE;
    CREATE SCHEMA public;

    GRANT ALL ON SCHEMA public TO postgres;
    GRANT ALL ON SCHEMA public TO public;

    CREATE TABLE users (id int not null, name text not null, password text not null);
    INSERT INTO users VALUES (1, 'dev', 'dev');
    INSERT INTO users VALUES (2, 'test', 'test');

    CREATE TABLE config (key text not null, value text not null);
    INSERT INTO config (key, value) VALUES ('migration', 'done');
  |]
  return ()

initDB :: DBConnectionString -> IO ()
initDB connstr = bracket (connectPostgreSQL connstr) close $ \conn -> do
  [Only b] <- checkMigration conn
  if b then pure () else createStructure conn

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe

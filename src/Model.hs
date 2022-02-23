{-# LANGUAGE DataKinds, MultiParamTypeClasses, AllowAmbiguousTypes, TemplateHaskell, TypeOperators, OverloadedStrings #-}
module Model
    ( User (..)
    , Post (..)
    , Comment (..)
    ) where

import Data.ByteString (ByteString)
import Data.Aeson
import Data.Aeson.TH
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple

--  TODO: Add `getId :: e -> i` and make delete by id optional
class Entity e i where
  getAll      :: Connection -> IO [e]
  findById    :: Connection -> i -> IO (Maybe e)
  add         :: Connection -> e -> IO e
  addAll      :: Connection -> [e] -> IO [e]
  update      :: Connection -> i -> e -> IO e
  delete      :: Connection -> e -> IO ()
  deleteById  :: Connection -> i -> IO ()

getUsers :: Connection -> IO [User]
getUsers conn = do
  users <- query_ conn "SELECT * FROM users"
  return $ map (\(id,name,pass) -> User id name pass) users

getUserById :: Connection -> Int -> IO (Maybe User)
getUserById conn id = do
  users <- query conn "SELECT * FROM users WHERE id = ?" (Only id)
  return $
    case users of
      [(id,name,pass)] -> Just $ User id name pass
      _ -> Nothing

createUser :: Connection -> User -> IO User
createUser conn (User id name pass) = do
  execute conn "INSERT INTO users VALUES (?, ?, ?)" (id,name,pass)
  [(id,name,pass)] <- query conn "SELECT * FROM users WHERE id = ? AND name = ? AND password = ?" (id,name,pass)
  return $ User id name pass


data User = User
  { uid        :: Int
  , name      :: String
  , password  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

instance Entity User Int where
  getAll = getUsers
  findById = getUserById
  add = createUser
  addAll conn = undefined
  update conn = undefined
  delete conn = undefined
  deleteById conn = undefined

data Post = Post
  { pid      :: Int
  , header  :: String
  , body    :: String
  , pdate    :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Post)

instance Entity Post Int where
  getAll conn = undefined
  findById conn = undefined
  add conn = undefined
  addAll conn = undefined
  update conn = undefined
  delete conn = undefined
  deleteById conn = undefined

data Comment = Comment
  { cid      :: Int
  , userId  :: Int
  , postId  :: Int
  , cdate    :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Comment)

instance Entity Comment Int where
  getAll conn = undefined
  findById conn = undefined
  add conn = undefined
  addAll conn = undefined
  update conn = undefined
  delete conn = undefined
  deleteById conn = undefined


{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( runApp
    , api
    , User (..)
    ) where

import Data.ByteString (ByteString)
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Servant

data User = User
  { id  :: Int
  , name    :: String
  , pass    :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Capture "id" Int :> Get '[JSON] User
      :<|> "users" :> Get '[JSON] [User]
      :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] NoContent


api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server conns = getUserById' :<|> getUsers' :<|> createUser'

  where getUsers' :: Handler [User]
        getUsers' = liftIO $ withResource conns getUsers

        getUserById' :: Int -> Handler User
        getUserById' id = liftIO $ withResource conns $ flip getUserById id

        createUser' :: User -> Handler NoContent
        createUser' a = do
          liftIO . withResource conns $ flip createUser a
          return NoContent


runApp :: Pool Connection -> IO ()
runApp conns = run 8080 (serve api $ server conns)

getUsers :: Connection -> IO [User]
getUsers conn = do
  users <- query_ conn "SELECT * FROM users"
  return $ map (\(id,name,pass) -> User id name pass) users

getUserById :: Connection -> Int -> IO User
getUserById conn id = do
  [(id, name, pass)] <- query conn "SELECT * FROM users WHERE id = ?" (Only id)
  return $ User id name pass


createUser :: Connection -> User -> IO ()
createUser conn (User id name pass) = do
  execute conn "INSERT INTO users VALUES (?, ?, ?)" (id,name,pass)
  return ()
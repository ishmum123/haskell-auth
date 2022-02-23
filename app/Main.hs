{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Concurrent
import Control.Exception (bracket)
import Servant
import Servant.Client

import Lib
import DB

getUsrById :<|> getUsrs :<|> crtUsr = client api

main :: IO ()
main = do
  let connStr = "dbname='ecommerce' user='dev' password='dev'"
  pool <- initConnectionPool connStr
  initDB connStr
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp pool) killThread $ \_ -> do
    ms <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
      getUsrById 4
    print ms

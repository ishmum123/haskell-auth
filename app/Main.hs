{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds, DeriveGeneric, TypeOperators #-}
import Data.Aeson
import GHC.Generics
import Data.Proxy
import System.IO
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant as S
import Servant.Client
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Control.Monad.IO.Class (liftIO)
import Data.Pool
import Database.PostgreSQL.Simple hiding ((:.))
import Data.ByteString (ByteString)

import DB

port :: Int
port = 3001

data AuthenticatedUser = AUser { auID :: Int
                               , auOrgID :: Int
                               } deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type API = "foo" :> Capture "i" Int :> Get '[JSON] ()

type APIServer =
  Auth '[SA.JWT, SA.BasicAuth] AuthenticatedUser :> API


getUserByCreds :: Connection -> ByteString -> ByteString -> IO (Maybe AuthenticatedUser)
getUserByCreds conn user pass = do
  ids <- query conn "SELECT id FROM users WHERE name = ? AND password = ?" (user,pass)
  return $
    case ids of
      [Only id] -> Just $ AUser id id
      _ -> Nothing

authCheck :: Pool Connection
          -> BasicAuthData
          -> IO (AuthResult AuthenticatedUser)
authCheck connPool (BasicAuthData login password) = do
  maybeUser <- withResource connPool $ \conn -> getUserByCreds conn login password
  return . maybe SAS.Indefinite Authenticated $ maybeUser

server :: Server APIServer
server (Authenticated user) = handleFoo
  where
    handleFoo :: Int -> Handler ()
    handleFoo n = liftIO $ hPutStrLn stderr $
      concat ["foo: ", show user, " / ", show n]
server _ = throwAll err401

mkApp :: Pool Connection -> IO Application
mkApp connPool = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      authCfg = authCheck connPool
      cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
      api = Proxy :: Proxy APIServer
  pure $ serveWithContext api cfg server

main :: IO ()
main = do
  let connStr = "dbname='ecommerce' user='dev' password='dev'"
  pool <- initConnectionPool connStr
  initDB connStr
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr
                           ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp pool

-- TODO: Story -> User views user count publicly; Tries to get all usernames; Needs to register; Add his profile picture
-- TODO: Add Tests
-- TODO: Add ORM
-- TODO: Clear up package yaml
module Main where

import GHC.Generics
import Control.Monad.Except
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.List
import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Calendar
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import Servant.API.Verbs

type UserId = String

data User = User
          { name :: String
          , age :: Int
          , email :: String
          } deriving Generic

instance ToJSON User
instance FromJSON User

data UserWithId = UserWithId
                { id :: UserId
                , user :: User
                } deriving Generic

instance ToJSON UserWithId

type UserDB = M.Map UserId User

{-
 - The main API type
 -}

type UserAPI = "users" :> 
               
                 -- methods on /users
               ( Get '[JSON] [UserWithId]
            :<|> Capture "userId" UserId :>

                   -- methods on /users/:userId
                 ( Get '[JSON] User
              :<|> ReqBody '[JSON] User :> PutNoContent '[JSON] ()
              :<|> Delete '[JSON] User
                 )
               )
          :<|> "about" :> Get '[JSON] String

type ServantM = ExceptT ServantErr IO

{-
 - Hook up handlers to routes
 -}

server :: TVar (UserDB) -> Server UserAPI
server tdb = ( allUsers
         :<|> (\userId -> getUser userId 
                     :<|> putUser userId
                     :<|> deleteUser userId )
            )
       :<|> return "This is an API for CRUDing users!"
  where
    mkUserWithId (id, usr) = UserWithId id usr

{-
 - These are the implementations of the route handlers.
 -}

    allUsers :: ServantM [UserWithId]
    allUsers =
      liftIO $ (map mkUserWithId . M.toList) <$> readTVarIO tdb

    getUser :: UserId -> ServantM User
    getUser userId = do
      muser <- liftIO $ M.lookup userId <$> readTVarIO tdb
      maybe (throwError err404) return muser

    putUser :: UserId -> User -> ServantM ()
    putUser userId user = do
      when (null userId) (throwError err400) -- zero-length username not ok
      liftIO $ atomically $ modifyTVar tdb (M.insert userId user)
      return ()

    deleteUser :: UserId -> ServantM User
    deleteUser userId = do
      muser <- liftIO $ atomically $ do
        muser <- M.lookup userId <$> readTVar tdb
        modifyTVar tdb $ M.delete userId
        return muser
      maybe (throwError err404) return muser

main = do
  userDB <- newTVarIO M.empty :: IO (TVar UserDB)
  run 8080 (logStdoutDev (serve (Proxy :: Proxy UserAPI)  (server userDB)))

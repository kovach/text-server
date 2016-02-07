{-# LANGUAGE OverloadedStrings #-}
module Network.TextServer where

-- imports lol
import Data.Monoid (mappend)
import Control.Monad (forever, forM_)
import Control.Exception (finally)

import Data.ByteString.Lazy (ByteString)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.UTF8 as B8 (toString)

import Network.WebSockets as WS
import Network.Wai (Application)
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as WaiStatic

type Id = Int
type Client = (Id, WS.Connection)
data SS a = SS
  { count :: Int
  , clients :: [Client]
  , world :: a
  }

initSS a = SS { count = 0, clients = [], world = a }

type Handler a = Text -> a -> (Text, a)

broadcast :: Id -> Text -> [Client] -> IO ()
broadcast id msg clients = forM_ clients send 
  where
    send (id', conn) | id /= id' = WS.sendTextData conn msg
    send _ = return ()

sendText :: Id -> Text -> MVar (SS a) -> IO ()
sendText id text state = do
  SS {clients = cs} <- readMVar state
  let Just conn = lookup id cs
  WS.sendTextData conn text

talk id conn state handler = forever $ do
  msg <- WS.receiveData conn
  T.putStrLn $ "got: " `mappend` msg
  SS {clients = cs, world = a} <- readMVar state
  let (m, a') = handler msg a
  modifyMVar_ state $ \s -> return $ s {world = a'}
  broadcast id m cs

application :: MVar (SS a) -> Handler a -> WS.PendingConnection -> IO ()
application state handler pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  SS {world = w, count = count} <- readMVar state
  let id = count + 1
  putStrLn $ "User #" ++ show id ++ " connected?"
  modifyMVar_ state $ \s -> return s {count = id, clients = (id, conn) : clients s}

  -- TODO send initial msg --

  --send id (T.pack . B8.toString . encode . t2v . n2t w $ root w) state
  sendText id "hello down there!" state

  -- Main loop
  finally (talk id conn state handler) (disconnect id state)

staticContent :: Network.Wai.Application
staticContent = WaiStatic.staticApp $ WaiStatic.defaultWebAppSettings "./static"

disconnect id state = do
  putStrLn $ "User #" ++ show id ++ " disconnected?"
  modifyMVar_ state $ \s -> return $ removeClient id s
  --SS {clients = cs} <- readMVar state
  --broadcast id (prefixMsg id "disconnected.") cs

removeClient id s = s { clients = filter (not . (== id) . fst) (clients s) }

-- Entry point --
runServer a fn = do
  state <- newMVar (initSS a)
  Warp.run 8080 (WaiWS.websocketsOr WS.defaultConnectionOptions (application state fn) staticContent)

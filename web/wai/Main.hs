{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.IORef
import Network.HTTP.Types
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import System.IO.Unsafe

-- | Run the web app
main :: IO ()
main = do
  _ <- liftIO gameThread
  r <- liftIO $ newIORef 0
  run 8000 (socketsApp r)

-- | This is the main web app that handles all requests
--   it handles websocket connections itself, and passes
--   all other requests to the default app
socketsApp :: IORef Int -> Wai.Application
socketsApp ref = websocketsOr defaultConnectionOptions wsApp defaultApp where
  wsApp :: ServerApp  -- ServerApp is just this: IORef Int -> PendingConnection -> IO ()
  wsApp pending_conn = do
    i <- readIORef ref
    case i of
      0 -> connect connection1 pending_conn ref 1
      1 -> connect connection2 pending_conn ref 2
      _ -> rejectRequest pending_conn "too many players"

-- | This is a simple app that just serves up some html and javascript
defaultApp :: Wai.Application
defaultApp request respond = go where
  go = do
    putStrLn (B.unpack $ Wai.rawPathInfo request)
    respond $ case Wai.rawPathInfo request of
      "/"        -> plainIndex
      "/game.js" -> gameJS
      bad        -> notFound bad
  plainIndex    = Wai.responseFile status200 [(hContentType, "text/html")] "web/wai/index.html" Nothing
  gameJS        = Wai.responseFile status200 [(hContentType, "text/javascript")] "web/wai/game.js" Nothing
  notFound path = Wai.responseLBS  status404 [("Content-Type", "text/plain")] (LB.append "404 - Not Found: " (LB.fromStrict path))

-- | Connect to an incoming websocket request, set up communication with it in general.
connect :: Show a => MVar Connection -> PendingConnection -> IORef a -> a -> IO ()
connect convar pending_conn ref i = go where
  go = do
    conn <- acceptRequest pending_conn
    writeIORef ref i
    putMVar convar conn
    forkPingThread conn 30
    talk conn
  talk :: Network.WebSockets.Connection -> IO ()
  talk conn = forever $ do
    msg <- receiveData conn
    sendTextData conn (msg :: B.ByteString)

{-# NOINLINE connection1 #-}
connection1 :: MVar Connection
connection1 = unsafePerformIO newEmptyMVar

{-# NOINLINE connection2 #-}
connection2 :: MVar Connection
connection2 = unsafePerformIO newEmptyMVar

gameThread :: IO ThreadId
gameThread = forkIO $ do
  c1 <- readMVar connection1
  threadDelay 2000000
  c2 <- readMVar connection2
  sendTextData c1 ("game started!! you are player 1" :: B.ByteString)
  sendTextData c2 ("game started!! you are player 2" :: B.ByteString)

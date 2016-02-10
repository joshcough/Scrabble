{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Aeson (eitherDecode, encode)
import Data.IORef
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Types
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Scrabble
import System.IO.Unsafe

-- | Run the web app
main :: IO ()
main = do
  _ <- liftIO gameStartThread
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
  plainIndex    = Wai.responseFile status200 [(hContentType, "text/html")]       "web/wai-server/index.html" Nothing
  gameJS        = Wai.responseFile status200 [(hContentType, "text/javascript")] "web/wai-server/game.js" Nothing
  notFound path = Wai.responseLBS  status404 [("Content-Type", "text/plain")] (LB.append "404 - Not Found: " (LB.fromStrict path))

-- | Connect with the incoming websocket request, set up communication with it in general.
connect :: Show a => MVar Connection -> PendingConnection -> IORef a -> a -> IO ()
connect convar pending_conn ref i = do
  conn <- acceptRequest pending_conn
  writeIORef ref i
  putMVar convar conn
  forkPingThread conn 30
  forever $ receiveMove conn -- listen for moves, forever.

{-# NOINLINE connection1 #-}
connection1 :: MVar Connection
connection1 = unsafePerformIO newEmptyMVar

{-# NOINLINE connection2 #-}
connection2 :: MVar Connection
connection2 = unsafePerformIO newEmptyMVar

-- | wait for two players to connect
--   then send them their player names, and the new game.
gameStartThread :: IO ThreadId
gameStartThread = forkIO $ do
  c1 <- readMVar connection1
  c2 <- readMVar connection2
  g  <- newGame $ NE.fromList [human "player1", human "player2"]
  sendTextData c1 ("player1" :: LB.ByteString)
  sendTextData c2 ("player2" :: LB.ByteString)
  updateBothClients g

-- | send an updated game state to both clients.
updateBothClients :: Game -> IO ()
updateBothClients g = do
  c1 <- readMVar connection1
  c2 <- readMVar connection2
  let json = encode g
  sendTextData c1 json
  sendTextData c2 json
  return ()

-- | receive a move (and game) from a player, and attempt to apply it
--   if it isn't the players turn, or the move results in an error,
--   then send the error message back on the connection
--   if it succeeds, send the new game state to both players
-- TODO: check if it is the players turn.
receiveMove :: Connection -> IO ()
receiveMove conn = do
  gameAndMove <- receiveData conn
  case decodeGameAndMove gameAndMove of
    Right g     -> updateBothClients g
    Left errMsg -> sendTextData conn (B.pack errMsg)

decodeGameAndMove :: LB.ByteString -> Either String Game
decodeGameAndMove gameAndMove = do
  (g,m) <- eitherDecode (gameAndMove :: LB.ByteString)
  applyWordPut g m

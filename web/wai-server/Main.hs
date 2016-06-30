{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad                  (forever)
import Control.Monad.Trans
import Data.Aeson                     (eitherDecode, encode)
import Data.IORef
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Scrabble
import System.IO.Unsafe
import ClientMessage

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as LB
import qualified Data.List.NonEmpty    as NE
import qualified Network.Wai           as Wai

type PlayerName = B.ByteString

-- | Run the web app
main :: IO ()
main = do
  _ <- liftIO gameStartThread
  r <- liftIO $ newIORef 0
  run 8000 (socketsApp r)

-- | This is the main web app that handles all requests
--   it handles web socket connections itself, and passes
--   all other requests to the default app
socketsApp :: IORef Int -> Wai.Application
socketsApp ref = websocketsOr defaultConnectionOptions wsApp defaultApp where

  -- | The app that handles web socket requests
  wsApp :: ServerApp  -- ServerApp is just this: IORef Int -> PendingConnection -> IO ()
  wsApp pending_conn = do
    i <- readIORef ref
    case i of
      0 -> connect player1 pending_conn ref 1
      1 -> connect player2 pending_conn ref 2
      _ -> rejectRequest pending_conn "too many players"

  -- |
  defaultApp :: Wai.Application
  defaultApp request respond = go where
    go = do
      putStrLn (B.unpack $ Wai.rawPathInfo request)
      respond $ case Wai.rawPathInfo request of
        "/"        -> plainIndex
        "/game.js" -> gameJS
        bad        -> notFound (LB.fromStrict bad)
    plainIndex    = Wai.responseFile status200
                                     [(hContentType, "text/html")]
                                     "web/wai-server/index.html"
                                     Nothing
    gameJS        = Wai.responseFile status200
                                     [(hContentType, "text/javascript")]
                                     "web/wai-server/game.js" Nothing
    notFound path = Wai.responseLBS  status404
                                     [("Content-Type", "text/plain")]
                                     (LB.append "404 - Not Found: " path)

-- | Connect with the incoming websocket request,
-- set up communication with it in general.
connect :: MVar (PlayerName, Connection)
        -> PendingConnection
        -> IORef Int
        -> Int
        -> IO ()
connect convar pending_conn ref i = do
  conn <- acceptRequest pending_conn
  writeIORef ref i
  -- as soon as we connect, they must send their name.
  name <- receiveData conn :: IO B.ByteString
  -- send them back an id
  sendTextData conn (B.pack $ show i)
  putMVar convar (name, conn)
  forkPingThread conn 30
  forever $ receiveMessage i conn -- listen for moves, forever.

-- | Receive a message from the client.
--   Decode the message and check if it's the player's turn
--   propagating error message along the way then match on the
--   MessageType and handle each message appropriately.
--   If the client just wants a ValidityCheck
--     * apply the move but only return whether it
--       applied successfully or not
--     * if the client wants an ActualMove, then apply the
--       move and return the updated game state or an error
receiveMessage :: Int
               -> Connection
               -> IO ()
receiveMessage pid conn = do
    messageData <- receiveData conn
    case decodeAndCheckTurn messageData of
        Right (Message ActualMove g m) ->
            case applyWordPut g m of
                Right newGame -> updateBothClients newGame
                Left errMsg   -> sendTextData conn (B.pack errMsg)
        Right (Message ValidityCheck g m) ->
            sendTextData conn $ encode $
                either (const False) (const True) (applyWordPut g m)
        Left errMsg -> sendTextData conn (B.pack errMsg)
    where
        decodeAndCheckTurn :: B.ByteString -> Either String ClientMessage
        decodeAndCheckTurn m = do
            msg@(Message _ g _) <- eitherDecode $ LB.fromStrict m
            -- thread ids (`pid` in this function) are one ahead of the client side player id
            if (playerId . NE.head $ gamePlayers g) + 1  ==  pid
            then Right msg
            else Left "Not your turn"


-- | send an updated game state to both clients.
updateBothClients :: Game -> IO ()
updateBothClients g = do
  c1 <- connection1
  c2 <- connection2
  let json = encode g
  sendTextData c1 json
  sendTextData c2 json

-- ===== Horrible mutable state below ===== --

-- | wait for two players to connect
--   then send them their player names, and the new game.
gameStartThread :: IO ThreadId
gameStartThread = forkIO $ do
  p1 <- player1Name
  p2 <- player2Name
  g  <- newGame $ NE.fromList [human $ B.unpack p1, human $ B.unpack p2]
  -- this is just a hack until players can enter their name.
  updateBothClients g

{-# NOINLINE player1 #-}
player1 :: MVar (PlayerName, Connection)
player1 = unsafePerformIO newEmptyMVar

{-# NOINLINE player2 #-}
player2 :: MVar (PlayerName, Connection)
player2 = unsafePerformIO newEmptyMVar

player1Name :: IO PlayerName
player1Name = readMVar player1 >>= return . fst

player2Name :: IO PlayerName
player2Name = readMVar player2 >>= return . fst

connection1 :: IO Connection
connection1 = readMVar player1 >>= return . snd

connection2 :: IO Connection
connection2 = readMVar player2 >>= return . snd

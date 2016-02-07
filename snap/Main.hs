{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Applicative
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import qualified Data.List.NonEmpty    as NE
import Scrabble
import Scrabble.Snap.JSON
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
  ifTop (writeBS "hello world") <|>
  route [ ("newGame",  newGameHandler)
        , ("makeMove", moveHandler)
        ] <|>
  dir "static" (serveDirectory ".")

-- | Takes in a list of players and returns a new game with those players
--   The game is in JSON format, created from its ToJSON instance.
newGameHandler :: Snap ()
newGameHandler = go where
  go = getParam "players" >>= liftIO . newGame . parsePlayers >>= writeJSON
  parsePlayers bs = NE.fromList $ human <$> playersString where
    playersString = B.unpack <$> (B.split ',' $ maybe (error "no players") id bs)

-- | Takes in a [Game, WordPut] pair in JSON format
--   applies the move to the game, and sends back a JSON result.
--   the result is an (Either String Game).
--   String for errors (such as an invalid word),
--   or the new state of the game if successful.
moveHandler :: Snap ()
moveHandler = do
  eWpG <- getJSON :: Snap (Either String (Game, WordPut))
  writeJSON $ uncurry applyWordPut <$> eWpG
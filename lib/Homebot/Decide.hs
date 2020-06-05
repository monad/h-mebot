{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Homebot.Decide (command, parser, handle) where

import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Except   (ExceptT)
import Data.Function                ((&))
import Data.Text                    (Text)
import Discord.Types                (messageChannel)
import Homebot.Common               (Command (..), TaskEnvironment (..), send)
import System.Random                (randomRIO)
import Text.ParserCombinators.ReadP
    (ReadP, char, many1, readP_to_S, satisfy, sepBy)

import qualified Data.Text        as T
import qualified Discord.Requests as R

command :: Text
command = "decide"

parser :: ReadP [String]
parser = many1 anyChar `sepBy` char '|'
  where anyChar = satisfy (/= '|')

nonempty :: [a] -> Maybe [a]
nonempty [] = Nothing
nonempty xs = Just xs

handle :: TaskEnvironment -> ExceptT String IO ()
handle e@TaskEnvironment {..} = do
  case choices of
    Nothing       -> send e $ R.CreateMessage source "Well that's easy! You should do nothing."
    Just choices' -> do
      index <- liftIO $ randomRIO (0, length choices' - 1)
      send e $ R.CreateMessage (messageChannel teMessage) $ T.pack $ choices' !! index
  pure ()
  where
    source = messageChannel teMessage
    choices :: Maybe [String]
    choices = commandArgs teCommand
      & T.unwords
      & T.unpack
      & readP_to_S parser
      & last
      & fst
      & nonempty

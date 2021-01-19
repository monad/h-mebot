{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Homebot.Decide (command, parser, handle, katCommand, katHandle) where

import Control.Monad.IO.Class       (liftIO, MonadIO)
import Control.Monad.Trans.Except   (ExceptT)
import Data.Char                    (isSpace)
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

katCommand :: Text
katCommand = "kat"

parser :: ReadP [String]
parser = many1 anyChar `sepBy` char '|'
  where anyChar = satisfy (/= '|')

nonempty :: [a] -> Maybe [a]
nonempty [] = Nothing
nonempty xs = Just xs

justify :: MonadIO m => Text -> m Text
justify thing' = (mconcat . ((!!) justifications)) <$> liftIO (randomRIO (0, length justifications - 1))
  where
    thing = thing' & T.dropWhile isSpace & T.reverse & T.dropWhile isSpace & T.reverse
    justifications =
      [ ["Give ", thing, " a try for an hour and see if you find your flow state :heart:"]
      , ["You'll feel accomplished if you make progress on ", thing, "!"]
      , ["Self care is important, you'll feel better rested and more capable of work later if you ", thing, " now :two_hearts:"]
      , ["Try a little ", thing, " see if you burn out quickly, and if you do, then try something else!"]
      , ["You should definitely ", thing, ", you'll feel great if you do."]
      , ["If it were me, I'd ", thing, ", but you know best!"]
      , ["I want you to ", thing, " for me! It might feel hard but please try :heart:"]
      , ["If you get some ", thing, " done then you'll feel great and I'll also be super proud of you >:3"]
      ]

katHandle :: TaskEnvironment -> ExceptT String IO ()
katHandle e@TaskEnvironment {..} = do
  case choices of
    Nothing -> send e $ R.CreateMessage source "Hey! You have to specify some things!"
    Just choices' -> do
      index <- liftIO $ randomRIO (0, length choices' - 1)
      choice <- justify $ T.pack $ choices' !! index
      send e $ R.CreateMessage source choice
  pure ()
  where
    source = messageChannel teMessage
    choices = commandArgs teCommand
      & T.unwords
      & T.unpack
      & readP_to_S parser
      & last
      & fst
      & nonempty

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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Monad                (when)
import Control.Monad.Trans.Except   (ExceptT, except, runExceptT)
import Data.Char                    (isSpace)
import Data.Either                  (isLeft)
import Data.Function                ((&))
import Data.Text                    (Text)
import System.Environment           (getEnvironment)
import Text.ParserCombinators.ReadP
    (ReadP, choice, many, readP_to_S, satisfy, sepBy, skipSpaces, string)

import Discord
import Discord.Types

import           Homebot.Common   (Command (..), TaskEnvironment (..), send)
import qualified Homebot.Palette  as Palette
import qualified Homebot.Pronouns as Pronouns
import qualified Homebot.Decide   as Decide

import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import qualified Discord.Requests as R

botPrefix :: String
botPrefix = "+"

type Handler = TaskEnvironment -> ExceptT String IO ()

commands :: [(Text, Handler)]
commands =
  [ ("help", help)
  , ("ping", ping)
  , ("source", source)
  , (Palette.command, Palette.handle)
  , (Pronouns.command, Pronouns.handle)
  , (Decide.command, Decide.handle)
  ]

main :: IO ()
main = do
  botToken <- lookup "DISCORD_BOT_TOKEN" <$> getEnvironment

  discordError <- runDiscord $ def
    { discordToken = maybe (error "No token!") (T.pack . addPrefix) botToken
    , discordOnEvent = handler
    }

  T.putStrLn discordError

addPrefix :: String -> String
addPrefix = ("Bot " <>)

handler :: DiscordHandle -> Event -> IO ()
handler discord event = case event of
  Ready {} ->
    sendCommand discord $ UpdateStatus $ UpdateStatusOpts
      { updateStatusOptsSince = Nothing
      , updateStatusOptsGame  = Just h_meActivity
      , updateStatusOptsNewStatus  = UpdateStatusOnline
      , updateStatusOptsAFK = False
      }
  MessageCreate m -> do
    res <- runExceptT $
      case parseCommand m of
        Right cmd -> runCommand $ TaskEnvironment discord m cmd
        Left  _   -> except $ Right ()
    when (isLeft res) $ print res
  _               -> pure ()

h_meActivity :: Activity
h_meActivity = Activity
  { activityName = "H+ME"
  , activityType = ActivityTypeGame
  , activityUrl = Just "https://twitter.com/PlayH_ME"
  }

fromSelf :: Message -> Bool
fromSelf = userIsBot . messageAuthor

parseCommand :: Message -> Either String Command
parseCommand = listToEither . readP_to_S commandParser . T.unpack . messageText
  where listToEither [] = Left "No command could be parsed!"
        listToEither xs = Right $ fst $ last xs

commandParser :: ReadP Command
commandParser = do
  string botPrefix
  name <- choice $ map (string . T.unpack . fst) commands
  skipSpaces
  args <- many anyChar `sepBy` satisfy isSpace
  pure $ Command
    { commandName = T.pack name
    , commandArgs = map T.pack args
    }

help :: Handler
help e@TaskEnvironment {..} = do
  send e $ R.CreateMessage origin "GR33T1NGS HUM4N. 1 4M H3XB0T 0x1."
  mconcat ["[", T.intercalate ", " (map fst commands), "]"]
    & R.CreateMessage origin
    & send e
  where origin = messageChannel teMessage

ping :: Handler
ping e@TaskEnvironment {..} =
  send e $ R.CreateMessage (messageChannel teMessage) "P0NG."

source :: Handler
source e@TaskEnvironment {..} =
  send e $ R.CreateMessage (messageChannel teMessage) "https://github.com/monad/h-mebot"

runCommand :: TaskEnvironment -> ExceptT String IO ()
runCommand e@TaskEnvironment {..} =
  lookup (commandName teCommand) commands
    & maybe (pure ()) ($ e)

anyChar :: ReadP Char
anyChar = satisfy (not . isSpace)

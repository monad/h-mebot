{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Monad                (when)
import Control.Monad.Trans.Except   (ExceptT, except, runExceptT)
import Data.Char                    (isSpace)
import Data.Either                  (isLeft)
import System.Environment           (getEnvironment)
import Text.ParserCombinators.ReadP
    (ReadP, choice, many, readP_to_S, satisfy, sepBy, skipSpaces, string)

import Discord
import Discord.Types

import           Homebot.Common   (Command (..), TaskEnvironment (..), send)
import qualified Homebot.Pronouns as Pronouns

import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import qualified Discord.Requests as R

botPrefix :: String
botPrefix = "+"

commandNames :: [String]
commandNames = ["help", "ping", "source", "pronouns"]

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
  name <- choice $ map string commandNames
  skipSpaces
  args <- many anyChar `sepBy` satisfy isSpace
  pure $ Command
    { commandName = T.pack name
    , commandArgs = map T.pack args
    }

runCommand :: TaskEnvironment -> ExceptT String IO ()
runCommand e@TaskEnvironment {..} =
  let origin = messageChannel teMessage in
    case commandName teCommand of
      "help"   -> do
        send e $ R.CreateMessage origin "Here's the commands I know!"
        send e $ R.CreateMessage origin "ping - Confirm whether I'm active."
        send e $ R.CreateMessage origin "source - Link to my source code!"
        send e $ R.CreateMessage origin "pronouns ... - Update your pronouns list!"
      "ping"   ->
        send e $ R.CreateMessage origin "pong!"
      "source" ->
        send e $ R.CreateMessage origin "Find my source at https://github.com/monad/h-mebot"
      "pronouns" -> Pronouns.handle e
      _       -> pure ()

anyChar :: ReadP Char
anyChar = satisfy (not . isSpace)

satisfies :: a -> [a -> Bool] -> Bool
satisfies n preds = all (== True) $ map ($ n) preds

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.DeepSeq              (NFData)
import Control.Arrow                ((&&&))
import Control.Monad                (forM_, void, when)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Class    (lift)
import Control.Monad.Trans.Except   (ExceptT, except, runExceptT)
import Data.Char                    (isSpace)
import Data.Either                  (fromRight, isLeft)
import Data.Function                ((&))
import Data.List                    (isPrefixOf)
import GHC.Generics                 (Generic)
import System.Environment           (getEnvironment)
import Text.ParserCombinators.ReadP (ReadP, satisfy, sepBy, many, string, choice, skipSpaces, readP_to_S)

import Discord
import Discord.Types

import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Discord.Requests             as R

data Command = Command
  { commandName :: T.Text
  , commandArgs :: [T.Text]
  }
  deriving (Eq, Show, Generic, NFData)

data TaskEnvironment = TaskEnvironment
  { teHandle  :: DiscordHandle
  , teMessage :: Message
  , teCommand :: Command
  }

botPrefix :: String
botPrefix = "+"

commandNames :: [String]
commandNames = ["help", "ping", "source", "pronouns"]

pronounRoles :: [T.Text]
pronounRoles = ["they/them", "he/him", "she/her", "he/it",  "any/pronouns"]

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
    res <- runExceptT $ do
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
runCommand TaskEnvironment {..} =
  case commandName teCommand of
    "help"   -> do
      send $ R.CreateMessage origin "Here's the commands I know!"
      send $ R.CreateMessage origin "ping - Confirm whether I'm active."
      send $ R.CreateMessage origin "source - Link to my source code!"
      send $ R.CreateMessage origin "pronouns ... - Update your pronouns list!"
    "ping"   ->
      send $ R.CreateMessage origin "pong!"
    "source" ->
      send $ R.CreateMessage origin "Find my source at https://github.com/monad/h-mebot"
    "pronouns" ->
      case messageGuild teMessage of
        Nothing -> send $ R.CreateMessage origin "Please do this in a server instead."
        Just guildId -> do
          guildRoles <- lift $ fromRight [] <$> restCall teHandle (R.GetGuildRoles guildId)
          -- a zip of role name -> role id
          let roles = map (roleName &&& roleId) guildRoles
          -- filter the command arguments to only existing, whitelisted role names
          let validRoleArgs = map T.toLower (commandArgs teCommand)
                                & filter (`elem` map fst roles)
                                & filter (`elem` pronounRoles)
          let authorId = userId $ messageAuthor teMessage

          forM_ pronounRoles $ \role ->
            case lookup role roles of
              Just roleId -> liftIO $ restCall teHandle $
                R.RemoveGuildMemberRole guildId authorId roleId
              Nothing -> pure $ Right ()

          forM_ validRoleArgs $ \role ->
            case lookup role roles of
              Just roleId -> liftIO $ restCall teHandle $
                R.AddGuildMemberRole guildId authorId roleId
              Nothing -> pure $ Right ()
          pure ()
    _       -> pure ()

  where
    send r = except =<< do
      result <- liftIO $ restCall teHandle r
      case result of
        Left err -> pure $ Left $ show err
        Right _  -> pure $ Right ()
    origin = messageChannel teMessage

anyChar :: ReadP Char
anyChar = satisfy (not . isSpace)

satisfies :: a -> [a -> Bool] -> Bool
satisfies n preds = all (== True) $ map ($ n) preds

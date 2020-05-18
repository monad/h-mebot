{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Homebot.Pronouns (pronounRoles, handle) where

import Control.Arrow              ((&&&))
import Control.Monad              (forM_)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except
import Data.Either                (fromRight)
import Data.Function              ((&))
import Data.Text                  (Text)
import Homebot.Common             (TaskEnvironment (..))

import qualified Data.Text        as T
import qualified Discord.Requests as R

import Homebot.Common (Command (..), send)

import Discord
import Discord.Types

pronounRoles :: [Text]
pronounRoles = ["they/them", "he/him", "she/her", "he/it",  "any/pronouns"]

handle :: TaskEnvironment -> ExceptT String IO ()
handle e@TaskEnvironment {..} =
  case messageGuild teMessage of
    Nothing -> send e $ R.CreateMessage (messageChannel teMessage) "Please do this in a server instead."
    Just guildId -> do
      guildRoles <- lift $ fromRight [] <$> restCall teHandle (R.GetGuildRoles guildId)
      let roles = map (roleName &&& roleId) guildRoles
      -- filter the command arguments to only existing, whitelisted role names
      let validRoleArgs = map T.toLower (commandArgs teCommand)
                          & filter (`elem` map fst roles)
                          & filter (`elem` pronounRoles)
      let authorId = userId $ messageAuthor teMessage

      forM_ pronounRoles $ \role ->
        case lookup role roles of
          Just roleId -> liftIO $ restCall teHandle $ R.RemoveGuildMemberRole guildId authorId roleId
          Nothing -> pure $ Right ()

      forM_ validRoleArgs $ \role ->
        case lookup role roles of
          Just roleId -> liftIO $ restCall teHandle $ R.AddGuildMemberRole guildId authorId roleId
          Nothing -> pure $ Right ()
      pure ()

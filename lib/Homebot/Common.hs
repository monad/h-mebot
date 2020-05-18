{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Homebot.Common
  ( TaskEnvironment (..)
  , Command (..)
  , origin
  , send )
where

import Control.DeepSeq            (NFData)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT, except)
import Data.Text                  (Text)
import GHC.Generics               (Generic)

import Discord       (DiscordHandle, FromJSON, restCall)
import Discord.Types (ChannelId, Message, messageChannel)

import qualified Discord.Requests as R

data Command = Command
  { commandName :: Text
  , commandArgs :: [Text]
  }
  deriving (Eq, Show, Generic, NFData)

data TaskEnvironment = TaskEnvironment
  { teHandle  :: DiscordHandle
  , teMessage :: Message
  , teCommand :: Command
  }

send :: (FromJSON a, MonadIO m) => TaskEnvironment -> R.ChannelRequest a -> ExceptT String m ()
send TaskEnvironment {..} r = except =<< do
  result <- liftIO $ restCall teHandle r
  case result of
    Left err -> pure $ Left $ show err
    Right _  -> pure $ Right ()

origin :: TaskEnvironment -> ChannelId
origin = messageChannel . teMessage

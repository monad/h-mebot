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
import Control.Monad.IO.Class     (MonadIO)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT, except)
import Data.Text                  (Text)
import GHC.Generics               (Generic)

import Discord       (DiscordHandler, FromJSON, restCall)
import Discord.Types (ChannelId, Message, messageChannel)

import qualified Discord.Requests as R

data Command = Command
  { commandName :: Text
  , commandArgs :: [Text]
  }
  deriving (Eq, Show, Generic, NFData)

data TaskEnvironment = TaskEnvironment
  { teMessage :: Message
  , teCommand :: Command
  }

send :: (FromJSON a) => TaskEnvironment -> R.ChannelRequest a -> ExceptT String DiscordHandler ()
send TaskEnvironment {..} r = except =<< do
  result <- lift $ restCall r
  case result of
    Left err -> pure $ Left $ show err
    Right _  -> pure $ Right ()

origin :: TaskEnvironment -> ChannelId
origin = messageChannel . teMessage

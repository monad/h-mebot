{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Homebot.Palette (handle, command) where

import Codec.Picture              (encodePng)
import Control.Monad.Trans.Except (ExceptT)
import Data.ByteString            (ByteString)
import Data.ByteString.Lazy       (toStrict)
import Data.Text                  (Text)
import Discord.Types
    (CreateEmbed (..), CreateEmbedImage (..), messageChannel)
import Homebot.Common             (TaskEnvironment (..), send)

import Diagrams.Backend.Rasterific
import Diagrams.Prelude

import qualified Discord.Requests as R

command :: Text
command = "palette"

basicAssSquare :: Diagram Rasterific
basicAssSquare = square 5 # fc red

rendered :: Diagram B -> ByteString
rendered = toStrict . encodePng . renderDia Rasterific opts
  where opts = RasterificOptions { _sizeSpec = dims (V2 50 50) }

handle :: TaskEnvironment -> ExceptT String IO ()
handle e@TaskEnvironment {..} =
  send e $ R.CreateMessageEmbed (messageChannel teMessage) "" $
    def { createEmbedImage = Just $ CreateEmbedImageUpload $ rendered basicAssSquare }

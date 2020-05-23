{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Homebot.Palette (handle, rendered, basicAssSquare) where

import Codec.Picture (encodePng)
import Control.Monad.Trans.Except (ExceptT)
import Homebot.Common (TaskEnvironment (..), send)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Discord.Types (CreateEmbed (..), CreateEmbedImage (..), messageChannel)

import Diagrams.Prelude
import Diagrams.Backend.Rasterific

import qualified Discord.Requests as R

basicAssSquare :: Diagram Rasterific
basicAssSquare = square 5 # fc red

rendered :: Diagram B -> ByteString
rendered = toStrict . encodePng . renderDia Rasterific opts
  where opts = RasterificOptions { _sizeSpec = dims (V2 50 50) }

handle :: TaskEnvironment -> ExceptT String IO ()
handle e@TaskEnvironment {..} = do
  send e $ R.CreateMessageEmbed (messageChannel teMessage) "" $ def { createEmbedImage = Just $ CreateEmbedImageUpload $ rendered basicAssSquare }

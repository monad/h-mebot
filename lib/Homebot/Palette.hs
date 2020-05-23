{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Homebot.Palette (handle, command, colour) where

import Codec.Picture              (encodePng)
import Control.Monad              (replicateM)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.ByteString            (ByteString)
import Data.ByteString.Lazy       (toStrict)
import Data.Maybe                 (fromMaybe)
import Data.Text                  (Text, unpack)
import Discord.Types
    (CreateEmbed (..), CreateEmbedImage (..), messageChannel)
import Homebot.Common             (Command (..), TaskEnvironment (..), send)
import System.Random              (randomIO)
import Text.Read                  (readMaybe)

import Diagrams.Backend.Rasterific
import Diagrams.Prelude

import qualified Discord.Requests as R

colour :: IO (Colour Double)
colour = sRGB24 <$> randomIO <*> randomIO <*> randomIO

command :: Text
command = "palette"

swatch :: Colour Double -> Diagram B
swatch = ($ square 1) . lw none . fc

rendered :: Diagram B -> ByteString
rendered = toStrict . encodePng . renderDia Rasterific opts
  where opts = RasterificOptions { _sizeSpec = mkHeight 60 }

parseText :: Text -> Maybe Int
parseText = readMaybe . unpack

handle :: TaskEnvironment -> ExceptT String IO ()
handle e@TaskEnvironment {..} = do
  palette <- liftIO (hcat <$> map swatch <$> replicateM (min 16 count) colour)
  send e $ R.CreateMessageEmbed (messageChannel teMessage) "" $
    def { createEmbedImage = Just $ CreateEmbedImageUpload $ rendered palette }
  where count = fromMaybe 5 $ parseText =<< headMay (commandArgs teCommand)

headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

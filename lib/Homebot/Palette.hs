{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Homebot.Palette (handle, flagHandle, command, flagCommand, colour) where

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

import Discord (DiscordHandler)

import Diagrams.Backend.Rasterific
import Diagrams.Prelude

import qualified Discord.Requests as R

data Mode = Palette | Flag

colour :: IO (Colour Double)
colour = sRGB24 <$> randomIO <*> randomIO <*> randomIO

command :: Text
command = "palette"

flagCommand :: Text
flagCommand = "flag"

swatch :: Colour Double -> Diagram B
swatch = ($ square 1) . lw none . fc

stripe :: Colour Double -> Diagram B
stripe = ($ rect 6 1) . lw none . fc

rendered :: Mode -> Diagram B -> ByteString
rendered Palette = toStrict . encodePng . renderDia Rasterific RasterificOptions { _sizeSpec = mkHeight 60 }
rendered Flag    = toStrict . encodePng . renderDia Rasterific RasterificOptions { _sizeSpec = mkWidth 120 }

parseText :: Text -> Maybe Int
parseText = readMaybe . unpack

handle :: TaskEnvironment -> ExceptT String DiscordHandler ()
handle e@TaskEnvironment {..} = do
  palette <- liftIO (hcat . map swatch <$> replicateM (min 16 count) colour)
  send e $ R.CreateMessageEmbed (messageChannel teMessage) "" $
    def { createEmbedImage = Just $ CreateEmbedImageUpload $ rendered Palette palette }
  where count = fromMaybe 5 $ parseText =<< headMay (commandArgs teCommand)

-- "flag mode" the same but vertically stacked rectangles
flagHandle :: TaskEnvironment -> ExceptT String DiscordHandler ()
flagHandle e@TaskEnvironment {..} = do
  stripes <- liftIO (vcat . map stripe <$> replicateM (min 6 count) colour)
  send e $ R.CreateMessageEmbed (messageChannel teMessage) "" $
    def { createEmbedImage = Just $ CreateEmbedImageUpload $ rendered Flag stripes }
  where count = fromMaybe 3 $ parseText =<< headMay (commandArgs teCommand)

headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

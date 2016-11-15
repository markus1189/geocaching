module Geocaching.Crypto (letterSum, letterValue) where

import           Data.Char (ord, toLower)
import           Data.Text (Text)
import qualified Data.Text as T

letterSum :: Text -> Int
letterSum = T.foldl' (\acc x -> acc + letterValue x) 0

letterValue :: Char -> Int
letterValue c = ord (toLower c) - 96

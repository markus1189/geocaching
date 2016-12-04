{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Geocaching.Types (Coordinate(..)
                        ,Latitude(..)
                        ,Longitude(..)
                        ,Geocache(..)
                        ,Url(..)
                        ,CheckerCode(..)
                        ,parseCoord
                        ,fmtLat
                        ,fmtLon) where

import           Data.Either.Combinators (mapLeft)
import           Data.Text (Text)
import qualified Data.Text as T
import           Formatting
import qualified Text.Megaparsec as P

data Latitude = Lat Int Int Int deriving (Show,Eq)
data Longitude = Lon Int Int Int deriving (Show,Eq)

data Coordinate = Coord { coordLat :: Latitude
                        , coordLon :: Longitude
                        } deriving (Show,Eq)

fmtLat :: Format r (Latitude -> r)
fmtLat = later go
  where go (Lat a b c) = bprint ("N " % int % " " % int % "." % int) a b c

fmtLon :: Format r (Longitude -> r)
fmtLon = later go
  where go (Lon a b c) = bprint ("E " % int % " " % int % "." % int) a b c

newtype Geocache = Geocache Text

newtype Url = Url String
newtype CheckerCode = CheckerCode Text

parseCoord :: String -> Either String Coordinate
parseCoord input = undefined

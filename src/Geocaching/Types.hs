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
import qualified Text.Parsec as P
import           Text.Parsec.Text

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
parseCoord input = mapLeft show $ P.runParser p () "<none>" (T.pack input)
  where p = Coord <$> parseGen 'N' Lat <*> (P.spaces *> P.optional (P.char ',') *> P.spaces *> parseGen 'E' Lon)

parseGen :: Char -> (Int -> Int -> Int -> a) -> Parser a
parseGen c mk = do
  _ <- P.char c
  _ <- P.spaces
  d1 <- read <$> P.many P.digit
  _ <- P.spaces
  d2 <- read <$> P.many P.digit
  _ <- P.char '.'
  d3 <- read <$> P.many P.digit
  return $ mk d1 d2 d3

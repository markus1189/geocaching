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
parseCoord input = mapLeft show $ P.runParser parser () "<none>" (T.pack input)
  where parser = Coord
             <$> parseGen P.digit 'N' Lat
             <*> (pSkip *> parseGen P.digit 'E' Lon)
        pSkip = P.spaces *> P.optional (P.char ',') *> P.spaces

parseGen :: Parser Char -> Char -> (Int -> Int -> Int -> a) -> Parser a
parseGen pDigit c mk = do
  _ <- P.char c
  _ <- P.spaces
  d1 <- read <$> P.many pDigit
  _ <- P.spaces
  d2 <- read <$> P.count 2 pDigit
  _ <- P.char '.'
  d3 <- read <$> P.count 3 pDigit
  return $ mk d1 d2 d3

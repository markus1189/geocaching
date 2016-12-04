{-# LANGUAGE OverloadedStrings #-}
module Geocaching.Parse (displayFile) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.Conduit (($$), ConduitM)
import Data.Function (on)
import Data.Text (Text)
import Data.XML.Types (Event, Name(..))
import Text.XML.Stream.Parse
import Debug.Trace (traceShowM)

data Gpx = Gpx { _gpxMeta :: GpxMeta
               , _gpxWpts :: [Waypoint]
               } deriving Show

data GpxMeta = GpxMeta { _metaName :: Text
                       , _metaDesc :: Text
                       , _metaAuthor :: Text
                       , _metaEmail :: Text
                       } deriving Show

data Waypoint = Waypoint { _wptLat :: Text
                         , _wptLon :: Text
                         } deriving Show

gpx :: MonadThrow m => ConduitM Event a m (Maybe Gpx)
gpx = tagPredicateIgnoreAttrs p $ do
      metadata <- parseMetadata
      wpts <- many waypoint
      return (Gpx metadata wpts)
  where p (Name local _ _) = local == "gpx"

parseMetadata :: MonadThrow m => ConduitM Event a m GpxMeta
parseMetadata = do
  name <- requireContent "name"
  desc <- requireContent "desc"
  author <- requireContent "author"
  email <- requireContent "email"
  _ <- requireContent "time"
  _ <- requireContent "keywords"
  _ <- requireContent "bounds"
  return (GpxMeta name desc author email)

waypoint :: MonadThrow m => ConduitM Event a m (Maybe Waypoint)
waypoint = tagPredicate (p "wpt") ((,) <$> requireAttr "lat" <*> requireAttr "lon") $ \(lat,lon) -> do
  _ <- requireContent "time"
  _ <- requireContent "name"
  _ <- requireContent "desc"
  _ <- requireContent "url"
  _ <- requireContent "urlname"
  _ <- requireContent "sym"
  _ <- requireContent "type"
  _ <- ignoreTree (p "cache")
  return $ Waypoint lat lon
  where p = (==) `on` nameLocalName

displayFile :: FilePath -> IO ()
displayFile fp = runResourceT $ do
  caches <- parseFile def fp $$ force "Could not parse the gpx file." gpx
  liftIO (print caches)

requireContent :: MonadThrow m => Name -> ConduitM Event a m Text
requireContent n = force ("Tag not found: " ++ show n) $ tagPredicateIgnoreAttrs (p n) $ content
  where p = (==) `on` nameLocalName

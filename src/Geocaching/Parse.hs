{-# LANGUAGE OverloadedStrings #-}
module Geocaching.Parse (displayFile) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.Conduit (($$), ConduitM)
import Data.Function (on)
import Data.Text (Text, unpack)
import Data.XML.Types (Event, Name(..))
import Debug.Trace (trace)
import Text.XML.Stream.Parse

data Gpx = Gpx { _gpxMeta :: GpxMeta
               , _gpxWpts :: [Waypoint]
               } deriving Show

data GpxMeta = GpxMeta { _metaName :: Text
                       , _metaDesc :: Text
                       , _metaAuthor :: Text
                       , _metaEmail :: Text
                       } deriving Show

data Waypoint = Waypoint { wptLat :: Text
                         , wptLon :: Text
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
  time <- requireContent "time"
  keyword <- requireContent "keywords"
  bounds <- requireContent "bounds"
  return (GpxMeta name desc author email)

metaAttrs :: AttrParser GpxMeta
metaAttrs = do
  name <- requireAttr "name"
  desc <- requireAttr "desc"
  author <- requireAttr "author"
  email <- requireAttr "email"
  return (GpxMeta name desc author email)

waypoint :: MonadThrow m => ConduitM Event a m (Maybe Waypoint)
waypoint = tagName "wpt" ((,) <$> requireAttr "lat" <*> requireAttr "lon") $ \(lat,lon) ->
  return $ Waypoint lat lon

displayFile :: FilePath -> IO ()
displayFile fp = runResourceT $ do
  caches <- parseFile def fp $$ force "Could not parse the gpx file." gpx
  liftIO (print caches)

requireContent n = force ("Tag not found: " ++ show n) $ tagPredicateIgnoreAttrs (p n) content
  where p = (==) `on` nameLocalName

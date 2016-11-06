{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens (view)
import           Control.Lens.Operators
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Foldable (for_)
import           Data.List (isInfixOf)
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Formatting
import           Network.Wreq
import qualified Network.Wreq.Session as S
import qualified Text.HTML.TagSoup as TS

data Latitude = Lat Int Int Int deriving (Show,Eq)
data Longitude = Lon Int Int Int deriving (Show,Eq)

newtype Geocache = Geocache Text

newtype Url = Url String
newtype CheckerCode = CheckerCode Text

data Coordinate = Coord { coordLat :: Latitude
                        , coordLon :: Longitude
                        } deriving (Show,Eq)

main :: IO ()
main = S.withSession $ \sess -> do
  let cache = Geocache "GC"
      coord = Coord (Lat 50 2 1) (Lon 8 2 1)
  checkerLink <- checker sess cache <&> listToMaybe
  for_ checkerLink $ \link -> do
    let checkerCode = CheckerCode (T.pack (urlParams link Map.! "code"))
    r <- checkCache sess cache checkerCode coord
    print r

urlParams :: Url -> Map String String
urlParams (Url url) =
  Map.fromList $ map (firstTwo . splitOn "=") $ splitOn "&" $ drop 1 $ dropWhile (/= '?') url
  where firstTwo [x,y] = (x,y)
        firstTwo _ = error "Unexpected :("

fmtLat :: Format r (Latitude -> r)
fmtLat = later go
  where go (Lat a b c) = bprint ("N " % int % " " % int % "." % int) a b c

fmtLon :: Format r (Longitude -> r)
fmtLon = later go
  where go (Lon a b c) = bprint ("E " % int % " " % int % "." % int) a b c

checkCache :: S.Session -> Geocache -> CheckerCode -> Coordinate -> IO (Response BS.ByteString)
checkCache sess (Geocache cache) (CheckerCode code) (Coord lat lon) =
  S.post sess  "http://www.geochecker.com/index.php" ["LatString" := sformat fmtLat lat
                                                     ,"LonString" := sformat fmtLon lon
                                                     ,"code" := code
                                                     ,"action" := ("confirm" :: Text)
                                                     ,"wp" := cache
                                                     ]

checker :: S.Session -> Geocache -> IO [Url]
checker sess (Geocache cache) = do
  body <- S.get sess url <&> view responseBody <&> TS.parseTags
  let links = filter ("geochecker.com" `isInfixOf`) . map BS.unpack $ map (TS.fromAttrib "href") $ filter (TS.isTagOpenName "a") body
  return (map Url links)
  where url = formatToString (string % stext) "https://coord.info/" cache

{-# LANGUAGE OverloadedStrings #-}
module Geocaching.Checker (checkSolution) where

import           Control.Lens (view)
import           Control.Lens.Operators
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List (isInfixOf)
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Formatting
import           Geocaching.Types
import           Network.Wreq
import qualified Network.Wreq.Session as S
import qualified Text.HTML.TagSoup as TS

checkSolution :: S.Session -> Geocache -> Coordinate -> IO ()
checkSolution sess cache coord = do
  checkerLink <- checker sess cache <&> listToMaybe
  case checkerLink of
    Nothing -> putStrLn "No geochecker found."
    Just link -> do
      let checkerCode = CheckerCode (T.pack (urlParams link Map.! "code"))
      r <- checkCache sess cache checkerCode coord
      print r

urlParams :: Url -> Map String String
urlParams (Url url) =
  Map.fromList $ map (firstTwo . splitOn "=") $ splitOn "&" $ drop 1 $ dropWhile (/= '?') url
  where firstTwo [x,y] = (x,y)
        firstTwo _ = error "Unexpected :("

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

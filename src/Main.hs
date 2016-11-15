{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import           Geocaching.Crypto
import           System.Environment (getArgs)

-- main :: IO ()
-- main = S.withSession $ \sess -> do
--   let cache = Geocache "GC"
--       coord = Coord (Lat 50 2 1) (Lon 8 2 1)
--   checkSolution sess cache coord

main :: IO ()
main = do
  args <- getArgs
  print . letterSum $ T.pack (head args)

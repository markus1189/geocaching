{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Geocaching.Crypto
import           Options.Applicative

-- main :: IO ()
-- main = S.withSession $ \sess -> do
--   let cache = Geocache "GC"
--       coord = Coord (Lat 50 2 1) (Lon 8 2 1)
--   checkSolution sess cache coord

data Command = LetterSum Text

opts :: Parser Command
opts = subparser (command "lsum" (info (helper <*> lsum) (fullDesc
                                                       <> progDesc "Calculate the letter sum"
                                                       <> header "geocaching - the geocaching toolbox")))

txt :: ReadM Text
txt = T.pack <$> str

lsum :: Parser Command
lsum = LetterSum <$> argument txt (metavar "INPUT")

execute :: Command -> IO ()
execute (LetterSum input) = do
  T.putStrLn $ "The letter sum for '" <> input <> "' is:"
  print (letterSum input)

main :: IO ()
main = do
  cmd <- execParser (info opts idm)
  execute cmd

{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Geocaching.Checker (checkSolution)
import           Geocaching.Crypto (letterSum)
import           Geocaching.Formula
import           Geocaching.Types
import qualified Network.Wreq.Session as S
import           Options.Applicative

data Command = LetterSum Text
             | CheckCoords Geocache Coordinate
             | Formula Text


main :: IO ()
main = execParser (info (helper <*> opts) idm) >>= execute

execute :: Command -> IO ()
execute (LetterSum input) = do
  T.putStrLn $ "The letter sum for '" <> input <> "' is:"
  print (letterSum input)
execute (CheckCoords cache coord) = S.withSession $ \sess ->
  checkSolution sess cache coord
execute (Formula inp) = undefined

lsum :: Parser Command
lsum = LetterSum <$> argument txt (metavar "INPUT")

check :: Parser Command
check = CheckCoords
    <$> (Geocache <$> argument txt (metavar "GC"))
    <*> argument (eitherReader parseCoord) (metavar "COORDINATE")

txt :: ReadM Text
txt = T.pack <$> str

opts :: Parser Command
opts = subparser (command "lsum" (info (helper <*> lsum)
                                       (fullDesc
                                     <> progDesc "Calculate the letter sum"))
              <> command "check" (info (helper <*> check)
                                       (fullDesc
                                     <> progDesc checkDesc))
                )
  where checkDesc = "Check whether the given coordinates are correct using the checker from the cache listing"

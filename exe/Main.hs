module Main where

import Childes
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
  [dir'] <- getArgs
  let dir = if "/" `isSuffixOf` dir' then dir' else dir' ++ "/"
  files <- getDirectoryContents dir
  csvs <- forM (sort $ filter (".xml" `isSuffixOf`) files) $ \f ->
    parseTranscript $ dir ++ f
  BS.writeFile "output.csv" $ toCSV csvs

module Main where

import Childes
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import Data.Csv

main :: IO ()
main = do
  [dir] <- getArgs
  files <- getDirectoryContents dir
  forM_ (filter (".xml" `isSuffixOf`) files) $ \f -> do
    putStrLn $ "File:\t" ++ (dir ++ f)
    prettyIsh =<< allStats (dir ++ f)
    putStrLn $ "\n"

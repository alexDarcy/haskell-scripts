{-# LANGUAGE OverloadedStrings #-}
-- Example of a parser with attoparsec

module Main where

import JobParser
import qualified Data.Text.IO as TIO

main = do
  all <- parseData "test.txt"
  printCSV "output.csv" all
  printLATEX "output.tex" all

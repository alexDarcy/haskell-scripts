{-# LANGUAGE OverloadedStrings #-}
-- Example of a parser with attoparsec

module Main where

import JobParser
import qualified Data.Text.IO as TIO

main = do
  output <- parseData "test.txt"
  TIO.writeFile "output.csv" output

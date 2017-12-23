module Main where

import Control.Monad
import System.FilePath
import System.FilePath.Find

root = "/Data/Music/Classical/"

-- Count tracks only in the current folder
hasOneTrack :: FilePath -> IO Bool
hasOneTrack dir = do 
  tracks <- find (depth ==? 0) (extension ==? ".flac") dir
  return $ length tracks == 1

main = do
  -- Find all directories containing cue files 
  cueFiles <-  find always (extension ==? ".cue") root 
  let dirs = map takeDirectory cueFiles
  -- And with only one file (the whole cd)
  dirs' <- filterM hasOneTrack dirs
  print dirs'

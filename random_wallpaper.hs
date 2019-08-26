#!/usr/bin/env stack
{- stack --resolver lts-12.9 script -}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import qualified Data.Text as T
import Shelly
import System.Console.CmdArgs
import System.Directory
import System.Random

-- | Set a random wallpaper in a directory using feh
-- Usage : 
--    random_wallpaper DIR
--
-- Requirements : feh

data Wallpaper = Wallpaper { dir :: T.Text }
              deriving (Show, Data, Typeable)

instance Default T.Text where def = ""

wall = Wallpaper {dir = def &= argPos 0 &= typFile } 
        &= summary "Set a random wallaper" 

main = shelly $ do
  args <- liftIO $ cmdArgs wall
  pics <- lsT . fromText $  dir args
  g <- liftIO newStdGen
  let (i, _) = randomR (0, length pics-1) g :: (Int, StdGen)
  run "feh" ["--bg-max", pics !! i]

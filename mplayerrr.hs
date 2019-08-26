#!/usr/bin/env stack
{- stack --resolver lts-12.9 script -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveDataTypeable #-}


-- Run mplayer without the mouse cursor

module Main where

import Prelude hiding (FilePath)
import Shelly
import System.Console.CmdArgs
import qualified Data.Text as T
default (T.Text)

data MplayerRR = MplayerRR { file :: T.Text }
              deriving (Show, Data, Typeable)

mplayerrr = MplayerRR  { file = "" &= args &= typFile }

main :: IO()
main = shelly $ do
    args <- liftIO $ cmdArgs mplayerrr
    run "setterm" ["-cursor", "off"]
    run "mplayer" [file args]
    run "setterm" ["-cursor", "on"]
    echo "done"

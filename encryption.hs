{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Shelly
import Control.Applicative
import Data.String
import Data.Foldable
import qualified Data.Text as T
default (T.Text)


-- | Symmetric encryption
encrypt :: String -> Sh()
encrypt x = run_ "gpg" ["-o", T.pack (x ++ ".gpg")
                       , "--passphrase-file", "~/.passphrase_files"
                       , "-c", T.pack x]

-- | Symmetric decryption
decrypt :: String -> Sh()
decrypt x = run_ "gpg" ["-o", T.pack x
                       , "--passphrase-file", "~/.passphrase_files"
                       , "-d", T.pack $ x ++ ".gpg"]

-- | Apply an encryption/decryption function to a list of files
examine :: (String -> Sh()) -> [String] -> Sh()
examine f files = 
  forM_ files $ \cur -> do
    exists <- (test_f . fromText . T.pack) cur
    if exists then f cur else echo $ T.pack $ cur ++ " do not exist"

-- | Encrypt or decrypt with GPG a list of files
main :: IO()
main = shelly $ verbosely $ do
  let files = [ "mail_config/.fetchmailrc"
              , "mail_config/.msmtprc"
              , "mail_config/.procmailrc"
              , "abook/.abook/addressbook"
              , "irssi/.irssi/config"]
  let paths = map ("~/dotfiles/" ++) files

  -- T.pack converts a string to a text
  examine encrypt paths
  
  echo "Done"

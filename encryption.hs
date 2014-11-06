{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Control.Applicative
import Data.String
import Data.Foldable
import qualified Data.Text as T
default (T.Text)

encrypt :: String -> Sh()
encrypt x = run_ "gpg" ["-o", T.pack (x ++ ".gpg")
                       , "--passphrase-file", "./.passphrase_files"
                       , "-c", T.pack x]

examine :: [String] -> Sh()
examine files = 
  forM_ files $ \cur -> do
    exists <- (test_f . fromText . T.pack) cur
    run_ "echo" ["lol", "kikoo"]
    if exists then encrypt cur else error $ cur++"do not exist"

main = shelly $ verbosely $ do
  let files = [ "mail_config/.fetchmailrc2"
              , "mail_config/.msmtprc"
              , "mail_config/.procmailrc"
              , "abook/.abook/addressbook"
              , "irssi/.irssi/config"]
  let folder = "dotfiles/"
  let paths = map ("dotfiles/" ++) files

  -- T.pack converts a string to a text
  examine paths
  
  echo "lol"

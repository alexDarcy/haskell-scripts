{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Control.Applicative
import Data.String
import qualified Data.Text as T
default (T.Text)

--paths :: Data.String.IsString a => [a] -> [a]
--paths x = map (Data.String.ToString ("~/dotfiles/") ++) x

main = shelly $ verbosely $ do
  let files = [ "mail_config/.fetchmailrc.gpg"
              , "mail_config/.msmtprc"
              , "mail_config/.procmailrc"
              , "abook/.abook/addressbook"
              , "irssi/.irssi/config"]
  let paths = map (("../../dotfiles/") ++) files

  -- T.pack converts a string to a text
  echo (T.pack (head paths))
  a <- (test_f . fromText . T.pack . head) paths
  unless a $ echo "File does not exist"
  echo "lol"

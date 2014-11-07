{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | A script to encrypt or decrypt a hard-coded list of files with GPG using
-- symmetric encyrption.
-- Errors about existing files are managed by GPG.

module Main where

import Prelude hiding (FilePath)
import Shelly
import Control.Applicative
import Data.String
import Data.Foldable
import qualified Data.Text as T
default (T.Text)


-- Tilde does not work
inputDir :: FilePath
inputDir = "../../dotfiles"

getPath :: FilePath -> FilePath
getPath x = inputDir </> x

-- | Generic symmetric encryption with gpg. The function needs the outpu file, a
-- flag ("-c" for encryption, "-d" for decryption) and the input file.
gpg :: FilePath -> FilePath -> FilePath -> Sh()
gpg input flag output = cmd "gpg" ["-o", input
                       , "--passphrase-file", "../../.passphrase_files"
                       , flag, output]

-- | Symmetric encryption from FILE to FILE.gpg.
encrypt :: FilePath -> Sh()
encrypt x = gpg x "-d" (x <.> "gpg")

-- | Symmetric decryption from FILE.gpg to FILE
decrypt :: FilePath -> Sh()
decrypt x = gpg x "-d" (x <.> "gpg")

main :: IO()
main = shelly $ verbosely $ do
  let files = [ "mail_config/.fetchmailrc"
              , "mail_config/.msmtprc"
              , "mail_config/.procmailrc"
              , "abook/.abook/addressbook"
              , "irssi/.irssi/config"]

  echo "Decrypting"
  mapM (decrypt . getPath) files
  echo "Done"

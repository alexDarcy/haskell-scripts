{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A script to encrypt or decrypt a hard-coded list of files with GPG using
-- symmetric encryption. The passphrase must be stored in 
-- ../../.passphrase_files.
-- Errors about existing files are managed by GPG.
-- Usage : 
--    encryption -e 
--    encryption -d 

import Prelude hiding (FilePath)
import Shelly
import Control.Applicative
import Data.String
import System.Console.CmdArgs
import qualified Data.Text as T
default (T.Text)

-- Tilde does not work
inputDir :: FilePath
inputDir = "../../dotfiles"

getPath :: FilePath -> FilePath
getPath x = inputDir </> x

-- | Generic symmetric encryption with gpg. The function needs the outpu file, 
-- a flag ("-c" for encryption, "-d" for decryption) and the input file.
gpg :: FilePath -> FilePath -> FilePath -> Sh()
gpg input flag output = cmd "gpg" ["-o", input
                       , "--passphrase-file", "../../.passphrase_files"
                       , flag, output]

-- | Symmetric encryption from FILE to FILE.gpg.
encryptSym :: FilePath -> Sh()
encryptSym x = gpg (x <.> "gpg") "-c" x

-- | Symmetric decryption from FILE.gpg to FILE
decryptSym :: FilePath -> Sh()
decryptSym x = gpg x "-d" (x <.> "gpg")

data Args = Args{encrypt :: Bool, decrypt :: Bool}
              deriving (Show, Data, Typeable)

arguments = Args{encrypt = def, decrypt = def}

main :: IO()
main = shelly $ do
    let files = [ "mail_config/.fetchmailrc"
                , "mail_config/.msmtprc"
                , "mail_config/.procmailrc"
                , "abook/.abook/addressbook"
                , "irssi/.irssi/config"]

    args <- liftIO $ cmdArgs arguments
    if encrypt args then do
      echo "Encrypting" 
      mapM (encryptSym . getPath) files
    else do
      echo "Decrypting"
      mapM (decryptSym . getPath) files
    -- We need an echo 
    echo "Done"

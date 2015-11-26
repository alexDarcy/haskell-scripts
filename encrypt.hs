{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A script to encrypt or decrypt a hard-coded list of files with GPG using
-- symmetric encryption. The passphrase must be stored in 
-- ~/.passphrase_files.
-- The script must be used in the directory containing the files.
-- Errors about existing files are managed by GPG.
-- Usage : 
--    encryption -c 
--    encryption -d 
--
-- Requirements : gnupg 1

module Main where

import Prelude hiding (FilePath)
import Shelly
import Control.Applicative
import Data.String
import System.Console.CmdArgs
import qualified Data.Text as T
default (T.Text)

homeDir = do
  dir <- get_env "HOME"
  case dir of
    Just home -> return home
    Nothing -> errorExit "Failed to find a $HOME !"

--passFile :: FilePath
passFile = do
  home <- homeDir 
  return (home </> ".passphrase_files")

-- | Generic symmetric encryption with gpg. The function needs the outpu file, 
-- a flag ("-c" for encryption, "-d" for decryption) and the input file.
gpg :: FilePath -> FilePath -> FilePath -> FilePath -> Sh()
gpg input pass flag output = cmd "gpg" args
  where args = ["-o", input, "--batch", "--passphrase-file", pass
               , flag, output]

-- | Symmetric encryption from FILE to FILE.gpg.
encryptSym :: FilePath -> FilePath -> Sh()
encryptSym pass x = gpg (x <.> "gpg") pass "-c" x 

-- | Symmetric decryption from FILE.gpg to FILE
decryptSym :: FilePath -> FilePath -> Sh()
decryptSym pass x = gpg x pass "-d" (x <.> "gpg")

      
data Args = Args{encrypt :: Bool, decrypt :: Bool}
              deriving (Show, Data, Typeable)

arguments = Args{encrypt = def, decrypt = def}

main :: IO()
main = shelly $ do
    let files = [ "mail_config/.offlineimaprc"
                , "mail_config/.msmtprc"
                , "mail_config/.procmailrc"
                , "abook/.abook/addressbook"
--                , "irssi/.irssi/config"
                ]

    args <- liftIO $ cmdArgs arguments

    pass <- passFile
    if encrypt args then do
      echo "Encrypting" 
      mapM (encryptSym pass) files
    else do
      echo "Decrypting"
      mapM (decryptSym pass) files
    echo "Done"

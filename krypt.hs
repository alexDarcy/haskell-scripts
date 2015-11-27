{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Interface to GPG : encrypt or decrypt a list of files with GPG using
-- symmetric encryption. The passphrase must be stored in ~/.passphrase_files.
-- Errors about existing files are managed by GPG.
-- Usage : 
--    krypt [-e|-d] file1 [file2 ... ]
--
-- Requirements : gnupg 2

module Main where

import Prelude hiding (FilePath)
import Shelly
import Control.Applicative
import Data.String
import Filesystem.Path (dropExtension)
import System.Console.CmdArgs
import qualified Data.Text as T
default (T.Text)

homeDir = do
  dir <- get_env "HOME"
  case dir of
    Just home -> return home
    Nothing -> errorExit "Failed to find a $HOME !"

--dropExt x = dropExtension x

passphrase = do
  home <- homeDir 
  return (home </> ".passphrase_files")

-- | Generic symmetric encryption with gpg. The function needs the outpu file, 
-- a flag ("-e" for encryption, "-d" for decryption) and the input file.
gpg :: FilePath -> FilePath -> FilePath -> FilePath -> Sh()
gpg pass flag input output = cmd "gpg" args
  where args = ["-o", output, "--batch", "--passphrase-file", pass
               , flag, input]

-- | Symmetric encryption from FILE to FILE.gpg.
encryptSym :: FilePath -> FilePath -> Sh()
encryptSym pass x = gpg pass "-c" x (x <.> ".gpg")

-- | Symmetric decryption from FILE.gpg to FILE
decryptSym :: FilePath -> FilePath -> Sh()
decryptSym pass x = gpg pass"-d" x (dropExtension x) 

      
data Krypt = Krypt{ encrypt :: Bool
                  , decrypt :: Bool
         --         , pass :: T.Text
                  , src :: [T.Text]
                  }
              deriving (Show, Data, Typeable)

passFile :: T.Text
passFile = "~/.passphrase_files"

--passphrase = fromText passFile

krypt = Krypt
        { encrypt = def &= name "encrypt" 
        , decrypt = def 
        --, pass = passFile &= name "passphrase" &= help "passphrase file (default=~/.passphrase_files)"
        , src = def &= args &= typFile 
        } &= 
        summary "Wrapper around GPG" &=
        help "Encrypts or decrypt a list of files using a passphrase"

files x = map fromText ( src x )

main :: IO()
main = shelly $ do
    args <- liftIO $ cmdArgs krypt
    pass <- passphrase
    --echo $ T.pack $ show $ src args
    case () of _
                | encrypt args -> do
                    echo "Encrypting" 
                    mapM_ (encryptSym pass) (files args)
                | decrypt args -> do
                    echo "Decrypting" 
                    mapM_ (decryptSym pass) (files args)
                | otherwise -> 
                    echo "Nothing to do"
    echo "Done"

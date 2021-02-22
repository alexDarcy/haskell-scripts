-- module Main where

-- import Data.Char
-- import Network.HTTP
-- import Text.HTML.TagSoup

-- openURL :: String -> IO String
-- openURL x = getResponseBody =<< simpleHTTP (getRequest x)

-- haskellLastModifiedDateTime :: IO ()
-- haskellLastModifiedDateTime = do
--     src <- openURL "http://www.reddit.com/r/AskHistorians/wiki/books"
--     writeFile "tmp.html" src
--     -- print $ parseTags src
--     -- let lastModifiedDateTime = fromFooter $ parseTags src
--     -- putStrLn $ "wiki.haskell.org was last modified on " ++ lastModifiedDateTime
--     -- where fromFooter = unwords . drop 6 . words . innerText . take 2 . dropWhile (~/= "<li id=lastmod>")

-- main :: IO ()
-- main = haskellLastModifiedDateTime

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics
import Network.HTTP.Req
import qualified Text.URI as URI
import Data.Maybe (fromJust)
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Token = Token { access_token :: String } deriving (Generic, Show)

instance FromJSON Token
main :: IO ()
main = do
  loginRaw <- readFile "login.conf"
  let login = lines loginRaw
  t <- getToken login
  print t

getToken :: [String] -> IO Token
getToken login = runReq defaultHttpConfig $ do
  uri <- URI.mkURI "https://www.reddit.com/api/v1/access_token"
  let (url, options) = fromJust (useHttpsURI uri)
  r <- req POST url NoReqBody jsonResponse $
        "grant_type" =: ("password" :: String) <>
        "username" =: (login !! 0) <>
        "password" =: (login !! 1) <>
        basicAuth (C.pack $ login !! 2) (C.pack $ login !! 3) <>
        header "User-Agent" "freebsd:askhistorians:v1.2.3 (by /u/clumskyKnife)"
  pure (responseBody r :: Token)

-- -H "Authorization: bearer J1qK1c18UUGJFAzz9xnH56584l4" -A "ChangeMeClient/0.1 by YourUsername" https://oauth.reddit.com/api/v1/me

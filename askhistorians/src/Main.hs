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

import CMark
import Control.Monad.IO.Class
import Control.Monad
import Data.Aeson
import Data.Text as T
import GHC.Generics
import Network.HTTP.Req
import qualified Text.URI as URI
import Data.Maybe (fromJust)
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Token = Token {
  access_token :: String,
  token_type :: String
  } deriving (Generic)

data RawData = RawData {
  _data :: RawMarkdown
  } deriving (Generic, Show)

data RawMarkdown = RawMarkdown {
  content_md :: String
  } deriving (Generic, Show)


instance Show Token where
  show (Token a t)  = t ++ " " ++ a

instance FromJSON Token
instance FromJSON RawMarkdown

instance FromJSON RawData where
 parseJSON (Object v) =
    RawData  <$> v .: "data"
 parseJSON _ = mzero

-- First, get the token before requesting the API
getToken :: [String] -> IO Token
getToken login = runReq defaultHttpConfig $ do
  uri <- URI.mkURI "https://www.reddit.com/api/v1/access_token"
  let (url, _) = fromJust (useHttpsURI uri)
  r <- req POST url NoReqBody jsonResponse $
        "grant_type" =: ("password" :: String) <>
        "username" =: (login !! 0) <>
        "password" =: (login !! 1) <>
        basicAuth (C.pack $ login !! 2) (C.pack $ login !! 3) <>
        header "User-Agent" "freebsd:askhistorians:v1.2.3 (by /u/clumskyKnife)"
  pure (responseBody r :: Token)

readWiki :: Token -> IO RawData
readWiki t = runReq defaultHttpConfig $ do
  uri <- URI.mkURI "https://oauth.reddit.com/r/AskHistorians/wiki/books"
  let (url, _) = fromJust (useHttpsURI uri)
  r <- req GET url NoReqBody jsonResponse $
        header "User-Agent" "freebsd:askhistorians:v1.2.3 (by /u/clumskyKnife)" <>
        header "Authorization" (C.pack . show $ t)
  pure (responseBody r :: RawData)

-- Node (Maybe PosInfo) NodeType [Node]
-- The document is just a list of nodes apparently. No need to go deep
-- Example :
-- # Test
--  Content
--  ## Lol
-- becomes
-- Node  DOCUMENT [
--   Node  (HEADING 1) [Node  (TEXT "Test") []],
--   Node  PARAGRAPH [Node  (TEXT "Content") []],
--   Node  (HEADING 2) [Node  (TEXT "Lol") []]]
-- parse :: Node -> Text
-- parse (Node (Just _) (HEADING level) [Node (Just _) (TEXT title) _]) = title
-- parse (Node (Just _) _ []) = ""
-- parse (Node (Just _) _ n) = Prelude.concat (Prelude.map parse n)

printNode :: Int -> Node -> String
printNode depth (Node (Just _) t []) = offset ++ show t ++ "\n"
  where
    offset = Prelude.concat $ Prelude.take depth $ repeat "--"
printNode depth (Node (Just _) t n) = offset ++ show t ++ "\n"  ++ n'
  where
    offset = Prelude.concat $ Prelude.take depth $ repeat "--"
    n' = Prelude.concatMap (printNode (depth+1)) n

-- s = "## Books and Resources list\r\n\r\n### Also available on [Goodreads!](https://www.goodreads.com/askhistorians) "
main :: IO ()
main = do
  loginRaw <- readFile "login.conf"
  let login = Prelude.lines loginRaw
  t <- getToken login
  putStrLn $ show t
  r <- readWiki t
  let md = T.pack . content_md . _data $ r
  -- print $ content_md . _data $ r
  putStrLn $ printNode 0 $ commonmarkToNode [] md
  -- putStrLn $ printNode 0 $ commonmarkToNode [] (T.pack s)

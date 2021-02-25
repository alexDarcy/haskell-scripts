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

-- Data structure for books ---
data Book = Book {
  title :: T.Text,
  themes :: [T.Text],
  amazon :: T.Text,
  description :: T.Text
} deriving (Generic)

instance Show Book where
  show (Book t _ _ d) = show $ T.concat [t, T.pack ":", T.take 10 d]


-- For reddit API requests ---
data Token = Token {
  access_token :: String,
  token_type :: String
  } deriving (Generic)

newtype RawData = RawData {
  _data :: RawMarkdown
  } deriving (Generic, Show)

newtype RawMarkdown = RawMarkdown {
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
        "username" =: Prelude.head login <>
        "password" =: (login !! 1) <>
        basicAuth (C.pack $ login !! 2) (C.pack $ login !! 3) <>
        header "User-Agent" "freebsd:askhistorians:v1.2.3 (by /u/clumskyKnife)"
  pure (responseBody r :: Token)

root :: String
root = "https://oauth.reddit.com/r/AskHistorians/"

readWiki :: Text -> Token -> IO RawData
readWiki url t = runReq defaultHttpConfig $ do
  uri <- URI.mkURI url
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

-- -- The title is actually 2 nodes deep into a link !
-- titleFromLink :: Node -> Text
-- titleFromLink (Node (Just _) EMPH [Node (Just _) (TEXT t) []]) = t
-- titleFromLink _ = ""

-- A Paragraph node contains a list of nodes
-- 1. a link node with the amazon link
-- -> nested into the link node, an Emph node which contains a text node with the title !!
-- 2. Text node with the description
-- FIXME Ugly pattern matching
bookFromParagraph :: [Node] -> Maybe Book
bookFromParagraph [Node (Just _) (LINK url _) [
                      Node (Just _) EMPH [Node (Just _) (TEXT title) []]
                      ]
                  , Node (Just _) (TEXT descr) []] = Just $ Book title [] url descr
bookFromParagraph _ = Nothing

-- titleFromLink (Node (Just _) EMPH [Node (Just _) (TEXT t) []]) = t
                   -- (Node titleemph descr) n) = [titleFromLink . Prelude.head $ n]
-- Extract only links
parseLinks :: Node -> [Book]
parseLinks (Node (Just _) PARAGRAPH n) = maybe [] (\x -> [x]) $ bookFromParagraph n
parseLinks (Node (Just _) _ n) = Prelude.concatMap parseLinks n
parseLinks (Node _ _ []) = []

-- helper function
offset :: Int -> String
offset depth = Prelude.concat $ Prelude.replicate depth  "--"

printNode :: Int -> Node -> String
printNode depth (Node (Just _) t []) = offset depth ++ show t ++ "\n"
printNode depth (Node (Just _) t n) = offset depth ++ show t ++ "\n"  ++ n'
  where
    n' = Prelude.concatMap (printNode (depth+1)) n

-- s = "## Books and Resources list\r\n\r\n### Also available on [Goodreads!](https://www.goodreads.com/askhistorians) "
main :: IO ()
main = do
  loginRaw <- readFile "login.conf"
  let login = Prelude.lines loginRaw
  t <- getToken login
  print t
  -- This is easy
  r <- readWiki (T.pack $ root ++ "wiki/books/general") t
  let md = T.pack . content_md . _data $ r
  -- print $ content_md . _data $ r
  let nodes = commonmarkToNode [] md
  putStrLn $ printNode 0 nodes
  print $ parseLinks nodes
  -- putStrLn $ printNode 0 $ commonmarkToNode [] (T.pack s)
  -- putStrLn $ printNode 0 $ commonmarkToNode [] md

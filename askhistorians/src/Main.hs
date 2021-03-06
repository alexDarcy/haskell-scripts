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

import Data.List.Split
import CMark
import Control.Monad.IO.Class
import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Req
import qualified Text.URI as URI
import Data.Maybe
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

-- Data structure for books ---
data Book = Book {
  title :: T.Text,
  categories :: [T.Text],
  amazon :: T.Text,
  description :: T.Text
} deriving (Generic)

instance Show Book where
  show (Book t cat _ d) = show $ T.concat l
    where
      l = t : cat' : [ T.pack ":", T.take 10 d]
      cat' = T.concat [" (",  T.intercalate "," cat , ")"]

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

readWiki :: T.Text -> Token -> IO RawData
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
titleFromLink :: Node -> Maybe T.Text
titleFromLink (Node (Just _) EMPH [Node (Just _) (TEXT t) []]) = Just t
titleFromLink _ = Nothing

createBook :: Node -> [T.Text] -> T.Text -> T.Text -> Maybe Book
createBook emp cat url descr = (\x -> Just $ Book x cat url descr) =<< titleFromLink emp

-- The node structure for a Book inside a paragraph is a list :
-- [ LINK = amazon link
--    [ EMPH
--      [ TEXT = title ]
--    ]
-- , TEXT = description ]
-- Warning: there may be 2 links (two-parts books) with a shared description. We skip the second title
-- TODO: Hope it will not break
bookFromParagraph :: [T.Text] -> [Node] -> [Maybe Book]
bookFromParagraph category ((Node _ (LINK url _) [emp])
                   : (Node _ (TEXT descr) [])
                   : xs) = createBook emp category url descr : bookFromParagraph category xs
bookFromParagraph category ((Node _ (LINK url _) [emp])
                   : (Node _ (TEXT _) _)
                   : (Node _ (LINK _ _) _)
                   : (Node _ (TEXT descr) [])
                   : xs)= createBook emp category url descr : bookFromParagraph category xs
bookFromParagraph _ _ = []

-- A book is actually an item. For simplicity, we only search though paragraph with the following structure :
-- - a link (to amazon) with the book title
-- - text (description)
getBooksP :: [T.Text] -> Node -> [Book]
getBooksP category (Node _ PARAGRAPH n) = catMaybes $ bookFromParagraph category n
getBooksP category (Node _ _ n) = concatMap (getBooksP category) n

-- The document is a list of headings and paragraph.
-- the catagories are extracted from the headings and passed down to the book parser

-- Get category from heading
getCategory :: [Node] -> T.Text
getCategory [Node _ (TEXT t ) _] = t
getCategory [Node {}] = ""
getCategory [] = ""

getBooks  :: Node -> [Book]
getBooks  (Node _ DOCUMENT xs) = getBooksL [] xs
getBooks  _ = []

getBooksL :: [T.Text] -> [Node] -> [Book]
getBooksL cat (Node _ (HEADING h) hs : xs) = getBooksL c xs
  where c = getCategory hs : cat
getBooksL cat ((Node _ _ ns) : xs) = concatMap (getBooksP cat) ns ++ getBooksL cat xs
getBooksL _ [] = []

-- helper function
offset :: Int -> String
offset depth = Prelude.concat $ Prelude.replicate depth  "--"

-- helper function
printNode :: Int -> Node -> String
printNode depth (Node _ t []) = offset depth ++ show t ++ "\n"
printNode depth (Node _ t n) = offset depth ++ show t ++ "\n"  ++ n'
  where
    n' = Prelude.concatMap (printNode (depth+1)) n


sample = do
  f <- readFile "general.md"
  let nodes = commonmarkToNode [] $ T.pack f
  return $  getBooks nodes
  -- return $ concatMap bookFromParagraph $ getParagraph nodes
  -- putStrLn $ printNode 0 nodes

main :: IO ()
main = do
  loginRaw <- readFile "login.conf"
  let login = Prelude.lines loginRaw
  t <- getToken login
  print t
  -- This wiki page is easy
  r <- readWiki (T.pack $ root ++ "wiki/books/general") t
  let md = T.pack . content_md . _data $ r
  -- print $ content_md . _data $ r
  let nodes = commonmarkToNode [] md
  -- print nodes
  -- putStrLn $ printNode 0 nodes
  -- print $ Prelude.length $ getBooks nodes
  print $ getBooks nodes

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

createBook url descr emp = (\x -> Just $ Book x [] url descr) =<< titleFromLink emp

-- 1. either a link node with the amazon link and the title (2 levels deep !)
-- 2. Text node with the description
-- Warning: there may be 2 links (two-parts books) with a shared description. We skip the second title
-- TODO: Hope it will not break
bookFromParagraph :: [Node] -> [Maybe Book]
bookFromParagraph ((Node _ (LINK url _) [emp])
                   : (Node _ (TEXT descr) [])
                   : xs) = createBook url descr emp : bookFromParagraph xs
bookFromParagraph ((Node _ (LINK url _) [emp])
                   : (Node _ (TEXT _) _)
                   : (Node _ (LINK _ _) _)
                   : (Node _ (TEXT descr) [])
                   : xs)= createBook url descr emp : bookFromParagraph xs
bookFromParagraph _ = []

-- The document is actually a list of nodes (as lines in a text)
-- -- If we want to propagate the header to books, we have to split this list
splitByHeading :: [Node] -> String
splitByHeading [(Node _ (HEADING n) x), xs] = "lol"

-- readDocument :: Node -> [Node]
-- readDocument (Node _ DOCUMENT xs) = splitByHeading xs
getBooks :: Node -> [Book]
getBooks (Node _ PARAGRAPH n) = catMaybes $ bookFromParagraph n
getBooks (Node _ _ n) = concatMap getBooks n

-- helper function
offset :: Int -> String
offset depth = Prelude.concat $ Prelude.replicate depth  "--"

-- helper function
printNode :: Int -> Node -> String
printNode depth (Node _ t []) = offset depth ++ show t ++ "\n"
printNode depth (Node _ t n) = offset depth ++ show t ++ "\n"  ++ n'
  where
    n' = Prelude.concatMap (printNode (depth+1)) n

skipDocument :: Node -> [Node]
skipDocument (Node _ DOCUMENT n) = n
skipDocument _ = []

isHeading :: Int -> Node -> Bool
isHeading level (Node _ (HEADING level') _) = level == level'
isHeading _ _ = False


sample = do
  f <- readFile "general.md"
  let nodes = commonmarkToNode [] $ T.pack f
  -- return $ concatMap bookFromParagraph $ getParagraph nodes
  return $  getBooks nodes
  -- let groups = split (keepDelimsL $ whenElt (isHeading 3)) $ skipDocument nodes
  -- return groups
  -- print . Prelude.head $ l
  -- print . Prelude.head $ listNodes
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
  putStrLn $ printNode 0 nodes
  -- print $ Prelude.length $ getBooks nodes
  -- print $ getBooks nodes

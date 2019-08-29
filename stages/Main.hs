{-# LANGUAGE OverloadedStrings #-}
-- Read a PDF file to extract count redundant data. 
-- This is not a good example as the pdf layout is similar to a table and some
-- manual preprocessing is needed.
-- Still, it serves as an example for attoparsec usage

module Main where

-- Version non optimale : les données sont créées avec "pdftotex -layout lol.pdf"
-- On s'assure que l'espace entre les colonnes est d'au moins 2
-- Puis on utilise le preprocessing (cf preprocessing.sh) en vérifiant qu'il n'y
-- a pas eu de lignes de perdues

import Data.Word
import Data.Char
import Data.Either
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.List as L

data Stage = Stage {id :: String
  , unit :: String
  , place :: String
  , boss :: String }
  deriving (Show)

-- instance Show Stage where
--   show (Stage i u p b) = show u

-- instance Eq Stage where
--   (==) (Stage _ u _ _) (Stage _ u' _ _) = u == u'

-- instance Ord Stage where
--   (compare) (Stage _ u _ _) (Stage _ u' _ _) = compare u u'

-- content = notChar '#'

-- oneStage :: Parser Stage
-- oneStage = do
--   id <- many content
--   char '#'
--   unit  <- many content
--   char '#'
--   place <- many content
--   char '#'
--   boss <- manyTill content endOfLine
--   return $ Stage id unit place boss

-- allStages :: Parser [Stage]
-- allStages  = many $ oneStage

-- process = do
--   file <- TIO.readFile "test.txt"
--   let all = fromRight [] (parseOnly allStages file)
--   print $ length all
--   return all

-- printEntry :: (Stage, Int) -> T.Text
-- printEntry (x, y) = T.concat [ T.pack (unit x),  ";", T.pack (show y)]

-- main = do
--   l <- process
--   -- Count duplicates
--   let noDup = map (\l -> (head l, length l)) (L.group . L.sort $ l)
--   let content = T.unlines $ map printEntry noDup
--   let content' = T.concat [T.pack "service;nbpostes\n", content]
--   print content'
--   TIO.writeFile "postesA1S2.csv" $ content'

---
line1 = "E190     2° service et UMD C.H.S. Sarreguemines                               C.H.S.            MATEI"

--E312     Accueil - urgences Epinal                                            C.H.              LEMAU DE TALANCÉ

parseID :: Parser String
parseID  = do
  id1 <- letter
  id2 <- many decimal
  return $ id1 : (concatMap show $ id2)

parseStage = do
  id <- parseID
  space
  s1 <- many letter -- (satisfy isLetter)
  space
  s2 <-  string "C.H.S"
  return $ Stage id s1 (T.unpack s2) ""

testLook = do
  lol <- manyTill (lookAhead $ string "C.H.S")
  return lol
 
line2 = "E190 lol.test C.H.S"
runTest = parseOnly testLook line2

main = do
  print "coucou"

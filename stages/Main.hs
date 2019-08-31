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

import Data.Either
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.List as L

data Job = Job {id :: T.Text
  , unit :: T.Text
  , place :: T.Text
  , boss :: T.Text}
  deriving (Show)

-- instance Show Job where
--   show (Job i u p b) = show u

instance Eq Job where
  (==) (Job _ u _ _) (Job _ u' _ _) = u == u'

instance Ord Job where
  (compare) (Job _ u _ _) (Job _ u' _ _) = compare u u'


countIdentical :: [Job] -> [(Job, Int)]
countIdentical x = map (\l -> (head l, length l)) (L.group . L.sort $ x)

printEntry :: (Job, Int) -> T.Text
printEntry (x, y) = T.concat [ unit x,  "            ", T.pack (show y)]

---
line1 = "E190     2° service et UMD C.H.S. Sarreguemines                               C.H.S.            MATEI"
line2 = "E312     Accueil - urgences Epinal                                            C.H.             HOMEL"

--E312     Accueil - urgences Epinal                                            C.H.              LEMAU DE TALANCÉ
parseID :: Parser T.Text
parseID  = do
  id1 <- letter
  id2 <- decimal
  return $ T.concat [T.singleton id1, T.pack . show $ id2]

listPlaces = [
  "C.H.S."
  , "C.H.G."
  , "C.C.E.G."
  , "H.C."
  , "C.H."
  , "H.B."
  , "I.C.L."
  , "Bel Air"
  , "C.H."
  , "C.H.R."
  , "S.J."
  , "H.B."
  , "H.E."
  , "C.H."
  , "Cabinet médical"
  , "Mat"
  , "C.H."
  , "H.M.M."
  , "H.C."
  , "St-Charles"
  , "H.E."
  , "I.R.R."
  , "C.P.N."
  , "S.J."]

isPlace = choice $ map string l
  where l = map (\x -> T.append (T.pack x) (T.pack "  ")) listPlaces


-- Structure :
-- ID NAME PLACE BOSS
-- ID = a letter followed by 3 digits
-- name = a set of characters up to a place
-- place = search into a list of places *and* 2 spaces after (importsant !)
-- boss = everything else
parseJob :: Parser Job
parseJob = do
  id <- parseID
  space *> many1 space
  -- spaces are important
  s <- manyTill anyChar (lookAhead (isPlace))
  let s' = T.strip . T.pack $ s
  place <- isPlace
  space *> many1 space
  boss <- many1 letter
  return $ Job id  s' place (T.pack boss)

-- parseJobs = many $ parseJob <* endOfLine
-- runTest = parseOnly parseJobs $ T.unlines [line1, line2]

main = do
  file <- TIO.readFile "small.txt"
  -- let all = fromRight [] (parseOnly parseJobs file)
  -- Try to read a job by each line
  let all = map (parseOnly parseJob) $ T.lines file
  -- Filter non matching lines
  let all' =  rights all
  let output =  map printEntry $ countIdentical all'
  let output' = T.unlines $ T.pack "service;nbpostes" : output
  print output'
  TIO.writeFile "output.csv" $ output'

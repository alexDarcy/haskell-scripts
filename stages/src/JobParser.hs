{-# LANGUAGE QuasiQuotes #-}
-- | Attoparsec example
-- Data is created from a pdf with : pdftotex -layout
-- After reading the data, we count identical entries

-- Structure :
-- ID NAME PLACE BOSS
-- ID = a letter followed by 3 digits
-- name = a set of characters up to a place
-- place = search into a list of places *and* 2 spaces after (importsant !)
-- boss = everything else
module JobParser where

import Control.Applicative
import Data.Either
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.List as L
import Text.Printf
import Text.RawString.QQ

data Job = Job {id :: T.Text
  , unit :: T.Text
  , place :: T.Text
  , boss :: T.Text}
  deriving (Show)

instance Eq Job where
  (==) (Job _ u _ _) (Job _ u' _ _) = u == u'

instance Ord Job where
  (compare) (Job _ u _ _) (Job _ u' _ _) = compare u u'


countIdentical :: [Job] -> [(Job, Int)]
countIdentical x = map (\l -> (head l, length l)) (L.group . L.sort $ x)

-- Print a Job with padding and the number of occurences
-- Compatible with uniq output
printEntryCSV :: (Job, Int) -> T.Text
printEntryCSV (x, y) = T.pack $ printf "%7d %s" y (unit x)

-- Print a Job with padding and the number of occurences
-- Compatible for latex
printEntryLATEX :: (Job, Int) -> T.Text
printEntryLATEX (x, y) = T.pack $ printf "%s & %7d\\\\" (unit x) y

parseID :: Parser T.Text
parseID  = do
  id1 <- letter
  id2 <- decimal
  return $ T.concat [T.singleton id1, T.pack . show $ id2]

listPlaces = ["C.C.E.G."
             , "C.H."
             , "C.H.G."
             , "C.H.R."
             , "C.H.S."
             , "C.P.N."
             , "Cabinet médical"
             , "Cabinet médfical"      -- yes....
             , "cabinet médical"
             , "Cabinetmédical"
             , "cabinetmédical"
             , "Jury" -- Hack : pole 6 do not have a place
             , "Bel Air"
             , "H.B."
             , "H.C."
             , "H.E."
             , "H.M.M."
             , "I.C.L."
             , "I.R.R."
             , "Mat"
             , "S.J."
             , "St-Charles"]

-- Always two spaces after the place (otherwise the name can co)
isPlace = choice $ map string l
  where l = map (\x -> T.append (T.pack x) (T.pack "  ")) listPlaces

parseJob :: Parser Job
parseJob = do
  id <- parseID
  space *> many1 space
  -- spaces are important
  s <- manyTill anyChar (lookAhead isPlace)
  let s' = T.strip . T.pack $ s
  place <- isPlace
  space *> many1 space
  boss <- many1 (letter <|> char '?')
  return $ Job id  s' place (T.strip . T.pack $ boss)

-- If a line is not a job (i.e reading a Job fails), this allows us to continue
skipTill :: (Alternative f) => f a -> f b -> f b
skipTill skippedAction nextAction =
    nextAction
    <|> skippedAction *> skipTill skippedAction nextAction

skipLine = takeTill isEndOfLine <* endOfLine

-- We cannot read line by line as some Jobs are multi-line.....
parseAllJobs = many (skipLine `skipTill` parseJob)

getAllJobs :: T.Text -> [Job]
getAllJobs = fromRight [] . (parseOnly parseAllJobs)

parseData f = do
  file <- TIO.readFile f
-- We cannot read line by line as some Jobs are multi-line.....
  return $ countIdentical $ fromRight [] $ parseOnly parseAllJobs file

printCSV f all = do
  -- Count identical entries
  let output =  map printEntryCSV  all
  TIO.writeFile f $ T.unlines output

header :: String
header = [r|\documentclass{article}
 \usepackage[margin=5pt, left=15pt]{geometry}
 \usepackage[utf8]{inputenc}
 \usepackage[T1]{fontenc}
 \usepackage{xtab}
% \usepackage{pgfplotstable,filecontents}

\begin{document}

\twocolumn
\begin{xtabular}{p{8cm}c}
\shrinkheight{-2in} % hack for less page|]

footer :: String
footer = [r| \end{xtabular}
\end{document}|]

printLATEX f all = do
  -- Count identical entries
  let output =  map printEntryLATEX  all
  TIO.writeFile f $ T.unlines $ (T.pack header) : output ++ [T.pack footer]

module Main where

import Test.QuickCheck
import System.Random
import Data.List

data AnimalProteins = Beef | Chicken | Fish
data Legumes = WhiteBeans | RedBeans | Lentils | Chickpeas

data Proteins = AnimalProteins | Legumes deriving (Show)
data Menu = Menu Proteins  deriving (Show)

instance Show AnimalProteins where
    show Beef = "boeuf"
    show Chicken = "poulet"
    show Fish = "poisson"

instance Show Legumes where
    show WhiteBeans = "haricots blancs"
    show RedBeans = "haricots rouges"
    show Lentils = "lentilles"
    show Chickpeas = "pois chiches"

rAnimalProteins :: Gen AnimalProteins
rAnimalProteins = elements [Beef , Chicken , Fish]

rLegumes :: Gen Legumes
rLegumes = elements [WhiteBeans , RedBeans , Lentils , Chickpeas]

rProteins :: Gen Proteins
rProteins = choose (rLegumes, rAnimalProteins)

-- rMenu :: Gen Menu
-- rMenu = do
--           m <- rProteins
--           return (Menu m)

-- rMenu :: Gen Menu
-- rMenu = do
--       p <- rProteins
--       return (Menu p)

legumes = ["haricots blancs"
          , "haricots rouges"
          , "lentilles vertes"
          , "pois chiches"
          , "lentilles corail"]
vegetables = ["betteraves"
             , "carottes"
             , "choux de bruxelles"
             , "choux-fleur"
             , "haricots verts"
             , "navets"
             , "petits pois"
             , "poireaux"
             , "épinards"
             ]

randItem :: [a] -> IO a
randItem = generate . elements

main = do
    seed  <- newStdGen
    print "lol"
    -- let rs = randomlist (length proteins) seed
    -- print $ proteins !! rs

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

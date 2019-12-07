module Main where

import Test.QuickCheck
import System.Random
import Data.List

data AnimalProteins = Beef | Chicken | Fish
data Legumes = WhiteBeans | RedBeans | Lentils | PinkLentils | Chickpeas
data Vegetables = Beetroots | BrusselsSprouts | Cabbage | Carrots | Cauliflower | GreenBeans | Leeks | Peas | Rutabaga | Spinach | Turnips
data Carbs = Pasta | Rice | BrownRice | WholePasta | Potatoes | SweetPotatoes

data Proteins = AP AnimalProteins | Leg Legumes
data Menu = Menu Proteins Vegetables Carbs

instance Show Proteins where
    show (AP p) = show p
    show (Leg l) = show l

instance Show Menu where
    show (Menu p v c) = show p ++ ", " ++ show v ++ ", " ++ show c
   
instance Show AnimalProteins where
    show Beef = "boeuf"
    show Chicken = "poulet"
    show Fish = "poisson"

instance Show Legumes where
    show Chickpeas = "pois chiches"
    show Lentils = "lentilles"
    show PinkLentils = "lentilles corail"
    show RedBeans = "haricots rouges"
    show WhiteBeans = "haricots blancs"

instance Show Vegetables where
    show Beetroots = "betteraves"
    show BrusselsSprouts = "choux de bruxelles"
    show Cabbage = "chou"
    show Carrots = "carottes"
    show Cauliflower = "chou-fleur"
    show GreenBeans = "haricots verts"
    show Leeks = "poireaux"
    show Peas = "petits pois"
    show Rutabaga = "rutabaga"
    show Spinach = "épinards"
    show Turnips = "navets"

instance Show Carbs where
    show Pasta= "pâtes"
    show Rice= "riz"
    show BrownRice= "riz complet"
    show WholePasta= "pâtes complètes"
    show Potatoes= "pommes de terre"
    show SweetPotatoes= "patates douces"

rAnimalProteins :: Gen AnimalProteins
rAnimalProteins = elements [Beef , Chicken , Fish]

rLegumes :: Gen Legumes
rLegumes = elements [WhiteBeans , RedBeans , Lentils , Chickpeas]

rProteins :: Gen Proteins
rProteins = oneof [fmap AP rAnimalProteins, fmap Leg rLegumes]

rVegetables :: Gen Vegetables
rVegetables  = elements [Beetroots
                       , BrusselsSprouts
                       ,  Cabbage
                       ,  Carrots
                       ,  Cauliflower
                       ,  GreenBeans
                       ,  Leeks
                       ,  Peas
                       ,  Rutabaga
                       ,  Spinach
                       ,  Turnips]

rCarbs :: Gen Carbs
rCarbs = elements [Pasta
                  , Rice
                  , BrownRice
                  , WholePasta
                  , Potatoes
                  , SweetPotatoes]

rMenu :: Gen Menu
rMenu = do
   p <- rProteins
   v <- rVegetables
   c <- rCarbs
   return (Menu p v c)

main = do
   sample rMenu
    -- let rs = randomlist (length proteins) seed
    -- print $ proteins !! rs

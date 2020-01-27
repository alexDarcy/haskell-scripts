module Main where

import Test.QuickCheck
import System.Random
import Data.List

data AnimalProteins = Beef | Chicken | Fish
  deriving  (Eq)
data Legumes = WhiteBeans | RedBeans | Lentils | PinkLentils | Chickpeas
  deriving  (Eq)
data Vegetables = Beetroots | BrusselsSprouts | Cabbage | Carrots | Cauliflower | GreenBeans | Leeks | Peas | Rutabaga | Spinach | Turnips
  deriving  (Eq)
data Carbs = Pasta | Rice | BrownRice | WholePasta | Potatoes | SweetPotatoes
  deriving  (Eq)

data Proteins = AP AnimalProteins | Leg Legumes
  deriving  (Eq)
data Menu = Menu Proteins Vegetables Carbs

-- Used later to remove menu with at least one common part
instance Eq Menu where
  (==) (Menu p1 v1 c1) (Menu p2 v2 c2) = p1 == p2 || v1 == v2 || c1 == c2

-- Used to alternate between the 2 proteins
hasAnimalProt :: Menu -> Bool
hasAnimalProt (Menu (AP _) _ _) = True
hasAnimalProt _ = False

instance Show Proteins where
    show (AP p) = show p
    show (Leg l) = show l

instance Show Menu where
    show (Menu p v c) = show p ++ ", " ++ show v ++ ", " ++ show c ++ "\n"
   
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

-- Menu with maybe some redudancy and not well ordered
pseudoMenu :: Int -> IO [Menu]
pseudoMenu n =
  generate (sequence [ resize i rMenu | i <- [1..n] ])

-- -- Proteins only every 2 days
alternateProt :: [Menu] -> [Menu]
alternateProt m = concat $ zipWith (\x y -> [x, y]) animal legumes
  where (animal, legumes) = partition (hasAnimalProt) m

-- -- Remove redudancy
-- cleanMenu :: ([Menu], [Menu]) -> ([Menu], [Menu])
-- cleanMenu ([], y) = ([], y)
-- cleanMenu ([x:xs], y) = cleanMenu ( x : cleaned, others)
--   where (cleaned, others) = partition (/= x) xs

main = do
  let n = 7
  -- Take some margin because the randomisation has a lot of redudancies
  m <- pseudoMenu (4*n)
  let m' = take n $ alternateProt m
  print m'
    -- let rs = randomlist (length proteins) seed
    -- print $ proteins !! rs

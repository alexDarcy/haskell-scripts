{-# LANGUAGE OverloadedStrings #-}
-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import JobParser

import Data.Text as T
import qualified Data.Text.IO as TIO

singleLine = T.pack "E8       Unité de Soins de Longue Durée (USLD)                                   S.J.        MANCIAUX"
twoLines = T.pack "E666       CardiologieBidon             \n              H.C.        XXX"

main :: IO ()
main = hspec $ do
  describe "Small tests" $ do
    it "single line" $ do
      getAllJobs singleLine `shouldBe` [Job "E8" "Unité de Soins de Longue Durée (USLD)" "S.J." "MANCIAUX"]
    it "multi-lines (2)" $ do
      getAllJobs twoLines `shouldBe` [Job "E666" "CardiologieBidon" "H.C." "XXX"]

  describe "Full tests" $ do
    it "A1Trimestre3" $ do
      output <- parseData "tests/postesa1trimestre2.txt"
      TIO.writeFile "output.csv" output
      ref <- TIO.readFile "tests/postesa1trimestre2_ref.txt"
      ref == output `shouldBe` True

{-# LANGUAGE OverloadedStrings #-}
-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import JobParser
import Data.Text as T

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
      pdftotext -layout" postesA1Trimestre3.pdf

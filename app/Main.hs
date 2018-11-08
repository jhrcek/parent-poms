module Main where

import Pom (ImageFormat (PNG))
import qualified Pom

main :: IO ()
main = do
    parentChains <- Pom.getParentChains
    Pom.analyzeProperties parentChains
    Pom.showHierarchy PNG parentChains

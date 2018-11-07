module Main where

import qualified Pom

main :: IO ()
main = do
    parentChains <- Pom.getParentChains
    Pom.analyzeProperties parentChains
    Pom.showHierarchy parentChains

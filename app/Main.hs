module Main where

import qualified Options
import qualified Pom

main :: IO ()
main = do
    opts <- Options.parse
    let imageFormat = Options.imageFormat opts
    parentChains <- Pom.getParentChains
    Pom.analyzeProperties parentChains
    Pom.showHierarchy imageFormat parentChains

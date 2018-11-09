module Main where

import qualified Data.Map as Map
import qualified Options
import qualified Pom

main :: IO ()
main = do
    opts <- Options.parse
    let imageFormat = Options.imageFormat opts
        userHome = Options.userHome opts
    parentChains <- Pom.getParentChains
    gav2props <- Pom.loadProperties userHome parentChains
    mapM_ print $ Map.toList gav2props
    Pom.showHierarchy imageFormat parentChains

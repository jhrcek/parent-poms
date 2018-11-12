{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Maven.PomAncestors as Mvn
import qualified Options
import qualified Pom.Graphviz as Graphviz
import qualified Pom.Properties as Props
import qualified Pom.PropOverrides as Override

import Data.List (sortOn)
import Data.Set (Set)
import Options (Options (..))
import Pom.PropOverrides (PropOverride)

main :: IO ()
main = do
    Options{userHome, imageFormat, nodeFormat} <- Options.parse
    ancestorChains <- Mvn.getAncestorChains
    gav2props <- Props.loadProperties userHome ancestorChains

    let declared = mconcat $ Map.keysSet . Props.propsDeclared <$> Map.elems gav2props
        used =  mconcat $ Props.propsUsed <$> Map.elems gav2props
        unused = Set.difference declared used
    putStrLn "===== Unused property declarations ====="
    mapM_ print unused

    let allOverrides = foldMap (Override.getOverrides gav2props) ancestorChains
    saveToFile "all-overrides.txt" allOverrides

    let uselessOverrides = Set.filter Override.isUseless allOverrides
    saveToFile "useless-overrides.txt" uselessOverrides

    Graphviz.showHierarchy imageFormat nodeFormat ancestorChains gav2props


saveToFile :: String -> Set PropOverride -> IO ()
saveToFile file =
    Text.writeFile file
    . Text.unlines
    . fmap Override.formatOverride
    . sortOn Override.gav
    . Set.toList

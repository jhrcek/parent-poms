module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
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
    (Options userHome_ imageFormat_ nodeFormat_) <- Options.parse
    ancestorChains <- Mvn.getAncestorChains
    gav2props <- Props.loadProperties userHome_ ancestorChains

    let declared = mconcat $ Map.keysSet . Props.propsDeclared <$> Map.elems gav2props
        used =  mconcat $ Props.propsUsed <$> Map.elems gav2props
        unused = Set.difference declared used
    putStrLn "===== Unused property declarations ====="
    mapM_ print unused

    let allOverrides = foldMap (Override.getOverrides gav2props) ancestorChains
    putStrLn "===== All overrides ====="
    printOverrides allOverrides

    let uselessOverrides = Set.filter Override.isUseless allOverrides
    putStrLn "===== Useles overrides ====="
    printOverrides uselessOverrides

    Graphviz.showHierarchy imageFormat_ nodeFormat_ ancestorChains gav2props

printOverrides :: Set PropOverride -> IO ()
printOverrides =
    mapM_ (Text.putStrLn . Override.formatOverride) . sortOn Override.gav . Set.toList

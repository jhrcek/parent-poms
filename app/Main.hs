module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as Text
import qualified Maven.PomAncestors as Mvn
import Options (Options (..))
import qualified Options
import qualified Pom.Graphviz as Graphviz
import qualified Pom.Properties as Props
import qualified Pom.PropOverrides as Override

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
    mapM_ (Text.putStrLn . Override.formatOverride) allOverrides

    let uselessOverrides = Set.filter Override.isUseless allOverrides
    putStrLn "===== Useles overrides ====="
    mapM_ (Text.putStrLn . Override.formatOverride) uselessOverrides

    Graphviz.showHierarchy imageFormat_ nodeFormat_ ancestorChains gav2props

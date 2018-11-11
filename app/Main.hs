module Main where

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
    (Options usrHome imgFormat) <- Options.parse
    ancestorChains <- Mvn.getAncestorChains
    gav2props <- Props.loadProperties usrHome ancestorChains

    let allOverrides = foldMap (Override.getOverrides gav2props) ancestorChains
        uselessOverrides = Set.filter Override.isUseless allOverrides

    mapM_ (Text.putStrLn . Override.formatOverride) uselessOverrides

    Graphviz.showHierarchy imgFormat ancestorChains

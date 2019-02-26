{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map           as Map
import qualified Data.Set           as Set
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text
import qualified Maven.PomAncestors as Mvn
import qualified Options
import qualified Pom.Graphviz       as Graphviz
import qualified Pom.Properties     as Props
import qualified Pom.PropOverrides  as Override

import           Data.Coerce        (coerce)
import           Data.List          (sortOn)
import           Data.Set           (Set)
import           Options            (Options (..))
import           Pom.Properties     (PropKey)
import           Pom.PropOverrides  (PropOverride)

main :: IO ()
main = do
    Options{userHome, imageFormat, nodeFormat} <- Options.parse
    ancestorChains <- Mvn.getAncestorChains
    gav2props <- Props.loadProperties userHome ancestorChains

    let declared = mconcat $ Map.keysSet . Props.propsDeclared <$> Map.elems gav2props
        used =  mconcat $ Props.propsUsed <$> Map.elems gav2props
        unused = Set.difference declared used

    saveProperties "unused-properties.txt" unused

    let allOverrides = foldMap (Override.getOverrides gav2props) ancestorChains
    saveOverrides "all-overrides.txt" allOverrides

    let uselessOverrides = Set.filter Override.isUseless allOverrides
    saveOverrides "useless-overrides.txt" uselessOverrides

    Graphviz.generateHierarchyImage imageFormat nodeFormat ancestorChains gav2props

saveProperties :: FilePath -> Set PropKey -> IO ()
saveProperties file =
    Text.writeFile file
    . Text.unlines
    . fmap coerce
    . Set.toList

saveOverrides :: String -> Set PropOverride -> IO ()
saveOverrides file =
    Text.writeFile file
    . Text.unlines
    . fmap Override.formatOverride
    . sortOn Override.gav
    . Set.toList

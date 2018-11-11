module Main where

import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.Text.IO as Text
import qualified Data.Tree as Tree
import qualified Options as Opts
import qualified Pom
import qualified Pom.PropOverrides as Override

main :: IO ()
main = do
    opts <- Opts.parse
    let imageFormat = Opts.imageFormat opts
        userHome = Opts.userHome opts

    parentChains <- Pom.getParentChains

    let pomHierarchy = Pom.buildForest parentChains
    mapM_ (\tree -> putStrLn $ show (Tree.rootLabel tree) <> " is root of POM hierarchy with " <> show (F.length tree) <> " POMs") pomHierarchy

    gav2props <- Pom.loadProperties userHome pomHierarchy

    let uselessOverrides = Set.filter Override.isUseless $
            foldMap (Override.getOverrides gav2props) parentChains
    mapM_ (Text.putStrLn . Override.formatOverride) uselessOverrides

    Pom.showHierarchy imageFormat parentChains

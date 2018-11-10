module Main where

import qualified Data.Set as Set
import qualified Data.Text.IO as Text
import qualified Options as Opts
import qualified Pom
import qualified Pom.PropOverrides as Override

main :: IO ()
main = do
    opts <- Opts.parse
    let imageFormat = Opts.imageFormat opts
        userHome = Opts.userHome opts
    parentChains <- Pom.getParentChains
    gav2props <- Pom.loadProperties userHome parentChains

    let uselessOverrides = Set.filter Override.isUseless $
            foldMap (Override.getOverrides gav2props) parentChains
    mapM_ (Text.putStrLn . Override.formatOverride) uselessOverrides

    Pom.showHierarchy imageFormat parentChains

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}

module Pom.Properties
    ( Properties
    , DeclaredProps
    , UsedProps
    , PropKey(PK)
    , PropValue(PV)
    , loadProperties
    , propsDeclared
    , propsUsed
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Text.XML as XML

import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Maven.Types (AncestorChain (AncestorChain), GAV (GAV))
import Prelude hiding (FilePath)
import Text.XML (Document, Element, Name (..), def)
import Text.XML.Lens (Traversal', el, localName, nodes, root, to, (./), (^.),
                      (^..), (^?), _Content, _Element, _head)
import Turtle (FilePath, format, fp, fromText, testfile, (%), (</>))

data Properties = Properties
  { propsDeclared :: DeclaredProps
  , propsUsed     :: UsedProps
  }

instance Semigroup Properties where
    Properties d1 u1 <> Properties d2 u2 = Properties (d1 <> d2) (u1 <> u2)

instance Monoid Properties where
    mempty = Properties Map.empty Set.empty

-- | Properties declared in the <properties> section of the pom.xml
type DeclaredProps = Map PropKey PropValue
-- | Properties used throughout the pom (stuff in ${..})
type UsedProps = Set PropKey

newtype PropKey = PK Text deriving newtype (Eq, Ord, Show)
newtype PropValue = PV Text deriving newtype (Eq, Ord, Show)

loadProperties :: FilePath -> [AncestorChain] -> IO (Map GAV Properties)
loadProperties userHome ancestorChains =
    foldMap loadProps uniqueGavs
  where
    uniqueGavs :: [GAV]
    uniqueGavs = nub $ concatMap (\(AncestorChain gavs) -> gavs) ancestorChains

    loadProps :: GAV -> IO (Map GAV Properties)
    loadProps gav = do
        let pomPath = toPomPath userHome gav
        exists <- testfile pomPath
        if exists
            then do
                declared <-  loadDeclaredProps pomPath
                used <- loadUsedProps pomPath
                return . Map.singleton gav $ Properties declared used
            else do
                Text.putStrLn $ format ("WARNING: "%fp%" does not exist. Did you mvn install the repo?") pomPath
                return Map.empty

toPomPath :: FilePath -> GAV -> FilePath
toPomPath userHome (GAV g a v) =
    userHome </> fromText (Text.intercalate "/" [".m2/repository", Text.replace "." "/"  g, a, v, a <> "-" <> v <> ".pom"])

loadDeclaredProps :: FilePath -> IO DeclaredProps
loadDeclaredProps pomFile =
    parseDeclaredProps <$> XML.readFile def ghcIoPomFile
  where
    ghcIoPomFile = Text.unpack $ format fp pomFile

loadUsedProps :: FilePath -> IO UsedProps
loadUsedProps pomPath = do
    pomContents <- Text.readFile $ Text.unpack $ format fp pomPath
    case Text.splitOn "${" pomContents of
        [] -> return Set.empty
        (_:propsAndCrap) -> return . Set.fromList $ fmap (PK . Text.takeWhile (/='}')) propsAndCrap

parseDeclaredProps :: Document -> DeclaredProps
parseDeclaredProps doc =
    Map.fromList $ doc ^.. root
    . pomEl "project" ./ pomEl "properties"
    . nodes . traverse . _Element
    . to getPropertyKeyAndValue

pomEl :: Text -> Traversal' Element Element
pomEl elName = el $ Name
  { nameLocalName = elName
  , nameNamespace = Just "http://maven.apache.org/POM/4.0.0"
  , namePrefix = Nothing
  }

getPropertyKeyAndValue :: Element -> (PropKey, PropValue)
getPropertyKeyAndValue e = (PK key, PV val)
  where
    key =  e ^. localName
    val = fromMaybe "" $ e ^? nodes . _head . _Content

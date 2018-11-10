{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}

module Pom.Properties
    ( Properties
    , PropKey(PK)
    , PropValue(PV)
    , readProperties
    ) where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Prelude hiding (FilePath)
import Text.XML (Document, Element, Name (..), def)
import Text.XML.Lens (Traversal', el, localName, nodes, root, to, (./), (^.),
                      (^..), (^?), _Content, _Element, _head)
import Turtle (FilePath, format, fp)

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Text.XML as XML

type Properties = Map PropKey PropValue
newtype PropKey = PK Text deriving newtype (Eq, Ord, Show)
newtype PropValue = PV Text deriving newtype (Eq, Ord, Show)

{-| Read all property-value pairs from given pom.xml -}
readProperties :: FilePath -> IO Properties
readProperties pomFile =
    parseProps <$> XML.readFile def ghcIoPomFile
  where
    ghcIoPomFile = Text.unpack $ format fp pomFile

parseProps :: Document -> Properties
parseProps doc =
    Map.fromList $ doc ^.. root
    . pomEl "project" ./ pomEl "properties"
    . nodes . traverse . _Element
    . to getPropertyNameAndValue

pomEl :: Text -> Traversal' Element Element
pomEl elName = el $ Name
  { nameLocalName = elName
  , nameNamespace = Just "http://maven.apache.org/POM/4.0.0"
  , namePrefix = Nothing
  }

getPropertyNameAndValue :: Element -> (PropKey, PropValue)
getPropertyNameAndValue e = (PK key, PV val)
  where
    key =  e ^. localName
    val = fromMaybe "" $ e ^? nodes . _head . _Content

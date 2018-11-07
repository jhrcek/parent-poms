{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Pom.Properties (readProperties) where

import qualified Data.Text as Text
import qualified Text.XML as XML

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Prelude hiding (FilePath)
import Text.XML (Document, Element, Name (..), def)
import Text.XML.Lens (Traversal', el, localName, nodes, root, to, (./), (^.),
                      (^..), (^?), _Content, _Element, _head)
import Turtle (FilePath, format, fp)

{-| Read all property-value pairs from given pom.xml -}
readProperties :: FilePath -> IO [(Text, Text)]
readProperties pomFile =
    parseProps <$> XML.readFile def ghcIoPomFile
  where
    ghcIoPomFile = Text.unpack $ format fp pomFile

parseProps :: Document -> [(Text, Text)]
parseProps doc =
    doc ^.. root
    . pomEl "project" ./ pomEl "properties"
    . nodes . traverse . _Element
    . to getPropertyNameAndValue

pomEl :: Text -> Traversal' Element Element
pomEl elName = el $ Name
  { nameLocalName = elName
  , nameNamespace = Just "http://maven.apache.org/POM/4.0.0"
  , namePrefix = Nothing
  }

getPropertyNameAndValue :: Element -> (Text, Text)
getPropertyNameAndValue e = (propName, propValue)
  where
    propName =  e ^. localName
    propValue = fromMaybe "" $ e ^? nodes . _head . _Content

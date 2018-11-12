{-# LANGUAGE OverloadedStrings #-}
module Pom.Graphviz (showHierarchy) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Maven.Types (AncestorChain (AncestorChain), GAV (GAV))
import Options (ImageFormat (..), NodeFormat (..))
import Pom.Properties (PropKey (PK), PropValue (PV), Properties (..),
                       propsDeclared, propsUsed)
import Turtle (Line, empty, select, shells, textToLine)

showHierarchy :: ImageFormat -> NodeFormat -> [AncestorChain] -> Map GAV Properties -> IO ()
showHierarchy imageFormat nodeFormat ancestorChains gav2Props = do
    shells ("dot -T" <> extension <> " -o hierarchy." <> extension) $ select dotLines
    shells (viewer <> " hierarchy." <> extension) empty
  where
    dotLines = toDotSource nodeRenderer ancestorChains
    (extension, viewer) = case imageFormat of
        PNG -> ("png", "shotwell")
        SVG -> ("svg", "google-chrome")
    nodeRenderer = case nodeFormat of
        ArtifactId       -> artifactIdRenderer
        Gav              -> fullGavRenderer
        GavAndProperties -> gavAndPropertiesRendered gav2Props


toDotSource :: NodeRenderer -> [AncestorChain] -> [Line]
toDotSource renderNode ancestorChains  =
    "digraph G {" : "rankdir=RL" : "node[shape=box]" : edgeLines <> ["}"]
  where
    edgeLines :: [Line]
    edgeLines =
        mapMaybe edgeToLine moduleParentPairs

    edgeToLine :: (GAV, GAV) -> Maybe Line
    edgeToLine (child, parent) =
        textToLine $ renderNode child <> " -> " <> renderNode parent

    moduleParentPairs :: [(GAV, GAV)] -- Name of module paired with it's parent module
    moduleParentPairs =
        nub $ concatMap (\(AncestorChain gavs) -> overlappingPairs gavs) ancestorChains

type NodeRenderer = GAV -> Text

gavAndPropertiesRendered :: Map GAV Properties -> NodeRenderer
gavAndPropertiesRendered gav2props gav@(GAV g a v) =
    quote $ Text.unlines (theGav : declaredPropsLines <> usedPropsLines)
  where
    theGav = Text.intercalate ":" [g,a,v]
    propertiesForGav = Map.findWithDefault mempty gav gav2props
    declaredProps = fmap (\(PK key, PV val) -> key <> "=" <> val) . Map.toList $ propsDeclared propertiesForGav
    usedProps =  fmap (\(PK key) -> key) . Set.toList $ propsUsed propertiesForGav
    declaredPropsLines = addHeadingIfNonempty "-- Declared Properties ---" declaredProps
    usedPropsLines = addHeadingIfNonempty  "--- Used Properties ---" usedProps
    addHeadingIfNonempty heading xs = case xs of
        [] -> []
        _  -> heading : xs

fullGavRenderer :: NodeRenderer
fullGavRenderer (GAV g a v) =
    quote $ Text.intercalate ":" [g,a,v]

artifactIdRenderer :: NodeRenderer
artifactIdRenderer (GAV _ a _) =
    quote a

quote :: Text -> Text
quote = Text.pack . show

-- [1,2,3,4,5] -> [(1,2), (2,3), (3,4), (4,5)]
overlappingPairs :: [a] -> [(a,a)]
overlappingPairs []       = []
overlappingPairs [_]      = []
overlappingPairs (x:y:zs) = (x,y) : overlappingPairs (y:zs)

{-# LANGUAGE OverloadedStrings #-}
module Pom.Graphviz (showHierarchy) where

import qualified Data.Text as Text

import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Maven.Types (AncestorChain (AncestorChain), GAV (GAV))
import Options (ImageFormat (..))
import Turtle (Line, empty, select, shells, textToLine)

showHierarchy :: ImageFormat -> [AncestorChain]  -> IO ()
showHierarchy imageFormat ancestorChains = do
    shells ("dot -T" <> extension <> " -o hierarchy." <> extension) $ select dotLines
    shells (viewer <> " hierarchy." <> extension) empty
  where
    dotLines = toDotSource fullGavRenderer ancestorChains
    (extension, viewer) = case imageFormat of
        PNG -> ("png", "shotwell")
        SVG -> ("svg", "google-chrome")

toDotSource :: NodeRenderer -> [AncestorChain] -> [Line]
toDotSource renderNode ancestorChains =
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

fullGavRenderer :: NodeRenderer
fullGavRenderer (GAV g a v) =
    Text.pack . show $ Text.intercalate ":" [g,a,v]

{-
artifactIdRenderer :: NodeRenderer
artifactIdRenderer (GAV _ a _) =
    "\"" <> a <> "\""
-}

-- [1,2,3,4,5] -> [(1,2), (2,3), (3,4), (4,5)]
overlappingPairs :: [a] -> [(a,a)]
overlappingPairs []       = []
overlappingPairs [_]      = []
overlappingPairs (x:y:zs) = (x,y) : overlappingPairs (y:zs)

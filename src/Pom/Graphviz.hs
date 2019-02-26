{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Pom.Graphviz (generateHierarchyImage) where

import qualified Data.Map       as Map
import qualified Data.Set       as Set
import qualified Data.Text      as Text

import           Data.List      (nub)
import           Data.Map       (Map)
import           Data.Maybe     (mapMaybe)
import           Data.Text      (Text)
import           Maven.Types    (AncestorChain (AncestorChain), GAV (GAV))
import           Options        (ImageFormat (..), NodeFormat (..))
import           Pom.Properties (PropKey (PK), PropValue (PV), Properties (..),
                                 propsDeclared, propsUsed)
import           Turtle         (Line, format, fp, printf, procs, s, select,
                                 textToLine, (%), (<.>))

generateHierarchyImage :: ImageFormat -> NodeFormat -> [AncestorChain] -> Map GAV Properties -> IO ()
generateHierarchyImage imageFormat nodeFormat ancestorChains gav2Props = do
    procs "dot" ["-T", extension, "-o", outputFile] $ select dotLines
    printf ("Generated image "%s%"\n") outputFile
  where
    dotLines = toDotSource nodeRenderer ancestorChains
    extension = getExtension imageFormat
    outputFile = format fp $ outputImageName <.> extension

    (nodeRenderer, outputImageName) = case nodeFormat of
        ArtifactId       -> (artifactIdRenderer, "parent-pom-hierarchy-artifacts")
        Gav              -> (fullGavRenderer, "parent-pom-hierarchy-gavs")
        GavAndProperties -> (gavAndPropertiesRendered gav2Props, "parent-pom-hierarchy-props")

getExtension :: ImageFormat -> Text
getExtension = \case
    PNG -> "png"
    SVG -> "svg"


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
    declaredPropsLines = addHeadingIfNonempty "--- Declared Properties ---" declaredProps
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

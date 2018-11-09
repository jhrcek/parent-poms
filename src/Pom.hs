{-# LANGUAGE OverloadedStrings #-}

module Pom
  ( loadProperties
  , getParentChains
  , showHierarchy
  , GAV(..)
  , ParentChain(..)
  ) where

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Control.Exception (handle)
import Control.Foldl (Fold (Fold))
import Control.Monad (foldM)
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Options (ImageFormat (..))
import Pom.Properties (Properties, readProperties)
import Prelude hiding (FilePath)
import Turtle (ExitCode, FilePath, Line, Shell, empty, exit, export, fold,
               format, fp, fromText, inshell, lineToText, repr, select, shells,
               testfile, textToLine, (%), (</>))


loadProperties :: FilePath -> [ParentChain] -> IO (Map GAV Properties)
loadProperties userHome parentChains =
    foldM loadProps Map.empty uniqueGavs
  where
    uniqueGavs :: [GAV]
    uniqueGavs = nub $ concatMap (\(ParentChain gavs) -> gavs) parentChains

    loadProps :: Map GAV Properties -> GAV -> IO (Map GAV Properties)
    loadProps m gav = do
        let pomPath = toPomPath userHome gav
        exists <- testfile pomPath
        if exists
            then (\props -> Map.insert gav props m) <$> readProperties pomPath
            else do
              Text.putStrLn $ format ("WARNING: "%fp%" does not exist. Did you mvn install the repo?") pomPath
              return m

data GAV = GAV
   { gavGroupId    :: Text
   , gavArtifactId :: Text
   , gavVersion    :: Text
   } deriving (Eq, Ord)

instance Show GAV where
  show (GAV g a v) = Text.unpack $ Text.intercalate ":" [g,a,v]

toPomPath :: FilePath -> GAV -> FilePath
toPomPath userHome (GAV g a v) =
    userHome </> fromText (Text.intercalate "/" [".m2/repository", Text.replace "." "/"  g, a, v, a <> "-" <> v <> ".pom"])
{-
List of GAVs starting from module's own GAV, followed by list of its parents ending with the root
Extracted from per-module output of `mvn dependency:display-ancestors` which looks like this:

[INFO] ---------< org.kie.server:kie-server-integ-tests-optaplanner >----------
[INFO] Building KIE :: Execution Server :: Tests :: OptaPlanner Integration Tests 7.14.0-SNAPSHOT [62/117]
[INFO] --------------------------------[ jar ]---------------------------------
[INFO]
[INFO] --- maven-dependency-plugin:3.1.1:display-ancestors (default-cli) @ kie-server-integ-tests-optaplanner ---
[INFO] Ancestor POMs: org.kie.server:kie-server-tests:7.14.0-SNAPSHOT <- org.kie.server:kie-server-parent:7.14.0-SNAPSHOT <- org.drools:droolsjbpm-integration:7.14.0-SNAPSHOT <- org.kie:kie-parent:7.14.0-SNAPSHOT <- org.jboss.integration-platform:jboss-integration-platform-bom:8.3.2.Final <- org.jboss.integration-platform:jboss-integration-platform-parent:8.3.2.Final <- org.jboss:jboss-parent:29
-}
newtype ParentChain = ParentChain [GAV] deriving Show

showHierarchy :: ImageFormat -> [ParentChain]  -> IO ()
showHierarchy imageFormat parentChains = do
    shells ("dot -T" <> extension <> " -o hierarchy." <> extension) $ select dotLines
    shells (viewer <> " hierarchy." <> extension) empty
  where
    dotLines = toDotSource parentChains
    (extension, viewer) = case imageFormat of
        PNG -> ("png", "shotwell")
        SVG -> ("svg", "google-chrome")

toDotSource :: [ParentChain] -> [Line]
toDotSource parentChains =
    "digraph G {" : "rankdir=RL" : "node[shape=box]" : edgeLines <> ["}"]
  where
    edgeLines :: [Line]
    edgeLines =
        mapMaybe mkEdgeWithGavs moduleParentPairs

    -- mkEdgeWithArtifactIds :: (GAV, GAV) -> Maybe Line
    -- mkEdgeWithArtifactIds (GAV _ childArtId _, GAV _ parentArtId _) =
    --     textToLine $ repr childArtId <> " -> " <> repr parentArtId

    mkEdgeWithGavs :: (GAV, GAV) -> Maybe Line
    mkEdgeWithGavs (child, parent) =
        textToLine . Text.pack $ show (show child) <> " -> " <> show (show parent)

    moduleParentPairs :: [(GAV, GAV)] -- Name of module paired with it's parent module
    moduleParentPairs =
        nub $ concatMap (\(ParentChain gavs) -> overlappingPairs gavs) parentChains

getParentChains :: IO [ParentChain]
getParentChains = handle errorHandler $ do
    Text.putStrLn $ "Running " <> mvnDisplayAncestors
    export "MAVEN_OPTS" "-Xmx8G" -- Give more memory to maven to avoid "GC overhead limit exceeded" for huge projects
    fold command foldChains

  where
    mvnDisplayAncestors = "mvn org.apache.maven.plugins:maven-dependency-plugin:3.1.1:display-ancestors"

    command :: Shell Line
    command =
        inshell "grep -B5 --group-separator=MY_GROUP_SEPARATOR 'Ancestor POMs:'"
        $ inshell mvnDisplayAncestors empty

    foldChains :: Fold Line [ParentChain]
    foldChains = Fold step initial extract

    step :: ([ParentChain], [Line]) -> Line -> ([ParentChain], [Line])
    step (chains, linesForModule) newLine = case lineToText newLine of
        "MY_GROUP_SEPARATOR" ->
            let newChain = parseModuleParentChain $ reverse linesForModule
            in  (newChain : chains, [])
        _                    -> (chains, newLine:linesForModule)

    initial :: ([ParentChain], [Line])
    initial = ([], [])

    extract :: ([ParentChain], [Line]) -> [ParentChain]
    extract = fst

    errorHandler :: ExitCode -> IO a
    errorHandler exitCode = do
        Text.putStrLn $ Text.unlines
            [ "ERROR: The output from maven didn't contain any info about ancestor POMs"
            , "       Try running the above maven command directly to see what's wrong"
            ]
        exit exitCode

-- [1,2,3,4,5] -> [(1,2), (2,3), (3,4), (4,5)]
overlappingPairs :: [a] -> [(a,a)]
overlappingPairs []       = []
overlappingPairs [_]      = []
overlappingPairs (x:y:zs) = (x,y) : overlappingPairs (y:zs)

parseModuleParentChain :: [Line] -> ParentChain
parseModuleParentChain linez = case linez of
    [groupAndArtifactLine, versionLine, _, _, _, ancestorPomsLine] ->
        ParentChain $ moduleGav : parentGavs
      where
        moduleGav = getModuleGav groupAndArtifactLine versionLine
        parentGavs = getParentGavChain ancestorPomsLine
    _ -> error $ "I was expecting 6 lines, but got " <> unlines (repr <$>linez)

{-
INPUT: 2 lines:
"[INFO] ---------< org.kie.server:kie-server-integ-tests-optaplanner >----------"
"[INFO] Building KIE :: Execution Server :: Tests :: OptaPlanner Integration Tests 7.14.0-SNAPSHOT [62/117]"

OUTPUT: GAV "org.kie.server" "kie-server-integ-tests-optaplanner" "7.14.0-SNAPSHOT"
-}
getModuleGav :: Line -> Line -> GAV
getModuleGav groupAndArtifactLine versionLine =
    GAV group artifact version
  where
    (group, artifact) = case Text.words $ lineToText groupAndArtifactLine of
       [_, _, groupColonArtifact, _] -> case Text.splitOn ":" groupColonArtifact of
           [g, a] -> (g, a)
           _ -> error $ "Failed to parse group and artifact from: " <> repr groupColonArtifact
       _ -> error $ "Failed to parse group and artifact from: " <> repr groupAndArtifactLine
    -- version is the next to last word
    version = last . init . Text.words $ lineToText versionLine

-- INPUT:  "[INFO] Ancestor POMs: org.drools:droolsjbpm-integration:7.14.0-SNAPSHOT <- org.kie:kie-parent:7.14.0-SNAPSHOT <- org.jboss.integration-platform:jboss-integration-platform-bom:8.3.2.Final <- org.jboss.integration-platform:jboss-integration-platform-parent:8.3.2.Final <- org.jboss:jboss-parent:29"
-- OUTPUT: [GAV "org.drools" "droolsjbpm-integration" "7.14.0-SNAPSHOT", GAV "org.kie" "kie-parent" "7.14.0-SNAPSHOT", ...]
getParentGavChain :: Line -> [GAV]
getParentGavChain lineWithParentChain =
    fmap toGav
    . Text.splitOn " <- "
    . Text.drop (Text.length "[INFO] Ancestor POMs: ")
    $ lineToText lineWithParentChain
  where
    toGav gav = case Text.splitOn ":" gav of
        (g:a:v:_) -> GAV g a v
        _         -> error $ "Unexpected GAV: " <> repr gav

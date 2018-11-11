{-# LANGUAGE OverloadedStrings #-}

module Maven.PomAncestors
  ( getAncestorChains
  ) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Control.Exception (handle)
import Control.Foldl (Fold (Fold))
import Maven.Types (GAV (GAV), AncestorChain (AncestorChain))
import Turtle (ExitCode, Line, Shell, empty, exit, export, fold, inshell,
               lineToText, repr)

getAncestorChains :: IO [AncestorChain]
getAncestorChains = handle errorHandler $ do
    Text.putStrLn $ "Running " <> mvnDisplayAncestors
    export "MAVEN_OPTS" "-Xmx8G" -- Give more memory to maven to avoid "GC overhead limit exceeded" for huge projects
    fold command foldChains
  where
    mvnDisplayAncestors = "mvn org.apache.maven.plugins:maven-dependency-plugin:3.1.1:display-ancestors"

    command :: Shell Line
    command =
        inshell "grep -B5 --group-separator=MY_GROUP_SEPARATOR 'Ancestor POMs:'"
        $ inshell mvnDisplayAncestors empty

    foldChains :: Fold Line [AncestorChain]
    foldChains = Fold step initial extract

    step :: ([AncestorChain], [Line]) -> Line -> ([AncestorChain], [Line])
    step (chains, linesForModule) newLine = case lineToText newLine of
        "MY_GROUP_SEPARATOR" ->
            let newChain = parseModuleAncestorChain $ reverse linesForModule
            in  (newChain : chains, [])
        _                    -> (chains, newLine:linesForModule)

    initial :: ([AncestorChain], [Line])
    initial = ([], [])

    extract :: ([AncestorChain], [Line]) -> [AncestorChain]
    extract = fst

    errorHandler :: ExitCode -> IO a
    errorHandler exitCode = do
        Text.putStrLn $ Text.unlines
            [ "ERROR: The output from maven didn't contain any info about ancestor POMs"
            , "       Try running the above maven command directly to see what's wrong"
            ]
        exit exitCode


parseModuleAncestorChain :: [Line] -> AncestorChain
parseModuleAncestorChain linez = case linez of
    [groupAndArtifactLine, versionLine, _, _, _, ancestorPomsLine] ->
        AncestorChain $ moduleGav : parentGavs
      where
        moduleGav = getModuleGav groupAndArtifactLine versionLine
        parentGavs = getAncestorChain ancestorPomsLine
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
getAncestorChain :: Line -> [GAV]
getAncestorChain lineWithAncestorChain =
    fmap toGav
    . Text.splitOn " <- "
    . Text.drop (Text.length "[INFO] Ancestor POMs: ")
    $ lineToText lineWithAncestorChain
  where
    toGav gav = case Text.splitOn ":" gav of
        (g:a:v:_) -> GAV g a v
        _         -> error $ "Unexpected GAV: " <> repr gav

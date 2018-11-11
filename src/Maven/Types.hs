{-# LANGUAGE OverloadedStrings #-}
module Maven.Types
    ( GAV (..)
    , AncestorChain (AncestorChain)
    ) where

import qualified Data.Text as Text

import Data.Text (Text)

data GAV = GAV
   { gavGroupId    :: Text
   , gavArtifactId :: Text
   , gavVersion    :: Text
   } deriving (Eq, Ord)

instance Show GAV where
  show (GAV g a v) = Text.unpack $ Text.intercalate ":" [g,a,v]

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
newtype AncestorChain = AncestorChain [GAV] deriving Show

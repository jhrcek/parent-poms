{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Pom.PropOverrides
    ( PropOverride(..)
    , getOverrides
    , formatOverride
    , isUseless
    ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Maven.Types (AncestorChain (..), GAV (..))
import Pom.Properties (PropKey (PK), PropValue (PV), Properties, propsDeclared)
import Turtle.Format (format, s, (%))

import qualified Data.Map as Map
import qualified Data.Set as Set

data PropOverride = PropOverride
    { propKey     :: PropKey -- | Key of the property being overridden
    , ancestorGav :: GAV -- | GAV of module containing property declaration being overridden
    , gav         :: GAV -- | GAV of module doing the overriding
    , ancestorVal :: PropValue -- | Property value in the ancestor
    , val         :: PropValue -- | Property value in the module
    } deriving (Eq, Ord)

isUseless :: PropOverride -> Bool
isUseless o = ancestorVal o == val o

{-|
Sweep the AncestorChain from the root module towars children module.
For each new module encountered update the map of effective properties `Map PropKey (GAV, PropValue)`.
This map remembers for each PropKey in which GAV that prop value was first defined (or last overridden) and the current value.
-}
getOverrides :: Map GAV Properties -> AncestorChain -> Set PropOverride
getOverrides gavProps (AncestorChain gavs) =
    fst $ foldr collectOverrides (Set.empty, Map.empty) gavs
  where
    collectOverrides
        :: GAV
        -> (Set PropOverride, Map PropKey (GAV, PropValue))
        -> (Set PropOverride, Map PropKey (GAV, PropValue))
    collectOverrides gav' (overrides, effectiveProps) =
        case propsDeclared <$> Map.lookup gav' gavProps of
            Nothing -> (overrides, effectiveProps) -- Nothing => The pom.xml corresponding to the GAV wasn't present in local repo
            Just props ->
                ( Set.union newOverrides overrides
                , Map.union newProps effectiveProps
                )
              where
                newProps = fmap (gav',) props
                newOverrides = Set.fromList . Map.elems $ Map.intersectionWithKey
                    (\propKey' val' (ancestorGav', ancestorVal') -> PropOverride propKey' ancestorGav' gav' ancestorVal' val') props effectiveProps

formatOverride :: PropOverride -> Text
formatOverride PropOverride
    { propKey = (PK pKey)
    , ancestorGav = ancGav
    , gav = curGav
    , ancestorVal = (PV ancVal)
    , val = (PV curVal)
    } = format (s%" overrides '"%s%"' defined in "%s%" from '"%s%"' to '"%s%"'")
          (gavArtifactId curGav) pKey (gavArtifactId ancGav) ancVal curVal

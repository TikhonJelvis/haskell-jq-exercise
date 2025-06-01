{-# HLINT ignore "Use foldM" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | A reference version of the types + functions for the first several steps in Jq
module ReferenceJq where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Vector (Vector, (!?))
import Data.Vector qualified as Vector

type FieldName = Text

-- | A simple JSON type—we'll probably want to switch to Aeson.Value
-- in the future, but this is simpler for now.
data JsonValue
    = Null
    | True
    | False
    | Number Double
    | String Text
    | Array (Vector JsonValue)
    | Object (Map FieldName JsonValue)
    deriving stock (Show, Eq, Ord)

-- * Step One: lookup by keys
type Path = [FieldName]

-- | Get the JSON value by following the given sequence of JSON object
-- field names.
--
-- Return 'Nothing' if any field name along the path is not present.
get :: Path -> JsonValue -> Maybe JsonValue
get [] value = pure value
get (f : fs) value = case value of
    Object object -> do
        next <- Map.lookup f object
        get fs next
    _ -> Nothing

-- * Step Two: lookup by field name or array index

-- | A step for a path query into a JSON value.
data Step = Field FieldName | Index Int
    deriving stock (Show, Eq, Ord)

-- | Get the value at the given step if present in the given JSON
-- value.
getStep :: Step -> JsonValue -> Maybe JsonValue
getStep (Field name) value = case value of
    Object object -> Map.lookup name object
    _ -> Nothing
getStep (Index i) value = case value of
    Array array -> array !? i
    _ -> Nothing

type Path' = [Step]

get' :: Path' -> JsonValue -> Maybe JsonValue
get' [] value = pure value
get' (step : steps) value = do
    next <- getStep step value
    get' steps next

get'Fold :: Path' -> JsonValue -> Maybe JsonValue
get'Fold path value = foldM (flip getStep) value path

-- * Step Three: add error messages?

-- * Step Four + Five: single-step wildcards, alternation

-- getting to have too many 's in identifiers...

data Step'
    = Field' FieldName
    | Index' Int
    | AnyField
    | AnyIndex
    | Any
    | Or Step' Step'
    deriving stock (Show, Eq, Ord)

type Path'' = [Step']

getStep'' :: Step' -> JsonValue -> [JsonValue]
getStep'' (Field' name) value = case value of
    Object object -> maybeToList $ Map.lookup name object
    _ -> []
getStep'' (Index' i) value = case value of
    Array array -> maybeToList $ array !? i
    _ -> []
getStep'' AnyField value = case value of
    Object object -> Map.elems object
    _ -> []
getStep'' AnyIndex value = case value of
    Array array -> Vector.toList array
    _ -> []
getStep'' Any value = case value of
    Object object -> Map.elems object
    Array array -> Vector.toList array
    _ -> []
getStep'' (Or s₁ s₂) value = do
    getStep'' s₁ value <|> getStep'' s₂ value

-- alternatively (++) or (<>) work too!

get'' :: Path'' -> JsonValue -> [JsonValue]
get'' [] value = pure value
get'' (step : steps) value = do
    next <- getStep'' step value
    get'' steps next

get''Fold :: Path'' -> JsonValue -> [JsonValue]
get''Fold path value = foldM (flip getStep'') value path

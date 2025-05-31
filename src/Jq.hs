module Jq where

import Data.Text (Text)

type FieldName = Text

-- | A simple JSON type—we'll probably want to switch to Aeson.Value
-- in the future, but this is simpler for now.
data JsonValue

-- * Step One: lookup by keys
type Path = [FieldName]

-- | Get the JSON value by following the given sequence of JSON object
-- field names.
--
-- Return 'Nothing' if any field name along the path is not present.
get :: Path -> JsonValue -> Maybe JsonValue
get = _

-- * Step Two: lookup by field name or array index

-- * Step Three: add error messages?

-- * Step Four: wildcards

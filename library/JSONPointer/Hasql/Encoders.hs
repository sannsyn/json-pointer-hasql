-- |
-- Extension to the "Hasql.Encoders" module.
-- Contains no conflicting symbols, so it can be
-- imported under the same qualified name, e.g.:
-- 
-- >import qualified Hasql.Encoders as Encoders
-- >import qualified JSONPointer.Hasql.Encoders as Encoders
-- 
module JSONPointer.Hasql.Encoders where

import BasePrelude
import Hasql.Encoders
import Data.Text (Text)
import JSONPointer.Model (JSONPointer)
import qualified JSONPointer.Model


-- |
-- An encoder of JSON Pointer into the @text[]@ type of Postgres.
-- 
-- Can be used to query the JSON columns,
-- as in the following example:
-- 
-- >selectOfJSONByJSONPointer :: Query JSONPointer (Maybe Data.Aeson.Value)
-- >selectOfJSONByJSONPointer =
-- >  statement sql encoder decoder True
-- >  where
-- >    sql =
-- >      "select '{\"1\":[0,1,2,3]}'::jsonb #> $1"
-- >    encoder =
-- >      Encoders.value Encoders.textArrayFromJSONPointer
-- >    decoder =
-- >      Decoders.singleRow $
-- >      Decoders.nullableValue Decoders.jsonb
-- 
textArrayFromJSONPointer :: Value JSONPointer
textArrayFromJSONPointer =
  array $ arrayDimension fold $ arrayValue text
  where
    fold :: (a -> Text -> a) -> a -> JSONPointer -> a
    fold step init jsonPointer =
      appEndo endo id init
      where
        endo =
          JSONPointer.Model.run jsonPointer referenceInterpreter
          where
            referenceInterpreter _ key =
              Endo $ \cont -> \a -> cont $ step a key

module Main where

import Rebase.Prelude
import JSONPointer.Model (JSONPointer)
import Hasql.Query (Query)
import Hasql.Session (Session)

import qualified Hasql.Connection
import qualified Hasql.Session
import qualified Hasql.Query
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified JSONPointer.Parser
import qualified JSONPointer.Model
import qualified JSONPointer.Hasql.Encoders as Encoders
import qualified Data.Aeson


main =
  do
    result <- query jsonPointer selectOfJSONByJSONPointerQuery
    putStrLn $ "Result is: " <> show result
    exitWith $
      if result == Just (Data.Aeson.Number 2)
        then ExitSuccess
        else ExitFailure 1
  where
    query input query =
      either (fail . show) return =<<
      runSession (Hasql.Session.query input query)
    jsonPointer =
      either (error . show) id $
      JSONPointer.Parser.run JSONPointer.Parser.jsonPointer $
      "/1/2"

runSession :: Session a -> IO (Either (Either Hasql.Connection.ConnectionError Hasql.Session.Error) a)
runSession session =
  runEitherT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      EitherT $ fmap (mapLeft Left) $ Hasql.Connection.acquire settings
      where
        settings =
          Hasql.Connection.settings host port user password database
          where
            host = "localhost"
            port = 5432
            user = "postgres"
            password = ""
            database = "postgres"
    use connection =
      EitherT $
      fmap (mapLeft Right) $
      Hasql.Session.run session connection
    release connection =
      lift $ Hasql.Connection.release connection

selectOfJSONByJSONPointerQuery :: Query JSONPointer (Maybe Data.Aeson.Value)
selectOfJSONByJSONPointerQuery =
  Hasql.Query.statement sql encoder decoder True
  where
    sql =
      "select '{\"1\":[0,1,2,3]}'::jsonb #> $1"
    encoder =
      Encoders.value Encoders.textArrayFromJSONPointer
    decoder =
      Decoders.singleRow $
      Decoders.nullableValue Decoders.jsonb

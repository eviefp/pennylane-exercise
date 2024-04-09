{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Importer.Runner
    ( run
    ) where

import Control.Monad.Logger qualified as Log
import Data.Aeson qualified as Aeson
import Database.Persist ((!=.))
import Database.Persist.Class qualified as P
import Database.Persist.Postgresql qualified as P
import Database.Types qualified as Db

-- Parse the json file and replace the database contents with the result.
run :: P.ConnectionString -> IO ()
run conn =
    Aeson.eitherDecodeFileStrict "data/recipes-en.json" >>= \case
        Right result -> process conn result
        Left err -> putStrLn err

-- Since we map the same type `Db.Recipe` to both the JSON result and to the
-- database type, we can just insert them as-is. If there were some kind of
-- processing we needed to do, thid would be the place.
process :: P.ConnectionString -> [Db.Recipe] -> IO ()
process conn recipes =
    Log.runStderrLoggingT
        . P.withPostgresqlConn conn
        $ P.runSqlConn do
            P.runMigration Db.migrateAll
            P.deleteWhere [Db.RecipeTitle !=. ""] -- delete everything
            P.insertMany_ recipes

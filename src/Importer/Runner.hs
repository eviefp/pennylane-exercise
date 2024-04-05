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

run :: P.ConnectionString -> IO ()
run conn =
    Aeson.eitherDecodeFileStrict "data/recipes-en.json" >>= \case
        Right result -> process conn result
        Left err -> putStrLn err

process :: P.ConnectionString -> [Db.Recipe] -> IO ()
process conn recipes =
    Log.runStderrLoggingT
        . P.withPostgresqlConn conn
        $ P.runSqlConn do
            P.runMigration Db.migrateAll
            P.deleteWhere [Db.RecipeTitle !=. ""] -- delete everything
            P.insertMany_ recipes

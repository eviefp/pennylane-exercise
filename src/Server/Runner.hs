{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Server.Runner
    ( run
    , ApplicationConfig (..)
    ) where

import Control.Monad.Logger qualified as Log
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Database.Persist.Postgresql qualified as P
import Database.Types qualified as Db
import Web.Scotty.Trans qualified as Web

data ApplicationConfig = ApplicationConfig
    { connectionString :: P.ConnectionString
    , poolSize :: Int
    , port :: Int
    }

run :: ApplicationConfig -> IO ()
run ApplicationConfig {..} =
    Log.runStderrLoggingT
        . P.withPostgresqlPool connectionString poolSize
        $ \pool ->
            Web.scottyT port (`P.runSqlPersistMPool` pool) do
                Web.get "/hello" do
                    Web.text "hello, world"
                Web.get "/index.html" do
                    Web.setHeader "Content-Type" "text/html"
                    Web.file "./html/index.html"
                Web.post "/api/recipe/byIngredients" do
                    let
                        ingredientMatch i = P.PersistText $ "%" <> i <> "%"
                    ingredients <- Web.jsonData @[Text]
                    let
                        sql = "select ?? from recipe where ingredients like ALL(?)"
                    result <-
                        lift $ P.rawSql @(P.Entity Db.Recipe) sql [P.PersistArray (ingredientMatch <$> ingredients)]
                    Web.json $ P.entityVal <$> result

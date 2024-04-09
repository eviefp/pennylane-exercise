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
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Persist.Postgresql qualified as P
import Database.Types qualified as Db
import Web.Scotty.Trans qualified as Web

data ApplicationConfig = ApplicationConfig
    { connectionString :: ByteString
    , poolSize :: Int
    , port :: Int
    }

-- Run the web server, using stderr logging and a postgres connection pool.
run :: ApplicationConfig -> IO ()
run ApplicationConfig {..} =
    Log.runStderrLoggingT
        . P.withPostgresqlPool connectionString poolSize
        $ \pool ->
            Web.scottyT port (`P.runSqlPersistMPool` pool) do
                Web.get "/ping" do
                    Web.text "pong"
                Web.get "/" do
                    Web.setHeader "Content-Type" "text/html"
                    Web.file "./html/index.html"
                Web.get "/index.html" do
                    Web.setHeader "Content-Type" "text/html"
                    Web.file "./html/index.html"
                Web.get "/index.js" do
                    Web.setHeader "Content-Type" "text/javascript"
                    Web.file "./html/index.js"
                -- post with json string array of ingredients
                -- responds with the list of matching recipes, or an empty list
                Web.post "/api/recipe/byIngredients" do
                    let
                        ingredientMatch i = P.PersistText $ "%" <> i <> "%"
                    ingredients <- Web.jsonData @[Text]
                    let
                        -- this is fast enough for the current database size
                        sql = "select ?? from recipe where ingredients like ALL(?)"
                    result <-
                        lift $ P.rawSql @(P.Entity Db.Recipe) sql [P.PersistArray (ingredientMatch <$> ingredients)]
                    Web.json $ P.entityVal <$> result

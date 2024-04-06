{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.ByteString.Char8 (pack)
import Database.Persist.Postgresql qualified as P
import Importer.Runner qualified as Importer
import Server.Runner qualified as Server
import System.Environment (getArgs, lookupEnv)

main :: IO ()
main = do
    lookupEnv "DATABASE_URL" >>= \case
        Nothing -> putStrLn "please set DATABASE_URL"
        Just rawConn -> do
            let
                conn = pack rawConn
            getArgs >>= \case
                ["import"] -> Importer.run conn
                _ -> Server.run (mkDefaultSettings conn)

mkDefaultSettings :: P.ConnectionString -> Server.ApplicationConfig
mkDefaultSettings connectionString =
    let
        poolSize = 10
        port = 8888
    in
        Server.ApplicationConfig {..}

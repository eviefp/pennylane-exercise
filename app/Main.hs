{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Importer.Runner qualified as Importer
import Server.Runner qualified as Server
import System.Environment (lookupEnv)

main :: IO ()
main = do
    lookupEnv "DATABASE_URL" >>= \case
        Nothing -> putStrLn "please set DATABASE_URL"
        Just rawConn -> do
            let
                conn = pack rawConn
            Importer.run conn *> Server.run (mkDefaultSettings conn)

mkDefaultSettings :: ByteString -> Server.ApplicationConfig
mkDefaultSettings connectionString =
    let
        poolSize = 10
        port = 8888
    in
        Server.ApplicationConfig {..}

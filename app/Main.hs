{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 (pack)
import Importer.Runner qualified as Runner
import System.Environment (lookupEnv)

main :: IO ()
main = do
    lookupEnv "DATABASE_URL" >>= \case
        Nothing -> putStrLn "please set DATABASE_URL"
        Just conn -> Runner.run $ pack conn

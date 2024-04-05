{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Types where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
Recipe
  title Text
  cook_time Int
  prep_time Int
  ratings Double
  ingredients [Text]
  deriving Generic
|]

instance Aeson.FromJSON Recipe where
    parseJSON = Aeson.withObject "Recipe" $ \v ->
        Recipe
            <$> v .: "title"
            <*> v .: "cook_time"
            <*> v .: "prep_time"
            <*> v .: "ratings"
            <*> v .: "ingredients"

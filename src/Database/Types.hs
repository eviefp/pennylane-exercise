{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Types where

import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)

{-
Persistent's DSL for defining database tables/entities.

One could arguably try to parse each ingredient into a separate table,
perhaps separating "quantity", "unit of measurement", "main ingredient"
(or even "alternative" or "details"). Most of the provided json seem to
adhere to a pattern, but there's quite a few exceptions.

Instead, I opted for a simple Text field that can later be parsed into a
List (this is a Persistent specific feature and does not use Postgres'
Array feature).
-}
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

instance Aeson.ToJSON Recipe where
    toJSON Recipe {..} =
        Aeson.object
            [ "title" .= recipeTitle
            , "cook_time" .= recipeCook_time
            , "prep_time" .= recipePrep_time
            , "ratings" .= recipeRatings
            , "ingredients" .= recipeIngredients
            ]

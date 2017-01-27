{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mellon.Web.SwaggerAPISpec (spec) where

import Mellon.Web.Server (State(..), Time(..), mellonAPI, mellonSwagger)

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as C8 (readFile)
import Paths_mellon_web
import Servant.Swagger.Test
import Test.Hspec
import Test.QuickCheck
       (Arbitrary(..), elements, genericShrink, oneof, property)
import Test.QuickCheck.Instances ()

spec :: Spec
spec =
  do describe "Swagger" $
       do context "ToJSON matches ToSchema" $
            validateEveryToJSON mellonAPI
          context "swagger.json" $
            it "matches current file contents" $
              do path <- getDataFileName "swagger.json"
                 swagger <- eitherDecode <$> C8.readFile path
                 swagger `shouldBe` Right mellonSwagger

instance Arbitrary State where
  arbitrary = oneof [pure Locked, Unlocked <$> arbitrary]
  shrink = genericShrink

instance Arbitrary Time where
  arbitrary = Time <$> arbitrary
  shrink = genericShrink

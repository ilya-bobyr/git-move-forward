{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GetBranchesSpec (spec) where

-- import Run (getBranches)
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "getBranches upstream filtering" $ do
    it "returns everything for one upstream" $ do
      -- TODO Need to setup a temporary folder with a git repo
      True `shouldBe` True

    it "returns correct branches for two upstreams" $ do
      -- TODO Need to setup a temporary folder with a git repo
      True `shouldBe` True

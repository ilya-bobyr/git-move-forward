{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GitOutputParserSpec (spec) where

import GitOutputParser (UpstreamInfo (UpstreamInfo), branchInfoParser)
import Import
import Test.Hspec
import Text.Parsec (runParser)

-- import Test.Hspec.QuickCheck

-- Helper to make the expectation lists a bit shorter.
mkBranchInfo ::
  Bool ->
  Text ->
  Text ->
  Maybe UpstreamInfo ->
  (Bool, Text, Text, Maybe UpstreamInfo)
mkBranchInfo isCurrent name hash upstream = (isCurrent, name, hash, upstream)

spec :: Spec
spec = do
  describe "branchInfoParser can parse `git branch` output" $ do
    it "empty input is invalid" $ do
      let input = ""
          actual = runParser branchInfoParser () "test input" input

          -- TODO An alternative to using text, we can use structured error.
          -- And it actually process a nicer mismatch message.  But the problem
          -- is that the error structure depends on the grammar structure.  On
          -- one hand, it is expected, but it is not ideal as a test expectation
          -- expression.
          --
          -- expected =
          --   foldl'
          --     (flip addErrorMessage)
          --     (newErrorUnknown $ newPos "test input" 1 1)
          --     [ Expect "white space",
          --       SysUnExpect "",
          --       Expect "\"*\"",
          --       SysUnExpect "",
          --       Expect "white space",
          --       SysUnExpect "",
          --       SysUnExpect ""
          --     ]

          expected =
            "\"test input\" (line 1, column 1):\n\
            \unexpected end of input\n\
            \expecting white space or \"*\""

      mapLeft show actual `shouldBe` Left expected

    it "parses common branch specs" $ do
      let input =
            [ "  branch-a        361 [upstream/master: ahead 1] Branch A commit 1",
              "  branch-b       ce09 [upstream/master: ahead 1, behind 4] Branch A commit 1",
              "  branch-c        db2 [upstream/master] Branch C",
              "* branch-d        f2d [upstream/master: ahead 11] Branch D commit 1",
              "  path/branch-e   13c Branch E commit 1"
            ]
          actual = fmap (runParser branchInfoParser () "test input") input
          expected =
            [ Right $
                mkBranchInfo False "branch-a" "361" $
                  Just $
                    UpstreamInfo "upstream/master" (Just 1) Nothing,
              Right $
                mkBranchInfo False "branch-b" "ce09" $
                  Just $
                    UpstreamInfo "upstream/master" (Just 1) (Just 4),
              Right $
                mkBranchInfo False "branch-c" "db2" $
                  Just $
                    UpstreamInfo "upstream/master" Nothing Nothing,
              Right $
                mkBranchInfo True "branch-d" "f2d" $
                  Just $
                    UpstreamInfo "upstream/master" (Just 11) Nothing,
              Right $
                mkBranchInfo False "path/branch-e" "13c" Nothing
            ]

      actual `shouldBe` expected

-- it "parses " $ plus2 maxBound `shouldBe` minBound + 1
-- prop "minus 2" $ \i -> plus2 i - 2 `shouldBe` i

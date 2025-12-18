module ConfigParserSpec (spec) where

import Config
  ( Config
      ( configCheckoutBranch,
        configForceMoveMain,
        configMainName,
        configOriginName,
        configUpstreamName,
        configVerbose
      ),
    configParser,
  )
import Data.Default (def)
import Import
import Test.Hspec
import Text.Parsec (runParser)

checkParsing :: Text -> Config -> Expectation
checkParsing input expected =
  let actual = runParser (configParser def) () "test input" input
   in actual `shouldBe` Right expected

checkError :: Text -> String -> Expectation
checkError input expectedError = do
  let actual = runParser (configParser def) () "test input" input
   in mapLeft show actual `shouldBe` Left expectedError

spec :: Spec
spec = do
  describe "configParser can parse configs" $ do
    it "returns default Config for empty input" $
      checkParsing "" def

    it "requires a \"move-forward.\" prefix for values" $
      checkError
        "verbose yes"
        "\"test input\" (line 1, column 12):\n\
        \unexpected end of input\n\
        \Config lines are expected to have a \"move-forward.\" prefix. \
        \Got: verbose yes"

    it "can parse \"verbose\" option \"yes\"" $ do
      checkParsing "move-forward.verbose yes" $
        def {configVerbose = True}

    it "can parse \"verbose\" option \"false\"" $ do
      checkParsing "move-forward.verbose false" $
        def {configVerbose = False}

    it "can parse \"main\" option" $ do
      checkParsing "move-forward.main branch name with spaces?" $
        def {configMainName = "branch name with spaces?"}

    it "can parse \"origin\" option" $ do
      checkParsing "move-forward.origin myRepo" $
        def {configOriginName = "myRepo"}

    it "can parse \"upstream\" option" $ do
      checkParsing "move-forward.upstream reference-repo" $
        def {configUpstreamName = "reference-repo"}

    it "can parse \"switch\" option" $ do
      checkParsing "move-forward.switch the-very-main-branch" $
        def {configCheckoutBranch = Just "the-very-main-branch"}

    it "can parse \"force-move-main\" option \"true\"" $ do
      checkParsing "move-forward.force-move-main true" $
        def {configForceMoveMain = True}

    it "can parse \"force-move-main\" option \"no\"" $ do
      checkParsing "move-forward.force-move-main no" $
        def {configForceMoveMain = False}

module GitOutputParser
  ( branchInfoParser,
    BranchCheckoutState (..),
    BranchInfo (..),
    UpstreamInfo (UpstreamInfo, uiName, uiAhead, uiBehind),
  )
where

import Data.Text qualified as T
import Import
import RIO.Char (isSpace)
import Text.Parsec (many1, optionMaybe)
import Text.Parsec qualified as P
import Text.Parsec.Char (char, digit, noneOf, oneOf, satisfy, space, spaces, string)
import Text.Parsec.Text (Parser)

data BranchCheckoutState
  = NotCheckedOut
  | InCurrentWorkspace
  | InAnotherWorkspace
  deriving (Eq, Show)

data UpstreamInfo = UpstreamInfo
  { uiName :: !Text,
    uiAhead :: !(Maybe Int),
    uiBehind :: !(Maybe Int)
  }
  deriving (Eq, Show)

data BranchInfo = BranchInfo
  { branchInfoCheckoutState :: !BranchCheckoutState,
    branchInfoName :: !Text,
    branchInfoHash :: !Text,
    branchInfoWorktree :: !(Maybe Text),
    branchInfoUpstream :: !(Maybe UpstreamInfo)
  }
  deriving (Eq, Show)

branchInfoParser :: Parser BranchInfo
branchInfoParser = do
  let nonWs = many1 $ satisfy (not . isSpace)

      checkoutState :: Parser BranchCheckoutState
      checkoutState =
        asum
          [ char '*' $> InCurrentWorkspace,
            char '+' $> InAnotherWorkspace,
            char ' ' $> NotCheckedOut
          ]

      worktreePath :: Parser Text
      worktreePath = do
        _ <- char '('
        path <- many1 $ noneOf ")"
        _ <- char ')'
        pure $ T.pack path

      upstreamInfo :: Parser UpstreamInfo
      upstreamInfo = do
        _ <- char '['
        name <- many1 $ noneOf ":]"
        (ahead, behind) <- branchState
        _ <- char ']'
        pure $ UpstreamInfo (T.pack name) ahead behind

      branchState :: Parser (Maybe Int, Maybe Int)
      branchState = do
        ahead <- optionMaybe $
          P.try $ do
            _ <- string ": ahead "
            count <- many1 digit
            return $ readMaybe count

        behind <- optionMaybe $
          P.try $ do
            _ <- oneOf ":,"
            _ <- string " behind "
            count <- many1 digit
            return $ readMaybe count

        pure (join ahead, join behind)

  checkout <- checkoutState
  space *> spaces
  name <- nonWs
  space *> spaces
  hash <- nonWs
  worktree <- optionMaybe $
    P.try $ do
      space *> spaces
      worktreePath
  upstream <- optionMaybe $
    P.try $ do
      space *> spaces
      upstreamInfo
  spaces

  return $
    BranchInfo
      checkout
      (T.pack name)
      (T.pack hash)
      worktree
      upstream

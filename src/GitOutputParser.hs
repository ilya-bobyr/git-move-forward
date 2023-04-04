module GitOutputParser
  ( branchInfoParser,
    BranchInfo,
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

data UpstreamInfo = UpstreamInfo
  { uiName :: !Text,
    uiAhead :: !(Maybe Int),
    uiBehind :: !(Maybe Int)
  }
  deriving (Eq, Show)

type BranchInfo = (Bool, Text, Text, Maybe UpstreamInfo)

branchInfoParser :: Parser BranchInfo
branchInfoParser = do
  let nonWs = many1 $ satisfy (not . isSpace)

      upstreamInfo :: Parser UpstreamInfo
      upstreamInfo = do
        _ <- char '['
        name <- many1 $ noneOf ":]"
        (ahead, behind) <- branchState
        char ']' *> space *> spaces
        pure $ UpstreamInfo (T.pack name) ahead behind

      branchState :: Parser (Maybe Int, Maybe Int)
      branchState = do
        ahead <- optionMaybe $ P.try $ do
          _ <- string ": ahead "
          count <- many1 digit
          return $ readMaybe count

        behind <- optionMaybe $ P.try $ do
          _ <- oneOf ":,"
          _ <- string " behind "
          count <- many1 digit
          return $ readMaybe count

        pure (join ahead, join behind)

  spaces
  isCurrent <- (char '*' $> True) <|> return False
  spaces
  name <- nonWs
  space *> spaces
  hash <- nonWs
  space *> spaces
  upstream <- optionMaybe $ P.try upstreamInfo
  return (isCurrent, T.pack name, T.pack hash, upstream)

module Config
  ( Config (..),
    ConfigBranches,
    BranchConfig (..),
    configParser,
    configLineParser,

    -- * Testing *
    parseValue,
    boolParser,
  )
where

import Data.Char (isSpace)
import Data.Default (Default, def)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import RIO
  ( Bool (False, True),
    Eq,
    Maybe (Just, Nothing),
    Show,
    String,
    Text,
    asum,
    drop,
    dropWhile,
    guard,
    not,
    return,
    reverse,
    takeWhile,
    ($),
    ($>),
    (*>),
    (.),
    (/=),
    (<$>),
    (<>),
    (<|>),
    (==),
    (>>=),
  )
import Text.Parsec (eof, many1, parserFail)
import Text.Parsec qualified as P
import Text.Parsec.Char
  ( newline,
    satisfy,
    space,
    spaces,
    string,
  )
import Text.Parsec.Text (Parser)

newtype BranchConfig = BranchConfig
  { branchConfigSkip :: Bool
  }
  deriving (Eq, Show)

instance Default BranchConfig where
  def = BranchConfig {branchConfigSkip = False}

type ConfigBranches = Map Text BranchConfig

data Config = Config
  { configVerbose :: !Bool,
    configMainName :: !Text,
    configOriginName :: !Text,
    configUpstreamName :: !Text,
    configForceMoveMain :: !Bool,
    configCheckoutBranch :: !(Maybe Text),
    configBranches :: !ConfigBranches
  }
  deriving (Eq, Show)

instance Default Config where
  def =
    Config
      { configVerbose = False,
        configMainName = "master",
        configOriginName = "origin",
        configUpstreamName = "upstream",
        configForceMoveMain = False,
        configCheckoutBranch = Nothing,
        configBranches = M.empty
      }

boolParser :: Parser Bool
boolParser =
  asum
    [ (string "yes" <|> string "true" <|> string "on") $> True,
      (string "no" <|> string "false" <|> string "off") $> False
    ]

textValueParser :: Parser Text
textValueParser =
  T.pack <$> many1 (satisfy (/= '\n'))

configParser :: Config -> Parser Config
configParser config =
  let emptyInput = P.try $ spaces *> eof $> config
   in emptyInput <|> (configLineParser config >>= configParser)

configLineParser :: Config -> Parser Config
configLineParser config = do
  let parseVerbose =
        parseValue "verbose" boolParser $
          \c v -> c {configVerbose = v}
      parseMainName =
        parseValue "main" textValueParser $
          \c v -> c {configMainName = v}
      parseOriginName =
        parseValue "origin" textValueParser $
          \c v -> c {configOriginName = v}
      parseUpstreamName =
        parseValue "upstream" textValueParser $
          \c v -> c {configUpstreamName = v}
      parseForceMoveMain =
        parseValue "force-move-main" boolParser $
          \c v -> c {configForceMoveMain = v}
      parseCheckoutBranch =
        parseValue "switch" textValueParser $
          \c v -> c {configCheckoutBranch = Just v}

  _ <-
    string "move-forward."
      <|> do
        unexpected <- textValueParser
        parserFail $
          "Config lines are expected to have a \"move-forward.\" prefix.\
          \ Got: "
            <> T.unpack unexpected

  config' <-
    asum
      [ parseVerbose config,
        parseMainName config,
        parseOriginName config,
        parseUpstreamName config,
        parseForceMoveMain config,
        parseCheckoutBranch config,
        branchConfigParser config
      ]
      <|> do
        unexpected <- textValueParser
        parserFail $
          "Unexpected config key or value format: " <> T.unpack unexpected

  eof
  return config'

parseValue ::
  String ->
  Parser a ->
  (Config -> a -> Config) ->
  (Config -> Parser Config)
parseValue key valueParser applyValue config = do
  value <- P.try $ string key *> space *> spaces *> valueParser
  (newline $> ()) <|> eof
  return $ applyValue config value

splitNameFromKey :: String -> (String, String)
splitNameFromKey nameAndKey =
  let backwards = reverse nameAndKey
      key = reverse $ takeWhile (/= '.') backwards
      name = reverse $ drop 1 $ dropWhile (/= '.') backwards
   in (name, key)

branchConfigParser :: Config -> Parser Config
branchConfigParser config = do
  _ <- string "branch."

  (name, key) <- splitNameFromKey <$> many1 (satisfy $ not . isSpace)
  guard $ key == "skip"

  _ <- space *> spaces
  skip <- boolParser

  (newline $> ()) <|> eof

  let branches' =
        M.insertWith
          (\_new old -> old {branchConfigSkip = skip})
          (T.pack name)
          (def {branchConfigSkip = skip})
          (configBranches config)

  return $ config {configBranches = branches'}

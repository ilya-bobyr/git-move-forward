module Config
  ( Config (..),
    configParser,
    configLineParser,

    -- * Testing *
    parseValue,
    boolParser,
  )
where

import Data.Default (Default, def)
import Data.Text qualified as T
import RIO
  ( Bool (False, True),
    Eq,
    Maybe (Just, Nothing),
    Show,
    String,
    Text,
    asum,
    return,
    ($),
    ($>),
    (*>),
    (/=),
    (<$>),
    (<>),
    (<|>),
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

data Config = Config
  { configVerbose :: !Bool,
    configMainName :: !Text,
    configOriginName :: !Text,
    configUpstreamName :: !Text,
    configForceMoveMain :: !Bool,
    configCheckoutBranch :: !(Maybe Text)
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
        configCheckoutBranch = Nothing
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
        parseCheckoutBranch config
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

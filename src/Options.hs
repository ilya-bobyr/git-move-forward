module Options
  ( optionsParser,
  )
where

-- showDefault,

import Config
  ( Config
      ( configCheckoutBranch,
        configForceMoveMain,
        configMainName,
        configOriginName,
        configUpstreamName,
        configVerbose
      ),
  )
import Control.Applicative (optional)
import Data.Function (($))
import Data.Maybe (Maybe (Just), maybe)
import Data.Traversable (sequenceA)
import Options.Applicative.Simple
  ( flag',
    help,
    long,
    metavar,
    short,
    showDefaultWith,
    strOption,
  )
import Options.Applicative.Types (Parser)
import RIO (Bool (True), const, (<$>), (<>))

optionalArg :: (Config -> a -> Config) -> Parser a -> Parser (Config -> Config)
optionalArg applyValue argParser =
  (\maybeValue config -> maybe config (applyValue config) maybeValue)
    <$> optional argParser

optionsParser :: Parser [Config -> Config]
optionsParser =
  sequenceA
    [ optionalArg (\c v -> c {configVerbose = v}) $
        flag'
          True
          ( long "verbose"
              <> short 'v'
              <> help "Verbose output?"
              <> showDefaultWith (const "no")
          ),
      optionalArg (\c v -> c {configMainName = v}) $
        strOption
          ( long "main"
              <> short 'm'
              <> help
                "Name of the main development branch.  Used to\
                \ construct the reference point for synchronization."
              <> metavar "MAIN-BRANCH"
              <> showDefaultWith (const "master")
          ),
      optionalArg (\c v -> c {configOriginName = v}) $
        strOption
          ( long "origin"
              <> short 'o'
              <> help
                "Name of the origin remote.  This is the repository on\
                \ GitHub containing your code.  Used to construct a\
                \ target for the forced update of the main branch."
              <> metavar "REMOTE"
              <> showDefaultWith (const "origin")
          ),
      optionalArg (\c v -> c {configUpstreamName = v}) $
        strOption
          ( long "upstream"
              <> short 'u'
              <> help
                "Name of the upstream remote.  This is the repository\
                \ you are getting updates from.  The update will only\
                \ affect branches that use this repository as their\
                \ \"upstream\" branch.\n\
                \ If --force-move-main is set, your main branch will\
                \ be moved to point to the main branch of this\
                \ repository."
              <> metavar "REMOTE"
              <> showDefaultWith (const "upstream")
          ),
      optionalArg (\c v -> c {configForceMoveMain = v}) $
        flag'
          True
          ( long "force-move-main"
              <> short 'M'
              <> help
                "Force move your origin repository main branch to\
                \ point to the main branch of the upstream repo, after\
                \ all the branches are rebased."
              <> showDefaultWith (const "no")
          ),
      optionalArg (\c v -> c {configCheckoutBranch = Just v}) $
        strOption
          ( long "switch"
              <> short 's'
              <> help
                "If specified, switch to this branch at the end.  If not\
                \ specified, keeps currently checkout out branch."
              <> metavar "BRANCH"
              <> showDefaultWith (const "NONE")
          )
    ]

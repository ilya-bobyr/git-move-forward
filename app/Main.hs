{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options (Options (Options), optionsVerbose)
import Options.Applicative.Simple
  ( empty,
    help,
    long,
    metavar,
    short,
    showDefault,
    simpleOptions,
    simpleVersion,
    strOption,
    switch,
    value,
  )
import Paths_git_move_forward qualified
import RIO.Process (mkDefaultProcessContext)
import Run (run)

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_git_move_forward.version)
      "Rebases local branches after a remote branch update"
      "Git workflow helper.  After fetching updates for a remote branch, \
      \this command can rebase all the local branches that have this remote \
      \branch as their upstream."
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
          <*> strOption
            ( long "main"
                <> short 'm'
                <> help
                  "Name of the main development branch.  Used to\
                  \ construct the reference point for synchronization."
                <> metavar "MAIN-BRANCH"
                <> value "master"
                <> showDefault
            )
          <*> strOption
            ( long "origin"
                <> short 'o'
                <> help
                  "Name of the origin remote.  This is the repository on\
                  \ GitHub containing your code.  Used to construct a\
                  \ target for the forced update of the main branch."
                <> metavar "REMOTE"
                <> value "origin"
                <> showDefault
            )
          <*> strOption
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
                <> value "upstream"
                <> showDefault
            )
          <*> switch
            ( long "force-move-main"
                <> short 'M'
                <> help
                  "Force move your origin repository main branch to\
                  \ point to the main branch of the upstream repo, after\
                  \ all the branches are rebased."
            )
          <*> optional
            ( strOption
                ( long "switch"
                    <> short 's'
                    <> help
                      "If specified, switch to this branch at the end.  If not\
                      \ specified, keeps currently checkout out branch."
                    <> metavar "BRANCH"
                )
            )
      )
      empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app $ run options

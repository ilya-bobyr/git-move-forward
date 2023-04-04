{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options
  ( Options (Options),
    optionsCheckoutBranch,
    optionsVerbose,
  )
import Options.Applicative.Simple
  ( empty,
    help,
    long,
    metavar,
    short,
    simpleOptions,
    simpleVersion,
    strArgument,
    switch,
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
          <*> optional
            ( strArgument
                ( help "If specified, checkout this branch at the end"
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
     in runRIO app $ run $ optionsCheckoutBranch options

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Config (configLineParser, configVerbose)
import Control.Applicative (empty)
import Control.Exception (tryJust)
import Data.Default (def)
import Import hiding (tryJust)
import Options (optionsParser)
import Options.Applicative.Simple (simpleOptions, simpleVersion)
import Paths_git_move_forward qualified
import RIO.Process (mkDefaultProcessContext)
import Run (run)
import Text.Parsec (parse)
import Turtle (Fold (Fold), inproc, lineToText, reduce)

main :: IO ()
main = do
  config <- readConfigFromGit >>= applyArgumentsToConfig
  lo <- logOptionsHandle stderr (configVerbose config)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appConfig = config
            }
     in runRIO app $ run config

applyArgumentsToConfig :: Config -> IO Config
applyArgumentsToConfig config = do
  (applyOptions, ()) <-
    simpleOptions
      $(simpleVersion Paths_git_move_forward.version)
      "Rebases local branches after a remote branch update"
      "Git workflow helper.  After fetching updates for a remote branch, \
      \this command can rebase all the local branches that have this remote \
      \branch as their upstream."
      optionsParser
      empty

  return $ foldl' (&) config applyOptions

readConfigFromGit :: IO Config
readConfigFromGit = do
  let processLine =
        Fold
          ( \cases
              (Left err) _line -> Left err
              (Right config) line ->
                parse (configLineParser config) "git config" $ lineToText line
          )
          (return def)
          id

  configOrError <-
    tryJust (\(e :: ExitCode) -> Just e) $
      reduce processLine $
        inproc
          "git"
          [ "config",
            "get",
            "--show-names",
            "--all",
            "--regex",
            "move-forward\\..*"
          ]
          mempty

  case configOrError of
    Left (ExitFailure 1) ->
      -- No configuration specified
      return def
    Left err ->
      error $
        "Failed to run git.\n\
        \Error: "
          <> show err
    Right (Left err) ->
      error $
        "Failed to parse git config for move-forward.\n\
        \Error: "
          <> show err
    Right (Right config) -> return config

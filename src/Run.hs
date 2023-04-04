{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Control.Foldl qualified as L
import Control.Monad.Extra (whenJust)
import Data.Text qualified as T
import GitOutputParser
  ( BranchInfo
      ( BranchInfo,
        branchInfoIsCurrent,
        branchInfoName,
        branchInfoUpstream
      ),
    UpstreamInfo (uiName),
    branchInfoParser,
  )
import Import
import Options
  ( Options
      ( Options,
        optionsCheckoutBranch,
        optionsForceMoveMain,
        optionsMainName,
        optionsOriginName,
        optionsUpstreamName
      ),
  )
import RIO.List (find)
import Text.Parsec (runParser)
import Turtle (Line, Shell, inproc, lineToText, procs, reduce, sh)
import Turtle.Format (format, printf, s, w, (%))

run :: Options -> RIO App ()
run = sh . moveForward

moveForward :: Options -> Shell ()
moveForward
  Options
    { optionsMainName = main,
      optionsOriginName = origin,
      optionsUpstreamName = upstream,
      optionsForceMoveMain = forceMoveMain,
      optionsCheckoutBranch = checkoutBranch
    } = do
    let targetUpstream = upstream <> "/" <> main

    (branches, currentBranch) <- getBranches (== main) targetUpstream

    printf "git-move-forward: Just blindly rebasing...\n"
    printf "\n"

    if null branches
      then printf "=== No branches to update\n"
      else do
        printf "=== Going to process these branches: \n"
        mapM_ (printf ("  " % s % "\n")) branches

    printf "\n"

    forM_ branches \branch -> do
      procs "git" ["checkout", branch] mempty
      procs "git" ["rebase"] mempty

    when forceMoveMain $ do
      printf
        ("Forcing " % s % "/" % s % " to match " % s % "/" % s % "\n")
        origin
        main
        upstream
        main
      -- Entering a detached state, in case we are currently on `main`, before
      -- we force update it.
      procs "git" ["checkout", "--detach", main] mempty
      procs
        "git"
        ["branch", "-f", main, format (s % "/" % s) upstream main]
        mempty
      procs "git" ["push", "-f", origin, main] mempty

    whenJust (asum [checkoutBranch, currentBranch]) \v ->
      procs "git" ["checkout", v] mempty

type CurrentBranch = Maybe Text

type Branches = [Text]

getBranches :: (Text -> Bool) -> Text -> Shell (Branches, CurrentBranch)
getBranches skipByName targetUpstream = do
  output <-
    reduce L.list $
      inproc
        "git"
        ["branch", "--list", "--no-column", "--verbose", "--verbose"]
        mempty

  let parseLine :: Line -> BranchInfo
      parseLine line =
        case runParser branchInfoParser () "git branch" $ lineToText line of
          Right branchInfo -> branchInfo
          Left err ->
            error $
              T.unpack $
                format ("Failed to parse " % w % ": " % w) line err

      keepBranches :: BranchInfo -> Bool
      keepBranches
        BranchInfo
          { branchInfoName = name,
            branchInfoUpstream = upstream
          } =
          let upstreamMatches = fromMaybe False $ do
                upstreamName <- uiName <$> upstream
                pure $ upstreamName == targetUpstream
           in not (skipByName name) && upstreamMatches

      branches = parseLine <$> output

      branchNames = branchInfoName <$> filter keepBranches branches
      currentBranch =
        branchInfoName <$> find branchInfoIsCurrent branches

  pure (branchNames, currentBranch)

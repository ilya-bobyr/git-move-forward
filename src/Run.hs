{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run
  ( run,

    -- * Testing *
    getBranches,
  )
where

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

    (branches', currentBranch) <- getBranches (== main) targetUpstream

    -- Process `currentBranch` last, if it is part of the process list.  This
    -- way it will show up in the log at the very top.
    let branches = maybeMoveBranchToBack currentBranch branches'

    printf "git-move-forward: Just blindly rebasing...\n"
    printf "\n"

    when (null branches) $ do
      printf "=== No branches to update\n\n"
      mzero

    printf "=== Going to process these branches: \n"
    forM_ branches $ \branch -> do
      let name = branchInfoName branch
      printf ("  " % s % " (upstream \"" % s % "\")\n") name targetUpstream
    printf "\n"

    forM_ branches \branch -> do
      let name = branchInfoName branch
      printf ("= Rebasing " % s % "\n") name
      procs "git" ["checkout", name] mempty
      procs "git" ["rebase"] mempty
      printf "\n"

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

type Branches = [BranchInfo]

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

      isTarget :: BranchInfo -> Bool
      isTarget
        BranchInfo
          { branchInfoName = name,
            branchInfoUpstream = upstream
          } =
          let upstreamMatches = fromMaybe False $ do
                upstreamName <- uiName <$> upstream
                pure $ upstreamName == targetUpstream
           in not (skipByName name) && upstreamMatches

      allBranches = parseLine <$> output

      targetBranches = filter isTarget allBranches
      currentBranch =
        branchInfoName <$> find branchInfoIsCurrent allBranches

  pure (targetBranches, currentBranch)

moveBranchToBack :: Text -> [BranchInfo] -> [BranchInfo]
moveBranchToBack x xs =
  let targetBranch = (== x) . branchInfoName
   in if any targetBranch xs
        then filter (not . targetBranch) xs ++ filter targetBranch xs
        else xs

maybeMoveBranchToBack :: Maybe Text -> [BranchInfo] -> [BranchInfo]
maybeMoveBranchToBack (Just x) xs = moveBranchToBack x xs
maybeMoveBranchToBack Nothing xs = xs

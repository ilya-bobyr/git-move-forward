{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Control.Foldl qualified as L
import Control.Monad.Extra (whenJust)
import Data.Text qualified as T
import GitOutputParser (BranchInfo, branchInfoParser)
import Import
import RIO.List (find)
import Text.Parsec (runParser)
import Turtle (Line, Shell, echo, inproc, lineToText, procs, reduce, sh)
import Turtle.Format (format, printf, s, w, (%))

run :: Maybe Text -> RIO App ()
run workingOn = sh $ moveForward workingOn

moveForward :: Maybe Text -> Shell ()
moveForward workingOn = do
  let main = "master"
      origin = "origin"
      upstream = "upstream"

  (branches, currentBranch) <- getBranches (== main)

  echo "git-move-forward: Just blindly rebasing..."

  printf "Going to process these branches: \n"
  mapM_ (printf ("  " % s % "\n")) branches

  forM_ branches \branch -> do
    procs "git" ["checkout", branch] mempty
    procs "git" ["rebase"] mempty

  procs "git" ["branch", "-f", main, format (s % "/" % s) upstream main] mempty
  procs "git" ["push", "-f", origin, main] mempty

  whenJust (asum [workingOn, currentBranch]) \v ->
    procs "git" ["checkout", v] mempty

type CurrentBranch = Maybe Text

type Branches = [Text]

getBranches :: (Text -> Bool) -> Shell (Branches, CurrentBranch)
getBranches skipByName = do
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
      keepBranches (_, name, _, upstream) =
        not (skipByName name) && isJust upstream

      getName :: BranchInfo -> Text
      getName (_, name, _, _) = name

      branches = map parseLine output

      branchNames = map getName $ filter keepBranches branches
      currentBranch =
        getName
          <$> find (\(isCurrent, _, _, _) -> isCurrent) branches

  pure (branchNames, currentBranch)

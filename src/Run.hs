module Run
  ( run,

    -- * Testing *
    getBranches,
  )
where

import Config
  ( BranchConfig (branchConfigSkip),
    Config
      ( Config,
        configBranches,
        configCheckoutBranch,
        configForceMoveMain,
        configMainName,
        configOriginName,
        configUpstreamName
      ),
    ConfigBranches,
  )
import Control.Foldl qualified as L
import Control.Monad.Extra (whenJust)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import GitOutputParser
  ( BranchCheckoutState (InAnotherWorkspace, InCurrentWorkspace, NotCheckedOut),
    BranchInfo
      ( BranchInfo,
        branchInfoCheckoutState,
        branchInfoName,
        branchInfoUpstream,
        branchInfoWorktree
      ),
    UpstreamInfo (uiName),
    branchInfoParser,
  )
import Import
import RIO.List (find)
import Text.Parsec (runParser)
import Turtle (Line, Shell, inproc, lineToText, procs, reduce, sh)
import Turtle.Format (format, printf, s, w, (%))

run :: Config -> RIO App ()
run = sh . moveForward

moveForward :: Config -> Shell ()
moveForward
  Config
    { configMainName = main,
      configOriginName = origin,
      configUpstreamName = upstream,
      configForceMoveMain = forceMoveMain,
      configCheckoutBranch = checkoutBranch,
      configBranches
    } = do
    let targetUpstream = upstream <> "/" <> main

    relevantBranches <- getBranches (== main) targetUpstream

    -- Process `currentBranch` last, if it is part of the process list.  This
    -- way it will show up in the log at the very top.
    let (branches, skippedBranches, ignoredBranches) =
          prepareBranchWorkOrder configBranches relevantBranches
    let currentBranch = branchInfoName <$> find isCurrent branches

    printf "git-move-forward: Just blindly rebasing...\n"
    printf "\n"

    unless (null skippedBranches) $ do
      printf
        "=== Branches marked as skipped in the configuration:\n"
      forM_ skippedBranches $ \branch -> do
        let name = branchInfoName branch
        printf ("  " % s % "\n") name

    unless (null ignoredBranches) $ do
      printf
        "=== WARNING These branches can not be updated.  \
        \Checked out in another worktree\n\n"
      forM_ ignoredBranches $ \branch -> do
        let name = branchInfoName branch
            worktree = fromMaybe "<unset>" $ branchInfoWorktree branch
        printf ("  " % s % " worktree: " % s % "\n") name worktree

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
      procs "git" ["switch", name] mempty
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
      procs "git" ["switch", "--detach", main] mempty
      procs
        "git"
        ["branch", "--force", main, format (s % "/" % s) upstream main]
        mempty
      procs "git" ["push", "--force", origin, main] mempty

    whenJust (asum [checkoutBranch, currentBranch]) \v ->
      procs "git" ["switch", v] mempty

isCurrent :: BranchInfo -> Bool
isCurrent = (== InCurrentWorkspace) . branchInfoCheckoutState

type Branches = [BranchInfo]

getBranches :: (Text -> Bool) -> Text -> Shell Branches
getBranches skipByName targetUpstream = do
  output <-
    reduce L.list $
      inproc
        "git"
        -- Second "--verbose" adds remote branch info, and the worktree path.
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

  pure targetBranches

-- Splits branches that are checked out in other workspaces into the second
-- list.  They can not be modified in the current workspace.
-- Puts branch currently checkout in this workspace to be the last in the first
-- list.  This way it would be updated last, and will show up first in the `git
-- log` output.
prepareBranchWorkOrder ::
  ConfigBranches ->
  [BranchInfo] ->
  ([BranchInfo], [BranchInfo], [BranchInfo])
prepareBranchWorkOrder configBranches branches =
  let skipBranch :: Text -> Bool
      skipBranch name =
        maybe False branchConfigSkip $ M.lookup name configBranches

      skippedBranches = filter (skipBranch . branchInfoName) branches
      branches' = filter (not . skipBranch . branchInfoName) branches

      notCheckedOut = (== NotCheckedOut) . branchInfoCheckoutState
      inCurrentWorkspace = (== InCurrentWorkspace) . branchInfoCheckoutState
      inOtherWorkspace = (== InAnotherWorkspace) . branchInfoCheckoutState

      thisWorkspace =
        filter notCheckedOut branches'
          ++ filter inCurrentWorkspace branches'
      otherWorkspace = filter inOtherWorkspace branches'
   in (thisWorkspace, skippedBranches, otherWorkspace)

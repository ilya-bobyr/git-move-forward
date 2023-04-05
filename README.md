# git-move-forward

Rebases local branches after a remote branch update.

Git workflow helper.  After fetching updates for a remote branch, this command
can rebase all the local branches that have this remote branch as their
upstream.

Supports a rebase flow, with a central repository storing the main copy, and a
local fork used for publishing changes.  One of the models supported (and
encouraged?) by GitHub.

## How-to

Just run with `--help` to see the arguments:

```
❯ git-move-forward --help
Rebases local branches after a remote branch update

Usage: git-move-forward [--version] [--help] [-v|--verbose]
                        [-m|--main MAIN-BRANCH] [-o|--origin REMOTE]
                        [-u|--upstream REMOTE] [-M|--force-move-main] [BRANCH]

  Git workflow helper. After fetching updates for a remote branch, this command
  can rebase all the local branches that have this remote branch as their
  upstream.

Available options:
  --version                Show version
  --help                   Show this help text
  -v,--verbose             Verbose output?
  -m,--main MAIN-BRANCH    Name of the main development branch. Used to
                           construct the reference point for synchronization.
                           (default: "master")
  -o,--origin REMOTE       Name of the origin remote. This is the repository on
                           GitHub containing your code. Used to construct a
                           target for the forced update of the main branch.
                           (default: "origin")
  -u,--upstream REMOTE     Name of the upstream remote. This is the repository
                           you are getting updates from. The update will only
                           affect branches that use this repository as their
                           "upstream" branch. If --force-move-main is set, your
                           main branch will be moved to point to the main branch
                           of this repository. (default: "upstream")
  -M,--force-move-main     Force move your origin repository main branch to
                           point to the main branch of the upstream repo, after
                           all the branches are rebased.
  BRANCH                   If specified, checkout this branch at the end
```

Under normal circumstances, `git move-forward` (when `git-move-forward` is in
`$PATH`) should be enough.

# Roadmap

Loading everything is the default because it is the fastest way to work when
the whole project fits in memory. When it doesn't (a project too big, or too
many worktrees running at once), the alternative today is `cabal build`, so
ghcid should also support starting empty and loading modules piecemeal. The
roadmap runs in three phases:

1. Fix bugs and improve features, keeping load-everything as the default.
   Most of this work serves both modes, so avoid baking in new
   load-everything assumptions along the way.
2. Refactor so that what is loaded is ghcid's own explicit state, instead of
   whatever the command loaded as reported back by GHCi (which today drives the
   watch set, the `--test` context and what "All good" means).
3. Provide both modes.

Planned work lives here. Known issues with no plan yet live in `bugs`
(managed with the `t` task tool).

## Phase 1 — Fix and improve, load-everything by default

Milestones within the phase are ordered by: live pain, then features that
retire wrapper scripts and external pollers, then parity nice-to-haves.

### Bugs

1. The sync marker GHCi echoes after each command,
   `let fromString s = s in ...`, shadows an imported `fromString`, so a
   `-Wall -Werror` project fails every load with `Werror=name-shadowing` in
   `<interactive>` (upstream #429). Build the marker without a string
   literal, so neither shadowing nor `RebindableSyntax` matters.
2. A hung session (GHCi never returns its prompt) goes undetected: GHC sits
   idle and the output file stays at `state=loading`. Detect it and report it,
   or restart. One candidate cause of the silent stall milestone 1 diagnosed.

### Milestone 1 — Output files a script can rely on

Status: done; merged to master 2026-11-20. Also made the test suite parallel
and free of load-dependent deadlines, and fixed ghcid crashing when GHCi dies
after startup.

Demo: a script (or agent) reading `errors.err` always sees either the real
failure or a build it can date; a typo'd `--restart` path fails loudly.

1. Error at startup on a nonexistent `--restart`/`--reload` path.
2. Write the underlying cabal/stack error into output files when the startup
   command fails, instead of bare "Ghcid has stopped."
3. Add a generation marker to output files, so pollers can distinguish stale
   from current.
4. Diagnose the silent reload stall and make ghcid recover (re-establish
   watches) or at least report that it is no longer watching.

### Milestone 2 — Multi-repl that works on real projects

Demo: out-of-the-box `ghcid` on ghc-debug (in-project build-tool-depends,
excluded components) builds and reports diagnostics for everything it can,
and `ghcid --test` runs a suite in a multi-repl monorepo session.

1. Make `--test`/`--run`/`--setup` expressions usable under multi-repl. Today
   they evaluate in an interactive context with no home module in scope, and
   `:module` is "not supported (yet) in multi-mode", so the documented test
   gate fails with "Not in scope" even with `--setup` imports.
2. Detect the multi-repl failure mode where a component has
   `build-tool-depends` on in-project executables, and fall back to a
   component set that loads. `cabal repl --enable-multi-repl all` fails there
   because cabal must build those executables against in-repl libraries (seen
   in ghc-debug: `system-test` needs `clock`, `debug-test` and
   `srts-test-prog` against the in-repl `ghc-debug-stub`).
3. When components are excluded from the loaded set, say so: print the
   coverage gap with the component names. Today they get no diagnostics at all
   (ghc-debug's `system-test` modules hid 2 warnings).
4. Give the auto command a fair `-j` default for concurrent sessions. Done
   2026-11-20: half the processors, plus `--jobs N`. Not `-jsem`: a
   session-shared semaphore would lose tokens whenever ghcid SIGKILLs a GHCi
   mid-compile, which restarts do routinely.

### Milestone 3 — Status interface for tools and agents

Demo: `ghcid status --wait` in a second terminal blocks until the current
build finishes, then prints diagnostics (optionally `--json`); a Claude Code
skill uses it the way tricorder's plugin uses `tricorder status`.

1. Add a `status` request to the socket server protocol: current generation,
   reloading flag, diagnostics; plus a blocking wait-for-current-build variant.
2. Add a `ghcid status [--wait] [--json]` client subcommand speaking to the
   running server's socket.
3. Guard the builddir against a second session, since two sessions sharing one
   fight over build products and the output file (lock file; the newcomer
   fails loudly naming the holder). Add a `ghcid stop` subcommand that ends
   the project's session and its whole process tree.
4. Ship a minimal agent skill (status + wait) alongside the existing VS Code
   integration.

### Milestone 4 — Lifecycle hooks

Demo: `ghcid --before-restart-shell hpack --restart-glob '**/package.yaml'`
regenerates cabal files with no wrapper script.

1. Shell hooks: `--before-startup-shell`, `--after-reload-shell`,
   `--before/after-restart-shell`, repeatable, run in sequence, failure output
   printed; set `IN_GHCID` in the session environment.
2. Ghci hooks: `--after-reload-ghci`, `--before/after-restart-ghci`
   (`--setup` stays as the after-startup-ghci spelling).
3. `async:` prefix for background shell hooks.

### Milestone 5 — Watching semantics parity

Demo: delete or rename a module and ghcid recovers by itself; glob rules
control what reloads vs restarts.

1. `--reload-glob`/`--restart-glob` with gitignore semantics including `!`
   negation.
2. Restart the session automatically when a loaded module is deleted or moved
   (today's wedge case).
3. Stretch: interrupt an in-progress reload when files change again, with an
   opt-out flag.

## Phase 2 — Make the loaded set explicit

Steps to be written once phase 1 shows where the load-everything assumptions
actually sit.

## Phase 3 — Both modes

Sketch, to be refined after phase 2:

- Start empty (ghciwatch's `--repl-no-load` approach).
- Socket `add`/`remove` requests to change the loaded set.
- A `reload` request that replies once diagnostics and watches are updated;
  it matters most in this mode.
- Output files name what is loaded.

## Deliberately not planned this round

Background daemonization, a TUI, type-checking excluded components in a second
pass, dependency source lookup, a project config file, structured logging,
test timeouts/memory caps, tricorder's extra eval-comment forms, and shell
completions. Revisit after the status interface proves out which of these the
agent workflow actually needs.

## Appendix: feature comparison with ghciwatch and tricorder

### ghcid already has it

- Eval comments: `--allow-eval` runs `-- $>` lines and `{- $> ... <$ -}`
  blocks. Same markers and semantics as ghciwatch's `--enable-eval`.
- Error file for consumers: `-o/--outputfile` (repeatable; ghciwatch's
  `--error-file` is explicitly ghcid-compatible). A `.json` extension switches
  to structured JSON with severity, file, and spans. Each file starts with a
  generation/state/start-time header, and a failed startup command's own
  output is written into it.
- Test-after-load: `--test`/`--run` (ghci expressions, run when error-free;
  `--warnings` relaxes to warnings-allowed), `--test-message`, `--setup` for
  startup ghci commands. Covers ghciwatch's `--test-ghci` +
  `--after-startup-ghci` core use (`:set args ...` works via `--setup`).
- Lint: `--lint` (hlint or any shell command, run on touched files). Neither
  ghciwatch nor tricorder has this.
- Clear on reload: `--clear`.
- Extra watch paths: `--reload`/`--restart` accept files and directories
  (directories pick up new files). Auto-restart on `.cabal`, `cabal.project`,
  `stack.yaml`, `.ghci` — including with a custom `--command`.
- Polling fallback: `--poll[=N]`.
- Resilience: waits for a change instead of exiting when nothing loads, the
  startup command fails, or GHCi dies; kills the whole GHCi process group on
  SIGTERM/SIGHUP/exit; force-terminates unresponsive GHCi; audits its file
  watcher and restarts it if it missed a change.
- Auto command: probes stack/cabal/ghc versions and picks `cabal repl
  --builddir=dist-newstyle/ghcid --enable-multi-repl all` with fast session
  flags (`-fprefer-byte-code`, `-fwrite-if-simplified-core`,
  `-fdefer-diagnostics`, `-fno-break-points`, ...) and half the processors.
  Neither competitor tunes the session this aggressively; ghciwatch defaults
  to bare `cabal v2-repl`.
- Socket server: `--server` pushes diagnostics to connected clients and
  executes ghci queries (`:type-at`, `:loc-at`, `:uses`) against the live
  session with path rewriting — navigation data no other tool offers.
  tricorder's daemon has a richer protocol but cannot query the session.
- Terminal UX: height-limited formatting, `--reverse-errors`,
  `--no-height-limit`, `--max-messages`, newest-errors-first sorting, window
  title/icon status, `--topmost`, color control.
- Per-directory default args via a `.ghcid` file.

### ghcid has a weaker version

- Lifecycle hooks. ghcid has only `--setup` (ghci, startup), `--test`
  (after successful load), and `--lint`. ghciwatch has the full matrix:
  before/after x startup/reload/restart x shell/ghci, repeatable and run in
  sequence, `async:` prefix for background shell hooks, failure output
  capture, and an `IN_GHCIWATCH` env var. tricorder has start/reload
  before/after shell hooks in config. The flagship use case is running hpack
  before (re)start. ghcid cannot express any shell hook.
- Test running. `--test` is a single async ghci expression whose failure is
  detected by sniffing `*** Exception:` on stderr. No shell test commands, no
  per-target test runs, no timeout or memory cap (tricorder: `test_timeout`,
  `test_memory_limit` via `-M`), and a file change during a long test is only
  picked up for the next reload.
- Eval comment forms. ghcid lacks tricorder's line-comment multiline form
  (`-- $$>` ... `-- <$$`) and its block variant (`{- $$>` ... `<$$ -}`).
- Watching semantics. No glob support (ghciwatch's `--reload-glob` /
  `--restart-glob` with gitignore semantics including `!` negation), no
  debounce knob, and deleted/moved modules wedge the session (README FAQ says
  "kill ghcid and start it again"; ghciwatch restarts the session
  automatically).
- Machine-readable status. The output file can be dated and polled, but there
  is no query: tricorder offers `status --json` and a blocking
  `status --wait`.
- Configuration. `.ghcid` holds extra CLI args only; no project config file
  like `.tricorder.yaml` (command, targets, watch dirs, test targets, hooks).
- Logging. `--verbose` only; ghciwatch has leveled/filterable logs plus a JSON
  log file.

### ghcid lacks it entirely

- Interruptible reloads: ghciwatch interrupts an in-progress reload when files
  change again (with `--no-interrupt-reloads` to opt out). ghcid finishes the
  old reload first.
- `--repl-no-load` workflow: ghciwatch starts an empty session and `:add`s
  only edited modules — large-project reloads shrink dramatically.
- Background daemon with multiple clients: tricorder's daemon serves a TUI and
  CLI simultaneously from one build, with an idle shutdown timeout.
  (ghcid's `--server` shares a session but ghcid itself owns the terminal.)
- TUI: tricorder's `tricorder ui` dashboard with test views, eval-comment
  views, daemon info, and rebindable keys.
- Coding-agent integration: tricorder ships Claude Code/Copilot/Cursor plugins
  (skill + MCP) built on `tricorder status` / `status --wait`.
- Dependency source lookup: `tricorder source Some.Module`.
- hpack integration: tricorder auto-runs hpack when `package.yaml` changes;
  ghciwatch handles it via hooks.
- Shell completions: ghciwatch `--completions bash|zsh|fish|...`.

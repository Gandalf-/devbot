# devbot

A lightweight, inspectable task scheduler. Two top-level concepts:

- **Events** — recurring shell actions with intervals, optional requirements, and optional
  file/output monitors. Lifecycle in `Devbot/Event/Runtime.hs`.
- **Services** — long-running processes managed and restarted by devbot. Lifecycle in
  `Devbot/Service/Runtime.hs`.

The runtime loop lives in `Devbot/Bot.hs` (`runner` → state machine that ticks every half second).
Persistent state is a key-value store (`apocrypha`).

## Build / test

- `stack build` — builds the library and the `devbot` executable.
- `stack test` — runs the hspec test suite. Specs are auto-discovered (`test/Spec.hs` is just `{-#
  OPTIONS_GHC -F -pgmF hspec-discover #-}`).
- Resolver: `lts-20.25`, GHC 9.2.8.

When adding a new test spec, you must register it in `devbot.cabal` under `test-suite bot` →
`other-modules`. Likewise, new library modules go in `library` → `exposed-modules`. Forgetting
either is the most common build failure when adding files.

## Module layout

```
Devbot/
├── Bot.hs                  -- top-level state machine, ties events + services together
├── Daemon.hs, Run.hs       -- daemon entry, CLI dispatch
├── List.hs, Status.hs,
│   Table.hs, Schema.hs     -- user-facing display
├── ParseCheck.hs           -- config validation CLI
├── Event/
│   ├── Config.hs           -- Event/Config/Data ADTs, FromJSON, Valid
│   └── Runtime.hs          -- task lifecycle: handle → check → run → success/failure
├── Service/
│   ├── Config.hs
│   └── Runtime.hs
└── Internal/
    ├── Common.hs           -- logger, getTime, ContextF, checkHandles
    ├── Persist.hs          -- apocrypha wrapper
    ├── Directory.hs        -- config + DB path resolution
    ├── System.hs           -- pid + process helpers, config loading
    ├── Monitor.hs          -- file watching, output diffing
    ├── Parser.hs           -- natural-language interval parser ("hourly", "every 5 seconds")
    ├── Healthcheck.hs      -- optional ping notifications for actions
    ├── ColorText.hs, Display.hs, Table.hs
```

The executable's `main` lives in `src/main.hs` and is intentionally tiny — it dispatches into
`Devbot.Run`.

## Dependency-injection pattern for IO effects

This codebase prefers **typedef-based effect injection** over typeclasses or monad transformers.
The pattern:

```haskell
-- Devbot.Internal.Common
type ContextF = IO Context        -- persistence handle factory

-- Devbot.Internal.Healthcheck
type Pinger = String -> IO ()     -- HTTP ping effect
```

Production sites pass the real implementation (`defaultContext`, `httpPing`); tests pass a stub or
recorder. Most lifecycle functions in `Event/Runtime.hs` thread these explicitly:

```haskell
handle  :: Pinger -> ContextF -> Task -> IO Task
success :: Pinger -> ContextF -> Task -> IO Task
```

When adding a new external IO effect (network, filesystem mutation, time source other than
`getTime`), prefer this pattern over a typeclass — it's lighter and matches what's already here.

### Testing IO effects

Tests use real IO where it's cheap and deterministic (`spawnCommand "echo a"`, `getContext
ServerMemory` for in-memory apocrypha). For effects that aren't safe in tests (real HTTP,
filesystem outside `/tmp`), use an `IORef`-backed recorder:

```haskell
mkRecorder :: IO (Pinger, IO [String])
mkRecorder = do
    ref <- newIORef []
    pure (\u -> modifyIORef ref (u:), reverse <$> readIORef ref)
```

`test/HealthcheckSpec.hs` and `test/EventSpec.hs` show the pattern end-to-end. Don't add a test HTTP server when a recorder will do.

## Config style

- YAML/JSON via `aeson` + `yaml`. Each config ADT has a `FromJSON` instance with `parseJSON =
  withObject ...` and `o .:?` for optional fields.
- Validation lives in the `Valid` typeclass (`Devbot.Event.Config`). New optional fields that allow
  strings should reject the empty string in `valid` (mirror the existing `require` / `health`
  cases).
- Adding a field to `Config` requires updating positional pattern matches across the codebase.
  Known sites: `Devbot/Event/Runtime.hs` (`requirementsMet`, `monitorMet`, `Valid Config`),
  `Devbot/List.hs` (`printOptional`), and the `Config` literals in `test/EventSpec.hs` and
  `test/SystemSpec.hs`. Search for `Config (` and `Config [` before relying on the type-checker
  alone — the `Valid` instance uses positional patterns so the compiler will catch most, but not
  all, sites if you only update some.

## Style conventions

- Strict fields (`!`) on all record fields.
- `LambdaCase` and `RecordWildCards` are project-wide default extensions (set in `devbot.cabal`).
- 4-space indent. `where` clauses are common and preferred over top-level helpers when the helper
  isn't reused.
- Module headers use Haddock-style block comments with `Module`, `Description`, `Copyright`,
  `License`, `Maintainer`, `Stability`, `Portability` — match this when creating new top-level
  modules.
- `getTime` (POSIX seconds, `Integer`) is the standard time source — not `getCurrentTime` directly.
- Logging goes through `Devbot.Internal.Common.logger`; don't `putStrLn` from inside the runtime.

## Action lifecycle (events)

For background when touching `Event/Runtime.hs`:

1. `Bot.runner` calls `E.handle pinger cxf` on each task every tick.
2. `handle` dispatches by task state: idle → `check`; running → poll handles via `checkHandles`;
   partially running (serial) → continue or fail.
3. `check` runs `requirementsMet` then `monitorMet`. If a monitor reports "no changes", the run is
   short-circuited to `success` without spawning anything.
4. `runParallel` / `runSerial` spawn the actual shell commands.
5. `success` / `failure` update the persisted `Data` (`_when`, `_lastRun`, `_duration`, `_errors`)
   via `flush`.

Backoff on failure is `(errors + 1) * 60` seconds.

## When you're not sure

- Start by reading `Devbot/Event/Runtime.hs` end-to-end — it's the heart of the system and most
  changes touch it.
- Before changing a config ADT, run `grep -rn "Config (\\|Config \\[" --include="*.hs"` to find
  every positional pattern.
- Before adding a network or external dependency, check `devbot.cabal` — the project's style is to
  keep deps lean.

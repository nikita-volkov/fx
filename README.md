# fx: A Haskell Library for Modular Effectful Computations

[![Hackage](https://img.shields.io/hackage/v/fx.svg)](https://hackage.haskell.org/package/fx)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/fx/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Overview

`fx` is a lightweight Haskell library for writing effectful computations with explicit environment dependencies, error handling, and resource management. It provides a value-based abstraction for effects, allowing modular composition of computations without relying on deep monad transformer stacks. Inspired by principles like Ports and Adapters (Hexagonal Architecture), `fx` helps isolate the infrastructure concerns from pure domain logic.

At its core, `fx` treats effects as computations that are parametric over environments (`env`) and errors (`err`), enabling easy mapping, handling, and composition. It supports concurrent execution, safe resource acquisition/release.

This library is particularly useful for applications with:

- Modular resources (e.g., DB connections, S3 clients).
- Hierarchical environments and errors (e.g., sub-modules composing into a top-level app).
- Pure core logic that depends on abstract capabilities, interpreted effectfully at the edges (using `fx`).

## Features

- **Explicit Environment and Error Types**: Computations depend on a provided `env` and fail with a specific `err`, preventing silent failures and enabling type-safe composition.
- **Modular Composition**: Use `mapEnv` and `mapErr` to adapt sub-computations (e.g., embed a DB-specific effect into a full app context).
- **Resource Management**: The `With` type provides an applicative interface for acquiring and releasing resources, ensuring cleanup even on errors.
- **Concurrency Support**: Execute multiple effects in parallel via `concurrently` composing via `Alternative` interface.
- **Rich Error Handling**: Combinators like `handleErr`, `exposeErr`, and `absorbErr` for flexible recovery and transformation.
- **IO Integration**: Safely lift `IO` actions with exception handling, while warning about abstraction leaks.
- **Ports and Adapters Compatibility**: Define abstract ports (e.g., via MTL-style classes) in your core domain, and implement adapters using `Fx` in infrastructure layers for effectful interpretation.
- **Lightweight and Performant**: No transformer stacks, GADT overhead or exotic type-level constructs; effects compose as values for minimal runtime cost.

## Philosophy and Use Cases

`fx` is designed for applications where you want to separate pure domain logic from effectful infrastructure. Drawing from Ports and Adapters:

- **Core Domain**: Write pure functions or abstract monadic actions (e.g., constrained by MTL-style classes like `class DBPort m`). These act as "ports" declaring required capabilities without implementing them.
- **Infrastructure Adapters**: Use `Fx` to provide concrete implementations, composing sub-effects (e.g., DB and S3) via `mapEnv`/`mapErr`. Resources are managed with `With` for safe acquisition/release.
- **Interpretation**: At the app's entry point, run the core logic by interpreting ports into `Fx` instances, then execute via `runFx`.

This approach ensures:
- Testability: Mock ports with pure instances in tests.
- Modularity: Sub-modules define their own `env`/`err`, composable into larger structures without global state.
- Safety: Explicit errors and resources prevent leaks or ignored failures.

For larger apps (e.g., with DB, S3, reporting), `fx` avoids "transformer hell" by treating effects as composable values, making it a middle ground between direct stacks and full effect systems.

## Quick Start

```haskell
import Fx

-- Define your environment and error types
data Env = Env { dbConn :: Connection, apiKey :: Text }
data Err = DbError SomeException | ApiError Text

-- Resource acquisition with automatic cleanup
env :: With Err Env
env = do
  conn <-
    acquire (connectDb "postgres://localhost/mydb")
      `withRelease` closeDb
  key <-
    acquire (readApiKey "config.txt")
  pure $ Env conn key

-- Business logic with explicit dependencies
fetchUser :: Int -> Fx Env Err User
fetchUser userId = runExceptionalIO $ \Env{dbConn} -> 
  queryUser dbConn userId

-- Compose and run
main :: IO ()
main = runFx $ with env $ do
  user <- fetchUser 42
  liftIO $ print user
```

## Acknowledgments and Inspirations

The design of `fx` draws inspiration from several sources:

- **Ports and Adapters (Hexagonal Architecture)**: Emphasizes separation of core domain logic from infrastructure, with explicit ports and adapters. `fx` focuses on the infrastructure side, providing a way to implement adapters that compose effects modularly. Thus, it promotes the cornerstone principle of programming in Haskell, where pure functions are isolated from side effects.

- **Managed** (from `managed` library)**: The `With` type in `fx` is inspired by the `Managed` monad, providing a way to acquire and release resources safely. However, `fx` extends this concept to support explicit error handling and composition with other effects.

- **ReaderT and ExceptT**: The `Fx` type can be seen as a generalization of the `ReaderT env (ExceptT err IO)` pattern, but with better composability and safety. Same as in this pattern it avoids the pitfalls of deep transformer stacks while still providing the same capabilities.

- **UIO**: The approach to lifting `IO` actions with explicit error handling in `fx` is influenced by the ideas presented in the "unexceptionalio" library, promoting safer interaction with `IO` while avoiding unchecked exceptions.

- **ZIO**: Although initially conceived at the same time as `ZIO`, the library does intersect in core ideas and due to the popularity of ZIO it takes its experience and evolution into account.

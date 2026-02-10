# fx: A Haskell Library for Modular Effectful Computations

[![Hackage](https://img.shields.io/hackage/v/fx.svg)](https://hackage.haskell.org/package/fx)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/fx/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Overview

`fx` is a lightweight Haskell DSL for implementing IO-level operations in a safer and more composable way. It targets concurrent applications that deal with resources and require precision in error handling.

In `fx` application is an IO operation that is parameterized by some environment (`env`) and an error type (`err`). It captures a common pattern, where you have a set of resources (e.g., database connections, API clients) and a well-defined error model under the following type:

```haskell
data Fx env err result
```

Which is a `Monad` among other things.

And it performs, because it is implemented simply as a modification of the Reader pattern.

It supports leakless structured concurrency declarable via `Alternative` and `Applicative` interfaces. Similar to the `Concurrently` abstraction from [`async`](https://hackage.haskell.org/package/async). It does not lose threads or errors. The errors get compile-time checked to be handled.

It provides for leakless resource management via the type:

```haskell
data Scope err env
```

Which is an adaptation of the `Managed` abstraction from the [`managed`](https://hackage.haskell.org/package/managed) library.

Inspired by principles like [Ports and Adapters](https://en.wikipedia.org/wiki/Hexagonal_architecture_(software)) (Hexagonal Architecture), `fx` helps isolate the infrastructure concerns from pure domain logic.

It is particularly useful for applications with:

- Modular resources (e.g., DB connections, S3 clients).
- Hierarchical environments and errors (e.g., sub-modules composing into a top-level app).
- Pure core logic that depends on abstract capabilities, interpreted effectfully at the edges (using `fx`).

## Quick Start

```haskell
import Fx

-- Define your environment and error types
data Env = Env { dbConn :: Connection, apiKey :: Text }
data Err = DbError SomeException | ApiError Text

-- Resource acquisition with automatic cleanup
env :: Scope Err Env
env = do
  conn <-
    releasing closeDb $
      acquire (connectDb "postgres://localhost/mydb")
  key <-
    acquire (readApiKey "config.txt")
  pure $ Env conn key

-- Business logic with explicit dependencies
fetchUser :: Int -> Fx Env Err User
fetchUser userId =
  mapErr DbError $
    runExceptionalIO $ \Env{dbConn} -> 
      queryUser dbConn userId

-- Compose and run
main :: IO ()
main = runFx $ scoping env $ do
  user <- fetchUser 42
  liftIO $ print user
```

## Philosophy and Use Cases

`fx` is designed for applications where you want to separate pure domain logic from effectful infrastructure. Drawing from Ports and Adapters:

- **Core Domain**: Write pure functions or abstract monadic actions (e.g., constrained by MTL-style classes like `class DBPort m`). These act as "ports" declaring required capabilities without implementing them.
- **Infrastructure Adapters**: Use `Fx` to provide concrete implementations, composing sub-effects (e.g., DB and S3) via `mapEnv`/`mapErr`. Resources are managed with `Scope` for safe acquisition/release.
- **Interpretation**: At the app's entry point, run the core logic by interpreting ports into `Fx` instances, then execute via `runFx`.

This approach ensures:
- Testability: Mock ports with pure instances in tests.
- Modularity: Sub-modules define their own `env`/`err`, composable into larger structures without global state.
- Safety: Explicit errors and resources prevent leaks or ignored failures.

For larger apps (e.g., with DB, S3, reporting), `fx` avoids "transformer hell" by treating effects as composable values, making it a middle ground between direct stacks and full effect systems.

## Acknowledgments and Inspirations

The design of `fx` draws inspiration from several sources:

- **[Ports and Adapters](https://en.wikipedia.org/wiki/Hexagonal_architecture_(software)) (Hexagonal Architecture)**: Emphasizes separation of core domain logic from infrastructure, with explicit ports and adapters. `fx` focuses on the infrastructure side, providing a way to implement adapters that compose effects modularly. Thus, it promotes the cornerstone principle of programming in Haskell, where pure functions are isolated from side effects.

- **Managed** (from [`managed`](https://hackage.haskell.org/package/managed) library): The `Scope` type in `fx` is inspired by the `Managed` monad, providing a way to acquire and release resources safely. However, `fx` extends this concept to support explicit error handling and composition with other effects.

- **ReaderT and ExceptT**: The `Fx` type can be seen as a generalization of the `ReaderT env (ExceptT err IO)` pattern, but with better composability and safety. Same as in this pattern it avoids the pitfalls of deep transformer stacks while still providing the same capabilities.

- **UIO**: The approach to lifting `IO` actions with explicit error handling in `fx` is influenced by the ideas presented in the [`unexceptionalio`](https://hackage.haskell.org/package/unexceptionalio) library, promoting safer interaction with `IO` while avoiding unchecked exceptions.

- **ZIO**: Although initially conceived at the same time as the [`ZIO`](http://zio.dev) library for Scala, `fx` does intersect in core ideas and due to the popularity of ZIO it takes its experience and evolution into account.

module Fx
(
  -- * Uio
  Uio,
  exceptionlessIo,
  handledIo,
  handledEio,
  providerAndProcess,
  UioLifting(..),
  -- * Eio
  Eio,
  providerAndAccessor,
  bindErr,
  EioLifting(..),
  -- * Provider
  Provider,
  acquireAndRelease,
  -- * Accessor
  Accessor,
  use,
  mapEnv,
  mapErr,
  mapEnvAndErr,
  -- * Process
  Process,
  handledAccessor,
  -- * Io
  IoLifting,
  io,
)
where

import Fx.Prelude


-- * Uio
-------------------------

{-|
Unexceptional IO,
i.e., IO which only throws exceptions in case of bugs.
-}
newtype Uio res = Uio (IO res)

deriving instance Functor Uio
deriving instance Applicative Uio
deriving instance Monad Uio
deriving instance MonadFix Uio
deriving instance Apply Uio
deriving instance Bind Uio
deriving instance Semigroup res => Semigroup (Uio res)
deriving instance Monoid res => Monoid (Uio res)

instance UioLifting Uio where
  uio = id

instance EioLifting Void Uio where
  eio = handledEio absurd

instance EioLifting err (ExceptT err Uio) where
  eio (Eio (ExceptT io)) = ExceptT (Uio io)

instance MonadIO (ExceptT SomeException Uio) where
  liftIO = ExceptT . Uio . try

mapImp :: (IO res1 -> IO res2) -> Uio res1 -> Uio res2
mapImp fn (Uio imp) = Uio (fn imp)

{-|
Turn IO action into a non-failing action.
It is your responsibility to ensure that it does not throw exceptions.

For this reason an instance of MonadIO is not provided.
-}
exceptionlessIo :: IO res -> Uio res
exceptionlessIo = Uio

{-|
Turn a failing action into a non-failing one,
by providing an exception-handler.
-}
handledIo :: (SomeException -> Uio res) -> IO res -> Uio res
handledIo handler = Uio . handle ((\ (Uio io) -> io) . handler)

{-|
Turn a failing action into a non-failing one,
by providing an error-handler.
-}
handledEio :: (err -> Uio res) -> Eio err res -> Uio res
handledEio handler (Eio (ExceptT io)) = Uio $ do
  a <- io
  case a of
    Right res -> return res
    Left err -> case handler err of
      Uio handlerIo -> handlerIo

{-|
Having an environment provider, execute an action,
which uses the environment and encapsulates result and error handling,
-}
providerAndProcess :: Provider env -> Process env -> Uio ()
providerAndProcess (Provider providerIo) (Process processRdr) =
  Uio (bracket providerIo snd (runReaderT processRdr . fst))


{-|
Support for lifting of unexceptional IO.
-}
class UioLifting m where
  uio :: Uio a -> m a


-- * Eio
-------------------------

{-|
IO with explicit exceptions.
-}
newtype Eio err res = Eio (ExceptT err IO res)

deriving instance Functor (Eio err)
deriving instance Applicative (Eio err)
deriving instance Monoid err => Alternative (Eio err)
deriving instance Monad (Eio err)
deriving instance MonadFix (Eio err)
deriving instance MonadError err (Eio err)
deriving instance Monoid err => MonadPlus (Eio err)
deriving instance Apply (Eio err)
deriving instance Bind (Eio err)
deriving instance Semigroup err => Alt (Eio err)
deriving instance Monoid err => Plus (Eio err)

instance Bifunctor Eio where
  bimap lf rf = mapEio (mapExceptT (fmap (bimap lf rf)))

instance MonadFail (Eio String) where
  fail = Eio . throwE

{-|
Turns an exception-throwing IO into an action with explicit errors.
-}
instance MonadIO (Eio SomeException) where
  liftIO = Eio . ExceptT . try

instance UioLifting (Eio err) where
  uio (Uio io) = Eio (lift io)

instance EioLifting err (Eio err) where
  eio = id

mapEio :: (ExceptT err1 IO res1 -> ExceptT err2 IO res2) -> Eio err1 res1 -> Eio err2 res2
mapEio fn (Eio imp) = Eio (fn imp)

{-|
Retry an action the specified amount of times.
-}
retry :: Word -> Eio err res -> Eio err res
retry times = mapEio $ mapExceptT $ \ io -> let
  loop n = \ case
    Right res -> return (Right res)
    Left err -> if n > 0
      then io >>= loop (pred n)
      else return (Left err)
  in io >>= loop times

{-|
Handle error in another failing action.
-}
bindErr :: (a -> Eio b res) -> Eio a res -> Eio b res
bindErr handler = mapEio $ mapExceptT $ \ io -> do
  a <- io
  case a of
    Right res -> return (Right res)
    Left err -> case handler err of
      Eio (ExceptT handlerIo) -> handlerIo

orElse :: Eio err res -> Eio err res -> Eio err res
orElse (Eio (ExceptT io2)) (Eio (ExceptT io1)) = Eio $ ExceptT $ do
  a <- io1
  case a of
    Right res -> return (Right res)
    Left err -> io2

{-|
Having an environment provider, execute an action,
which uses the environment and produces either an error or result.
-}
providerAndAccessor :: Provider env -> Accessor env err res -> Eio err res
providerAndAccessor (Provider providerIo) (Accessor accessorRdr) =
  Eio (ExceptT (bracket providerIo snd (runExceptT . runReaderT accessorRdr . fst)))


{-|
Support for lifting of IO with explicit exceptions.
-}
class EioLifting err m | m -> err where
  eio :: Eio err a -> m a


-- * Provider
-------------------------

{-|
Environment provider.
Encompasses resource acquisition, releasing and handling of all related errors.

Composes well, allowing you to merge multiple providers into one.

Builds up on some ideas expressed in http://www.haskellforall.com/2013/06/the-resource-applicative.html
-}
newtype Provider env = Provider (IO (env, IO ()))

instance Functor Provider where
  fmap f (Provider io) =
    Provider $ do
      (env, release) <- io
      return (f env, release)

instance Applicative Provider where
  pure env =
    Provider (pure (env, pure ()))
  Provider io1 <*> Provider io2 =
    Provider $ do
      (f, release1) <- io1
      (x, release2) <- onException io2 release1
      return (f x, release2 >> release1)

instance Monad Provider where
  return = pure
  (>>=) (Provider io1) k2 =
    Provider $ do
      (env1, release1) <- io1
      (env2, release2) <- case k2 env1 of Provider io2 -> onException io2 release1
      return (env2, release2 >> release1)

instance UioLifting Provider where
  uio (Uio io) = Provider (fmap (\ a -> (a, return ())) io)

instance EioLifting Void Provider where
  eio (Eio (ExceptT io)) = Provider (fmap (either absurd (\ a -> (a, return ()))) io)

{-|
Create a resource provider from actions that don't fail.
You can turn your exception-throwing actions into these
by means of the `Uio` API.
-}
acquireAndRelease :: Uio env -> (env -> Uio ()) -> Provider env
acquireAndRelease (Uio acquireIo) releaseUio = Provider $ do
  env <- acquireIo
  let
    Uio releaseIo = releaseUio env
    in return (env, releaseIo)


-- * Accessor
-------------------------

{-|
Environment handler, which has a notion of pure errors.
-}
newtype Accessor env err res = Accessor (ReaderT env (ExceptT err IO) res)

deriving instance Functor (Accessor env err)
deriving instance Applicative (Accessor env err)
deriving instance Monoid err => Alternative (Accessor env err)
deriving instance Monad (Accessor env err)
deriving instance Monoid err => MonadPlus (Accessor env err)
deriving instance MonadError err (Accessor env err)

instance Bifunctor (Accessor env) where
  first = mapErr
  second = fmap

instance UioLifting (Accessor env err) where
  uio (Uio io) = Accessor (liftIO io)

instance EioLifting err (Accessor env err) where
  eio (Eio impl) = Accessor (lift impl)

instance MonadIO (Accessor env SomeException) where
  liftIO io = Accessor (lift (ExceptT (try io)))

mapAccessor :: (ReaderT envA (ExceptT errA IO) resA -> ReaderT envB (ExceptT errB IO) resB) -> Accessor envA errA resA -> Accessor envB errB resB
mapAccessor mapper (Accessor impl) = Accessor (mapper impl)

{-|
Map the environment of an accessor.
-}
mapEnv :: (b -> a) -> Accessor a err res -> Accessor b err res
mapEnv fn = mapAccessor (withReaderT fn)

{-|
Map the error of an accessor.
-}
mapErr :: (a -> b) -> Accessor env a res -> Accessor env b res
mapErr fn = mapAccessor (mapReaderT (withExceptT fn))

{-|
Map both the environment and the error of an accessor.
-}
mapEnvAndErr :: (envB -> envA) -> (errA -> errB) -> Accessor envA errA res -> Accessor envB errB res
mapEnvAndErr envProj errProj = mapAccessor (withReaderT envProj . mapReaderT (withExceptT errProj))

{-|
Lift a terminating action into an accessor, which produces no result and
is compatible with any error type.
-}
process :: Process env -> Accessor env err ()
process (Process impl) = Accessor $ mapReaderT lift impl

{-|
Lift an env-using function into accessor.

This is the way you define accessors.
-}
use :: (env -> Eio err res) -> Accessor env err res
use proj = Accessor $ ReaderT $ \ env -> case proj env of
  Eio exceptT -> exceptT


-- * Process
-------------------------

{-|
Accessor on an environment with errors and result handlers fully encapsulated.
IOW, it is forced to handle errors internally.
-}
newtype Process env = Process (ReaderT env IO ())

instance Semigroup (Process env) where
  (<>) (Process a) (Process b) = Process (a *> b)

instance Monoid (Process env) where
  mempty = Process (pure ())
  mappend = (<>)

instance Contravariant Process where
  contramap envProj (Process impl) = Process (withReaderT envProj impl)

{-|
Lift an accessor, which produces no result or error.
-}
handledAccessor :: (err -> Uio ()) -> Accessor env err () -> Process env
handledAccessor handler (Accessor accessorImpl) =
  Process $ flip mapReaderT accessorImpl $ \ (ExceptT io) -> do
    a <- io
    case a of
      Right () -> return ()
      Left err -> case handler err of
        Uio handlerIo -> handlerIo


-- * IO
-------------------------

{-|
Synonym to `MonadIO`, conforming to naming conventions of this library.
-}
type IoLifting = MonadIO

instance UioLifting IO where
  uio (Uio io) = io

instance EioLifting SomeException IO where
  eio (Eio (ExceptT io)) = io >>= either throwIO return

instance EioLifting err (ExceptT err IO) where
  eio (Eio (ExceptT io)) = ExceptT io

{-|
Synonym to `liftIO`, conforming to naming conventions of this library.
-}
io :: IoLifting m => IO a -> m a
io = liftIO

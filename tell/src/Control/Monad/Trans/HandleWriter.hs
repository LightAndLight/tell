{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
 Module      :  Control.Monad.Trans.HandleWriter
 Copyright   :  (C) 2021 Isaac Elliott
 License     :  BSD-3 (see the file LICENSE)
 Maintainer  :  Isaac Elliott <isaace71295@gmail.com>
-}
module Control.Monad.Trans.HandleWriter (
  HandleWriterT (..),
  Env (..),
  runHandleWriterT,
) where

import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State.Class (MonadState)
import Control.Monad.Tell.Class (MonadTell (..))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import System.IO (Handle)

data Env w = Env !Handle !(Handle -> w -> IO ())

{- |

A monad that can write a monoidal summary to a 'Handle'.
-}
newtype HandleWriterT w m a = HandleWriterT {unHandleWriterT :: ReaderT (Env w) m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadTrans
    , MonadState s
    , MonadError e
    , MonadCont
    )

instance MonadReader r m => MonadReader r (HandleWriterT w m) where
  ask = HandleWriterT $ lift ask
  local f (HandleWriterT ma) =
    HandleWriterT $ ask >>= lift . local f . runReaderT ma

{- | The 'MonadTell' law @tell (a <> b) ≡ tell a *> tell b@ is only obeyed when
 the 'Handle' is written to by a single thread.
-}
instance (Monoid w, MonadIO m) => MonadTell w (HandleWriterT w m) where
  tell w =
    HandleWriterT $ do
      Env handle f <- ask
      liftIO $ f handle w

runHandleWriterT :: Handle -> (Handle -> w -> IO ()) -> HandleWriterT w m a -> m a
runHandleWriterT handle f = flip runReaderT (Env handle f) . unHandleWriterT
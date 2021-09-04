{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
 Module      :  Control.Monad.Tell.Class
 Copyright   :  (C) 2021 Isaac Elliott
 License     :  BSD-3 (see the file LICENSE)
 Maintainer  :  Isaac Elliott <isaace71295@gmail.com>
-}
module Control.Monad.Tell.Class (MonadTell (..), WrappedMonadWriter (..)) where

import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.CPS as CPS (RWST)
import qualified Control.Monad.Trans.RWS.CPS as RWS.CPS
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Select (SelectT)
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.CPS as CPS (WriterT)
import qualified Control.Monad.Trans.Writer.CPS as Writer.CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Writer.Class (MonadWriter)
import qualified Control.Monad.Writer.Class as Writer.Class

{- |

== Laws

@tell mempty ≡ pure ()@

@tell (a <> b) ≡ tell a *> tell b@

== How does this related to 'MonadWriter'?

'MonadTell' is a generalisation of 'MonadWriter'. It only provides 'tell';
a function that \'appends\' a monoidal value to some output. Morally, we have
@class 'MonadTell' w m => 'MonadWriter' w m where ...@. See 'WrappedMonadWriter'
for the witness of this.

'MonadWriter'\'s 'Control.Monad.Writer.Class.listen' and 'Control.Monad.Writer.Class.pass'
functions require the monad to hold onto
the output for an arbitrarily long time. This can cause applications to use memory
linear in the number of 'Control.Monad.Writer.tell's, when constant memory usage would suffice.

A motivating example is writing monoidal results to a file. Using 'MonadWriter'
(via a @WriterT@), you would have to accumulate the the entire output and then write it
to the file. In contrast, 'MonadTell' allows you to write each result to the file
as it's obtained, allowing the result to be freed while the rest of the program runs.
-}
class (Monoid w, Monad m) => MonadTell w m | m -> w where
  tell :: w -> m ()
  default tell :: (m ~ t n, MonadTrans t, MonadTell w n) => w -> m ()
  tell = lift . tell

instance (Monoid w, Monad m) => MonadTell w (Strict.WriterT w m) where
  tell = getWrappedMonadWriter . tell

instance (Monoid w, Monad m) => MonadTell w (Lazy.WriterT w m) where
  tell = getWrappedMonadWriter . tell

instance (Monoid w, Monad m) => MonadTell w (CPS.WriterT w m) where
  tell = Writer.CPS.tell

instance (Monoid w, Monad m) => MonadTell w (Strict.RWST r w s m) where
  tell = getWrappedMonadWriter . tell

instance (Monoid w, Monad m) => MonadTell w (Lazy.RWST r w s m) where
  tell = getWrappedMonadWriter . tell

instance (Monoid w, Monad m) => MonadTell w (CPS.RWST r w s m) where
  tell = RWS.CPS.tell

instance (MonadTell w m, Monoid s) => MonadTell w (AccumT s m)
instance MonadTell w m => MonadTell w (ContT r m)
instance MonadTell w m => MonadTell w (IdentityT m)
instance MonadTell w m => MonadTell w (MaybeT m)
instance MonadTell w m => MonadTell w (ReaderT r m)
instance MonadTell w m => MonadTell w (SelectT r m)
instance MonadTell w m => MonadTell w (Lazy.StateT s m)
instance MonadTell w m => MonadTell w (Strict.StateT s m)

-- | A proof that a 'MonadWriter' instance implies a 'MonadTell' instance.
newtype WrappedMonadWriter m a = WrappedMonadWriter {getWrappedMonadWriter :: m a}
  deriving (Functor, Applicative, Monad)

instance MonadTrans WrappedMonadWriter where
  lift = WrappedMonadWriter

instance MonadWriter w m => MonadTell w (WrappedMonadWriter m) where
  tell = WrappedMonadWriter . Writer.Class.tell
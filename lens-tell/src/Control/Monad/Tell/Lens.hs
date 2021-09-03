{- |
 Module      :  Control.Monad.Tell.Lens
 Copyright   :  (C) 2021 Isaac Elliott
 License     :  BSD-3 (see the file LICENSE)
 Maintainer  :  Isaac Elliott <isaace71295@gmail.com>
-}
module Control.Monad.Tell.Lens (scribe) where

import Control.Lens.Setter (ASetter, set)
import Control.Monad.Tell.Class (MonadTell, tell)

{-# INLINE scribe #-}
scribe :: (MonadTell t m, Monoid s) => ASetter s t a b -> b -> m ()
scribe l b = tell (set l b mempty)
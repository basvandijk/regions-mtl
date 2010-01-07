{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Region.Instances.MonadsFD
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module provides instances for the monads-fd classes for 'RegionT's.
--
--------------------------------------------------------------------------------

module Control.Monad.Trans.Region.Instances.MonadsFD where

-- from monads-fd:
-- TODO: import Control.Monad.Cont.Class   ( MonadCont, callCC )
import Control.Monad.Error.Class  ( MonadError, throwError, catchError )
import Control.Monad.RWS.Class    ( MonadRWS )
import Control.Monad.Reader.Class ( MonadReader, ask, local )
import Control.Monad.State.Class  ( MonadState, get, put )
import Control.Monad.Writer.Class ( MonadWriter, tell, listen, pass )

-- from transformers:
import Control.Monad.Trans ( lift )

-- from unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from regions:
import Control.Monad.Trans.Region.Unsafe ( RegionT
                                         , liftCatch
                                         , mapRegionT
                                         -- TODO: , liftCallCC
                                         )

-- TODO:
-- instance Monad pr ⇒ MonadCont (RegionT s pr) where
--     callCC = liftCallCC callCC

instance MonadError e pr ⇒ MonadError e (RegionT s pr) where
    throwError = lift ∘ throwError
    catchError = liftCatch catchError

instance MonadRWS r w st pr ⇒ MonadRWS r w st (RegionT s pr)

instance MonadReader r pr ⇒ MonadReader r (RegionT s pr) where
    ask   = lift ask
    local = mapRegionT ∘ local

instance MonadWriter w pr ⇒ MonadWriter w (RegionT s pr) where
    tell   = lift ∘ tell
    listen = mapRegionT listen
    pass   = mapRegionT pass

instance MonadState st pr ⇒ MonadState st (RegionT s pr) where
    get = lift get
    put = lift ∘ put

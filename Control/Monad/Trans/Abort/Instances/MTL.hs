{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Abort.Instances.MTL where

import Control.Monad.Cont.Class (MonadCont(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.RWS.Class (MonadRWS(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Abort
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Writer.Class (MonadWriter(..))

instance MonadCont m => MonadCont (AbortT a m) where
    callCC = liftCallCC callCC

instance MonadError e m => MonadError e (AbortT a m) where
    throwError = lift . throwError
    catchError = liftCatch catchError

instance MonadReader r m => MonadReader r (AbortT a m) where
    ask   = lift ask
    local f = AbortT . local f . unwrapAbortT

instance MonadWriter w m => MonadWriter w (AbortT a m) where
    tell   = lift . tell
    listen = liftListen listen
    pass   = liftPass pass

instance MonadState s m => MonadState s (AbortT a m) where
    get = lift get
    put = lift . put

instance MonadRWS r w s m => MonadRWS r w s (AbortT a m)

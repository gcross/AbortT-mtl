{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Trans.Abort.Instances.MonadsTF where

import Control.Monad.Cont.Class (MonadCont(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.RWS.Class (MonadRWS(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Abort
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Writer.Class (MonadWriter(..))

instance MonadCont m => MonadCont (AbortT r m) where
    callCC = liftCallCC callCC

instance MonadError m => MonadError (AbortT r m) where
    type ErrorType (AbortT r m) = ErrorType m
    throwError = lift . throwError
    catchError = liftCatch catchError

instance MonadReader m => MonadReader (AbortT r m) where
    type EnvType (AbortT r m) = EnvType m
    ask   = lift ask
    local f = AbortT . local f . unwrapAbortT

instance MonadWriter m => MonadWriter (AbortT r m) where
    type WriterType (AbortT r m) = WriterType m
    tell   = lift . tell
    listen = liftListen listen
    pass   = liftPass pass

instance MonadState m => MonadState (AbortT r m) where
    type StateType (AbortT r m) = StateType m
    get = lift get
    put = lift . put

instance MonadRWS m => MonadRWS (AbortT r m)

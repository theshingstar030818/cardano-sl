module Pos.Util.LogAction
       ( LogAction(..)
       , defaultLogAction
       , hoistLogAction
       ) where

import Universum

import System.Wlog (LoggerName, Severity, dispatchMessage)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO(..))

newtype LogAction m = LogAction (LoggerName -> Severity -> Text -> m ())

hoistLogAction :: (m () -> n ()) -> LogAction m -> LogAction n
hoistLogAction f (LogAction la) = LogAction $ \lgname sev msg -> f (la lgname sev msg)

defaultLogAction :: MonadIO m => LogAction m
defaultLogAction = hoistLogAction liftIO $ LogAction dispatchMessage

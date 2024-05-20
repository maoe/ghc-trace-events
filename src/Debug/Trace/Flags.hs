{-|
Utility functions to inspect RTS flags
-}
module Debug.Trace.Flags
  ( userTracingEnabled
  ) where
import Control.Monad ((<$!>))
import GHC.Exts (inline)
import GHC.RTS.Flags (getTraceFlags, user)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | Check if user tracing is enabled in event logging. Currently GHC RTS
-- doesn't modify the flag after the eventlog framework is initialized so making
-- this a constant value makes sense.
userTracingEnabled :: Bool
userTracingEnabled = unsafeDupablePerformIO $ user <$!> inline getTraceFlags

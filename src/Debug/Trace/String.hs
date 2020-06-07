{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-|
Drop-in replacements for the event tracing functions in "Debug.Trace" but are
faster when user tracing is disabled.
-}
module Debug.Trace.String
  ( traceEvent
  , traceEventIO

  , traceMarker
  , traceMarkerIO
  ) where
import Control.Monad (when)
import Foreign.C.String (CString)
import GHC.Exts (Ptr(..), traceEvent#, traceMarker#)
import GHC.IO (IO(..))
import GHC.IO.Encoding (utf8)
import qualified GHC.Foreign
import qualified GHC.IO.Unsafe as Unsafe

import Debug.Trace.Flags (userTracingEnabled)

-- | Drop-in replacement for 'Debug.Trace.traceEvent' but is more efficient
-- if user tracing in eventlog is disabled.
--
-- Note that this function doesn't evaluate the 'String' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceEvent :: String -> a -> a
traceEvent message a = Unsafe.unsafeDupablePerformIO $ do
  traceEventIO message
  return a
{-# NOINLINE traceEvent #-}

-- | Drop-in replacement for 'Debug.Trace.traceEventIO' but is more efficient
-- if user tracing in eventlog is disabled.
--
-- Note that this function doesn't evaluate the 'String' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceEventIO :: String -> IO ()
traceEventIO message = when userTracingEnabled $
  withCString message $ \(Ptr p) -> IO $ \s ->
    case traceEvent# p s of
      s' -> (# s', () #)

-- | Drop-in replacement for 'Debug.Trace.traceMarker' but is more efficient
-- if user tracing in eventlog is disabled.
--
-- Note that this function doesn't evaluate the 'String' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceMarker :: String -> a -> a
traceMarker message a = Unsafe.unsafeDupablePerformIO $ do
  traceMarkerIO message
  return a
{-# NOINLINE traceMarker #-}

-- | Drop-in replacement for 'Debug.Trace.traceMarkerIO' but is more efficient
-- if user tracing in eventlog is disabled.
--
-- Note that this function doesn't evaluate the 'String' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS
-- generates a broken eventlog.
traceMarkerIO :: String -> IO ()
traceMarkerIO message = when userTracingEnabled $
  withCString message $ \(Ptr p) -> IO $ \s ->
    case traceMarker# p s of
      s' -> (# s', () #)

withCString :: String -> (CString -> IO a) -> IO a
withCString = GHC.Foreign.withCString utf8

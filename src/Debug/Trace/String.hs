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
import Control.Monad
import qualified Debug.Trace as Base

import Debug.Trace.Internal (userTracingEnabled)

-- | Drop-in replacement for 'Debug.Trace.traceEvent' but is more efficient
-- if user tracing in eventlog is disabled.
--
-- Note that this function doesn't evaluate the 'String' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS generates a
-- broken evnetlog.
traceEvent :: String -> a -> a
traceEvent message a
  | userTracingEnabled = Base.traceEvent message a
  | otherwise = a

-- | Drop-in replacement for 'Debug.Trace.traceEventIO' but is more efficient
-- if user tracing in eventlog is disabled.
--
-- Note that this function doesn't evaluate the 'String' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS generates a
-- broken evnetlog.
traceEventIO :: String -> IO ()
traceEventIO message = when userTracingEnabled $ Base.traceEventIO message

-- | Drop-in replacement for 'Debug.Trace.traceMarker' but is more efficient
-- if user tracing in eventlog is disabled.
--
-- Note that this function doesn't evaluate the 'String' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS generates a
-- broken evnetlog.
traceMarker :: String -> a -> a
traceMarker message a
  | userTracingEnabled = Base.traceMarker message a
  | otherwise = a

-- | Drop-in replacement for 'Debug.Trace.traceMarkerIO' but is more efficient
-- if user tracing in eventlog is disabled.
--
-- Note that this function doesn't evaluate the 'String' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS generates a
-- broken evnetlog.
traceMarkerIO :: String -> IO ()
traceMarkerIO message = when userTracingEnabled $ Base.traceMarkerIO message

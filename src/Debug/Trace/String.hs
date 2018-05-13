module Debug.Trace.String
  ( traceEvent
  , traceEventIO

  , traceMarker
  , traceMarkerIO
  ) where
import Control.Monad
import qualified Debug.Trace as Base

import Debug.Trace.Internal (userTracingEnabled)

traceEvent :: String -> a -> a
traceEvent message a
  | userTracingEnabled = Base.traceEvent message a
  | otherwise = a

traceEventIO :: String -> IO ()
traceEventIO message = when userTracingEnabled $ Base.traceEventIO message

traceMarker :: String -> a -> a
traceMarker message a
  | userTracingEnabled = Base.traceMarker message a
  | otherwise = a

traceMarkerIO :: String -> IO ()
traceMarkerIO message = when userTracingEnabled $ Base.traceMarkerIO message

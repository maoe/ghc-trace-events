{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-|
'B.ByteString' variants of the tracing functions in "Debug.Trace".
-}
module Debug.Trace.ByteString
  ( traceEvent
  , traceEventIO

  , traceMarker
  , traceMarkerIO

  , unsafeTraceEvent
  , unsafeTraceEventIO

  , unsafeTraceMarker
  , unsafeTraceMarkerIO
  ) where
import GHC.Base
import GHC.IO
import GHC.Ptr
import qualified GHC.RTS.Flags as Flags
import qualified System.IO.Unsafe as Unsafe

import GHC.Prim
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

import Debug.Trace.Internal (userTracingEnabled)

-- | 'B.ByteString' variant of 'Debug.Trace.traceEvent'.
--
-- \(O(n)\) This function copies the 'B.ByteString' to convert it to a
-- null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
traceEvent :: B.ByteString -> a -> a
traceEvent message a
  | userTracingEnabled = Unsafe.unsafeDupablePerformIO $ do
    traceEventIO message
    return a
  | otherwise = a
{-# NOINLINE traceEvent #-}

-- | 'B.ByteString' variant of 'Debug.Trace.traceEventIO'.
--
-- \(O(n)\) This function copies the 'B.ByteString' to convert it to a
-- null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
traceEventIO :: B.ByteString -> IO ()
traceEventIO message = when userTracingEnabled $
  B.useAsCString message $ \(Ptr p) -> IO $ \s ->
    case traceEvent# p s of
      s' -> (# s', () #)

-- | 'B.ByteString' variant of 'Debug.Trace.traceMarker'.
--
-- \(O(n)\) This function copies the 'B.ByteString' to convert it to a
-- null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
traceMarker :: B.ByteString -> a -> a
traceMarker message a
  | userTracingEnabled = unsafeDupablePerformIO $ do
    traceMarkerIO message
    return a
  | otherwise = a
{-# NOINLINE traceMarker #-}

-- | 'B.ByteString' variant of 'Debug.Trace.traceMarkerIO'.
--
-- \(O(n)\) This function copies the 'B.ByteString' to convert it to a
-- null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
traceMarkerIO :: B.ByteString -> IO ()
traceMarkerIO message = when userTracingEnabled $
  B.useAsCString message $ \(Ptr p) -> IO $ \s ->
    case traceMarker# p s of
      s' -> (# s', () #)

-- | 'B.ByteString' variant of 'Debug.Trace.traceEvent'.
--
-- \(O(1)\) This function is unsafe in the way that it doesn't ensure the input
-- string to be null-terminated. It is user's responsibility to null-terminate
-- the input.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
unsafeTraceEvent :: B.ByteString -> a -> a
unsafeTraceEvent message a
  | userTracingEnabled = Unsafe.unsafeDupablePerformIO $ do
    unsafeTraceEventIO message
    return a
  | otherwise = a
{-# NOINLINE unsafeTraceEvent #-}

-- | 'B.ByteString' variant of 'Debug.Trace.traceEventIO'.
--
-- \(O(1)\) This function is unsafe in the way that it doesn't ensure the input
-- string to be null-terminated. It is user's responsibility to null-terminate
-- the input.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
unsafeTraceEventIO :: B.ByteString -> IO ()
unsafeTraceEventIO message = when userTracingEnabled $
  BU.unsafeUseAsCString message $ \(Ptr p) -> IO $ \s ->
    case traceEvent# p s of
      s' -> (# s', () #)

-- | 'B.ByteString' variant of 'Debug.Trace.traceMarker'.
--
-- \(O(1)\) This function is unsafe in the way that it doesn't ensure the input
-- string to be null-terminated. It is user's responsibility to null-terminate
-- the input.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
unsafeTraceMarker :: B.ByteString -> a -> a
unsafeTraceMarker message a
  | userTracingEnabled = Unsafe.unsafeDupablePerformIO $ do
    unsafeTraceEventIO message
    return a
  | otherwise = a
{-# NOINLINE unsafeTraceMarker #-}

-- | 'B.ByteString' variant of 'Debug.Trace.traceMarkerIO'.
--
-- \(O(1)\) This function is unsafe in the way that it doesn't ensure the input
-- string to be null-terminated. It is user's responsibility to null-terminate
-- the input.
--
-- Note that this function doesn't evaluate the 'B.ByteString' if user tracing
-- in eventlog is disabled.
unsafeTraceMarkerIO :: B.ByteString -> IO ()
unsafeTraceMarkerIO message = when userTracingEnabled $
  BU.unsafeUseAsCString message $ \(Ptr p) -> IO $ \s ->
    case traceMarker# p s of
      s' -> (# s', () #)

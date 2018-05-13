{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
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

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

import Debug.Trace.Internal (userTracingEnabled)

traceEvent :: B.ByteString -> a -> a
traceEvent message a
  | userTracingEnabled = Unsafe.unsafeDupablePerformIO $ do
    traceEventIO message
    return a
  | otherwise = a
{-# NOINLINE traceEvent #-}

traceEventIO :: B.ByteString -> IO ()
traceEventIO message = when userTracingEnabled $
  B.useAsCString message $ \(Ptr p) -> IO $ \s ->
    case traceEvent# p s of
      s' -> (# s', () #)

traceMarker :: B.ByteString -> a -> a
traceMarker message a
  | userTracingEnabled = unsafeDupablePerformIO $ do
    traceMarkerIO message
    return a
  | otherwise = a
{-# NOINLINE traceMarker #-}

traceMarkerIO :: B.ByteString -> IO ()
traceMarkerIO message = when userTracingEnabled $
  B.useAsCString message $ \(Ptr p) -> IO $ \s ->
    case traceMarker# p s of
      s' -> (# s', () #)

unsafeTraceEvent :: B.ByteString -> a -> a
unsafeTraceEvent message a
  | userTracingEnabled = Unsafe.unsafeDupablePerformIO $ do
    unsafeTraceEventIO message
    return a
  | otherwise = a
{-# NOINLINE unsafeTraceEvent #-}

unsafeTraceEventIO :: B.ByteString -> IO ()
unsafeTraceEventIO message = when userTracingEnabled $
  BU.unsafeUseAsCString message $ \(Ptr p) -> IO $ \s ->
    case traceEvent# p s of
      s' -> (# s', () #)

unsafeTraceMarker :: B.ByteString -> a -> a
unsafeTraceMarker message a
  | userTracingEnabled = Unsafe.unsafeDupablePerformIO $ do
    unsafeTraceEventIO message
    return a
  | otherwise = a
{-# NOINLINE unsafeTraceMarker #-}

unsafeTraceMarkerIO :: B.ByteString -> IO ()
unsafeTraceMarkerIO message = when userTracingEnabled $
  BU.unsafeUseAsCString message $ \(Ptr p) -> IO $ \s ->
    case traceMarker# p s of
      s' -> (# s', () #)

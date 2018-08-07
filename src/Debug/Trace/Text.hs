{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-|
'T.Text' variants of the tracing functions in "Debug.Trace".
-}
module Debug.Trace.Text
  ( traceEvent
  , traceEventIO

  , traceMarker
  , traceMarkerIO
  ) where
import Foreign.C.String (CString)
import GHC.Base
import GHC.IO
import GHC.Ptr
import qualified GHC.RTS.Flags as Flags
import qualified System.IO.Unsafe as Unsafe

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Debug.Trace.Internal (userTracingEnabled)

-- | 'T.Text' variant of 'Debug.Trace.traceEvent'.
--
-- \(O(n)\) This function marshals the 'T.Text' into a 'B.ByteString' and
-- convert it into a null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'T.Text' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS generates a
-- broken evnetlog.
traceEvent :: T.Text -> a -> a
traceEvent message a
  | userTracingEnabled = Unsafe.unsafeDupablePerformIO $ do
    traceEventIO message
    return a
  | otherwise = a
{-# NOINLINE traceEvent #-}

-- | 'T.Text' variant of 'Debug.Trace.traceEventIO'.
--
-- \(O(n)\) This function marshals the 'T.Text' into a 'B.ByteString' and
-- convert it into a null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'T.Text' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS generates a
-- broken evnetlog.
traceEventIO :: T.Text -> IO ()
traceEventIO message = when userTracingEnabled $
  withCString message $ \(Ptr p) -> IO $ \s ->
    case traceEvent# p s of
      s' -> (# s', () #)

-- | 'T.Text' variant of 'Debug.Trace.traceMarker'.
--
-- \(O(n)\) This function marshals the 'T.Text' into a 'B.ByteString' and
-- convert it into a null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'T.Text' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS generates a
-- broken evnetlog.
traceMarker :: T.Text -> a -> a
traceMarker message a
  | userTracingEnabled = unsafeDupablePerformIO $ do
    traceMarkerIO message
    return a
  | otherwise = a
{-# NOINLINE traceMarker #-}

-- | 'T.Text' variant of 'Debug.Trace.traceMarkerIO'.
--
-- \(O(n)\) This function marshals the 'T.Text' into a 'B.ByteString' and
-- convert it into a null-terminated 'Foreign.C.Types.CString'.
--
-- Note that this function doesn't evaluate the 'T.Text' if user tracing
-- in eventlog is disabled.
--
-- The input should be shorter than \(2^{16}\) bytes. Otherwise the RTS generates a
-- broken evnetlog.
traceMarkerIO :: T.Text -> IO ()
traceMarkerIO message = when userTracingEnabled $
  withCString message $ \(Ptr p) -> IO $ \s ->
    case traceMarker# p s of
      s' -> (# s', () #)

withCString :: T.Text -> (CString -> IO a) -> IO a
withCString text = B.useAsCString (TE.encodeUtf8 text)

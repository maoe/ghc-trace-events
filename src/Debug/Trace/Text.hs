{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
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

traceEvent :: T.Text -> a -> a
traceEvent message a
  | userTracingEnabled = Unsafe.unsafeDupablePerformIO $ do
    traceEventIO message
    return a
  | otherwise = a
{-# NOINLINE traceEvent #-}

traceEventIO :: T.Text -> IO ()
traceEventIO message = when userTracingEnabled $
  withCString message $ \(Ptr p) -> IO $ \s ->
    case traceEvent# p s of
      s' -> (# s', () #)

traceMarker :: T.Text -> a -> a
traceMarker message a
  | userTracingEnabled = unsafeDupablePerformIO $ do
    traceMarkerIO message
    return a
  | otherwise = a
{-# NOINLINE traceMarker #-}

traceMarkerIO :: T.Text -> IO ()
traceMarkerIO message = when userTracingEnabled $
  withCString message $ \(Ptr p) -> IO $ \s ->
    case traceMarker# p s of
      s' -> (# s', () #)

withCString :: T.Text -> (CString -> IO a) -> IO a
withCString text = B.useAsCString (TE.encodeUtf8 text)

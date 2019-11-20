{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Debug.Trace.Binary
  ( traceBinaryEvent
  , traceBinaryEventIO
  ) where
import Control.Monad (when)
import GHC.Exts (Ptr(..), Int(..), traceBinaryEvent#)
import GHC.IO (IO(..))
import qualified System.IO.Unsafe as Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

import Debug.Trace.Internal (userTracingEnabled)

traceBinaryEvent :: B.ByteString -> a -> a
traceBinaryEvent bytes a
  | userTracingEnabled = Unsafe.unsafeDupablePerformIO $ do
    traceBinaryEventIO bytes
    return a
  | otherwise = a
{-# NOINLINE traceBinaryEvent #-}

traceBinaryEventIO :: B.ByteString -> IO ()
traceBinaryEventIO bytes = when userTracingEnabled $
  BU.unsafeUseAsCStringLen bytes $ \(Ptr p, I# n) -> IO $ \s ->
    case traceBinaryEvent# p n s of
      s' -> (# s', () #)

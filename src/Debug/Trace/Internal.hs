{-# LANGUAGE CPP #-}
module Debug.Trace.Internal
  ( userTracingEnabled
  ) where
import Foreign.C.Types
import Foreign.Marshal.Utils
#if !MIN_VERSION_base(4, 10, 0)
import Data.Word
#endif

userTracingEnabled :: Bool
userTracingEnabled = toBool c_userTracingEnabled
{-# NOINLINE userTracingEnabled #-}

#if MIN_VERSION_base(4, 10, 0)
type CBOOL = CBool
#else
type CBOOL = Word8
#endif

foreign import ccall "userTracingEnabled" c_userTracingEnabled :: CBOOL

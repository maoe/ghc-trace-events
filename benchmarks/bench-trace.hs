{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main

import qualified Debug.Trace as Base
import qualified Debug.Trace.ByteString as B
import qualified Debug.Trace.Binary as Bin
import qualified Debug.Trace.String as S
import qualified Debug.Trace.Text as T

import qualified Debug.Trace.Internal as I

main :: IO ()
main = do
  putStrLn $ "userTracingEnabled: " ++ show I.userTracingEnabled
  defaultMain
    [ bgroup "traceEvent"
      [ bench "Base" $ whnf (Base.traceEvent "Hello") ()
      , bench "String" $ whnf (S.traceEvent "Hello") ()
      , bench "ByteString" $ whnf (B.traceEvent "Hello") ()
      , bench "ByteString/Unsafe" $ whnf (B.unsafeTraceEvent "Hello") ()
      , bench "Text" $ whnf (T.traceEvent "Hello") ()
      ]
    , bgroup "traceEventIO"
      [ bench "Base" $ nfIO $ Base.traceEventIO "Hello"
      , bench "String" $ nfIO $ S.traceEventIO "Hello"
      , bench "ByteString" $ nfIO $ B.traceEventIO "Hello"
      , bench "ByteString/Unsafe" $ nfIO $ B.unsafeTraceEventIO "Hello"
      , bench "Text" $ nfIO $ T.traceEventIO "Hello"
      ]
    , bgroup "traceMarker"
      [ bench "Base" $ whnf (Base.traceMarker "Hello") ()
      , bench "String" $ whnf (S.traceMarker "Hello") ()
      , bench "ByteString" $ whnf (B.traceMarker "Hello") ()
      , bench "ByteString/Unsafe" $ whnf (B.unsafeTraceMarker "Hello") ()
      , bench "Text" $ whnf (T.traceMarker "Hello") ()
      ]
    , bgroup "traceMarkerIO"
      [ bench "Base" $ nfIO $ Base.traceMarkerIO "Hello"
      , bench "String" $ nfIO $ S.traceMarkerIO "Hello"
      , bench "ByteString" $ nfIO $ B.traceMarkerIO "Hello"
      , bench "ByteString/Unsafe" $ nfIO $ B.unsafeTraceMarkerIO "Hello"
      , bench "Text" $ nfIO $ T.traceMarkerIO "Hello"
      ]
    , bench "traceBinaryEvent" $ whnf (Bin.traceBinaryEvent "Hello") ()
    , bench "traceBinaryEventIO" $ nfIO $ Bin.traceBinaryEventIO "Hello"
    ]

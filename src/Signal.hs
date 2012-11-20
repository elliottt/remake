module Signal where


import Control.Concurrent (MVar,newEmptyMVar,tryPutMVar,takeMVar)


type Signal = MVar ()

newSignal :: IO Signal
newSignal  = newEmptyMVar

signal :: Signal -> IO ()
signal sig = do
  _ <- tryPutMVar sig ()
  return ()

block :: Signal -> IO ()
block sig = do
  _ <- takeMVar sig
  return ()

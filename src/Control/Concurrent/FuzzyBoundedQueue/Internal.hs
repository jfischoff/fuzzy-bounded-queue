{-# LANGUAGE RecordWildCards #-}
module Control.Concurrent.FuzzyBoundedQueue.Internal where
import Data.Atomics.Counter
import Data.Atomics
import Data.IORef
import Data.Sequence (Seq, (|>), viewl, ViewL (..))
import qualified Data.Sequence as Seq
import Control.Concurrent.MVar

data FuzzyBoundedQueue a = FuzzyBoundedQueue
  { values      :: IORef (Seq a)
  , counter     :: AtomicCounter
  , maxCount    :: Int
  , waitForNext :: MVar ()
  }

newFuzzyBoundedQueue :: Int -> IO (FuzzyBoundedQueue a)
newFuzzyBoundedQueue maxCount = do
  values      <- newIORef mempty
  counter     <- newCounter 0
  waitForNext <- newEmptyMVar
  return FuzzyBoundedQueue {..}

dropFirst x = case viewl x of
  EmptyL -> mempty
  _ :< a -> a

write :: FuzzyBoundedQueue a -> a -> IO Bool
write FuzzyBoundedQueue {..} x = do
  tryPutMVar waitForNext ()
  count <- readCounter counter
  atomicModifyIORefCAS values $ \queue ->
    ((if count == maxCount then dropFirst else id) $ queue |> x, ())

  let result = count < maxCount
  -- when result $ incrCounter counter
  return result

drain :: FuzzyBoundedQueue a -> IO (Seq a)
drain FuzzyBoundedQueue {..} = do
  takeMVar waitForNext
  writeCounter counter 0
  readIORef values

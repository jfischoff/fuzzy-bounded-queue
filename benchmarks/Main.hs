import Criterion.Main
import Control.Concurrent
import Data.IORef
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.FuzzyBoundedQueue.Internal

main :: IO ()
main = do
  queue <- newFuzzyBoundedQueue 100000
  queue1 <- newFuzzyBoundedQueue 100000
  maxCap <- getNumCapabilities
  reader <- async $ forever $ void $ drain queue
  print maxCap
  defaultMain $ flip map [0 .. maxCap - 1] $ \n ->
    bgroup (show (n + 1) ++ " threads")
      [ bench "write performance" $ whnfIO $ do
          xs <- forM [0 .. n] $ \i -> asyncOn i $
            replicateM_ 10000 $ write queue 1
          mapM_ wait xs
      ]

  cancel reader
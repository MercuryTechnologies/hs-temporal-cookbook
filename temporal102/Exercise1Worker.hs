module Main where

import Exercise1 (runWorker)

-- | Worker executable that demonstrates durable execution.
-- This worker can be killed and restarted without losing workflow state.

main :: IO ()
main = runWorker
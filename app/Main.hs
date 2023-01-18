module Main where

import RIO

import Data
import Events ( play, castTick, castKey )
import Init ( initGame )
import Util ( setNoBuffering )
import Control.Concurrent ( forkIO )

main :: IO ()
main = do
  setNoBuffering
  -- create a channel queue for processing events
  chan <- newChan
  g    <- initGame
  -- create thread to write TickEvents to channel queue
  _ <- forkIO $ castTick chan (getTickRate g)
  -- create thread to listen for key input and write KeyEvents to channel queue
  _ <- forkIO $ castKey chan
  runRIO g $ play chan
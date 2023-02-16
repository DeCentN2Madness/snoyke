module Init (initGame) where


import Data.Maybe ( fromJust )
import RIO
import System.Console.ANSI ( getTerminalSize )
import System.Random ( randomIO )

import Data

initGame :: IO Game
initGame = do
  s@(sy, sx)  <- fromJust <$> getTerminalSize
  -- generate pair of random integers for positioning food item
  fy <- randomIO
  fx <- randomIO
  let
    -- determine initial food coordinates within screen size boundaries
    fp = (fy `mod` sy, fx `mod` sx)
  fp' <- newTVarIO $ Food fp
  dir <- newTVarIO R
  sn  <- newTVarIO $ Snake $ (10, 12) :| [(10, 11), (10, 10)]
  sc  <- newTVarIO 0
  let
      game = Game {
        snake      = sn
      , food       = fp'
      , direction  = dir
      , score      = sc
      , screenSize = s
      , tickRate   = 200
      , blinkRate  = 500
      }
  pure game

module Data where

import RIO

-- events to add to the channel queue:
data Event
  = TickEvent -- used to check if game is over, eat food and move the snake
  | KeyEvent Char -- used to handle keyboard input and change snake direction
  deriving Show

type Size       = (Int, Int)
type Time       = Int
type Point      = (Int, Int)
newtype Snake   = Snake { getSnakeCoords :: NonEmpty Point } deriving Show
newtype Food    = Food { getFoodCoords :: Point } deriving Show
type Score      = Int
data Direction  = U | L | R | D deriving (Show, Eq)
data Game       = Game {
    screenSize      :: Size
  , tickRate        :: Time
  , blinkRate       :: Time
  , snake           :: TVar Snake
  , food            :: TVar Food
  , direction       :: TVar Direction
  , score           :: TVar Int
  }


class HasScreenSize env where
  getScreenSize :: env -> Size

class HasTickRate env where
  getTickRate :: env -> Time

class HasBlinkRate env where
  getBlinkRate :: env -> Time

class HasSnake env where
  getSnake :: env -> TVar Snake

class HasFood env where
  getFood :: env -> TVar Food

class HasDirection env where
  getDirection :: env -> TVar Direction

class HasScore env where
  getScore :: env -> TVar Score

instance HasScreenSize Game where
  getScreenSize = screenSize

instance HasTickRate Game where
  getTickRate = tickRate

instance HasBlinkRate Game where
  getBlinkRate = blinkRate

instance HasSnake Game where
  getSnake = snake

instance HasFood Game where
  getFood = food

instance HasDirection Game where
  getDirection = direction

instance HasScore Game where
  getScore = score
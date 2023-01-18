module Events where

import Control.Monad.Reader
import RIO
import Prelude (getChar)
import System.Random ( randomIO )

import Data
import Render ( renderNext, renderOver )
import Util ( after )
import Data.Maybe (fromJust)
import RIO.NonEmpty ((<|), init)

castTick :: Chan Event -> Time -> IO ()
castTick chan rate = forever $ do
  -- perpetually writes TickEvents to the channel queue at specified interval
  after rate $ writeChan chan TickEvent

castKey :: Chan Event -> IO ()
castKey chan = forever $ do
  -- hide console echo and wait for key input
  hSetEcho stdin False
  c <- getChar
  -- unhide console output
  hSetEcho stdin True
  -- add key event to the channel queue
  writeChan chan (KeyEvent c)

-- IO (State (Reader x))
play :: ( HasDirection env
        , HasScreenSize env
        , HasSnake env
        , HasFood env
        , HasScore env
        , HasBlinkRate env
        ) => Chan Event -> RIO env ()
play chan = forever $ do
  -- pop event off channel queue and process
  event      <- liftIO $ readChan chan
  proceeding <- proceedGame event
  -- render console based on game status (proceeding or over)
  if proceeding then renderNext else renderOver

proceedGame :: (HasDirection env, HasScreenSize env, HasSnake env, HasFood env, HasScore env)
            => Event -> RIO env Bool
-- processes events from the channel and returns whether the game is proceeding
proceedGame (KeyEvent k) = do
  case lookup k keys of
    -- if a valid direction key, turn the snake
    Just d  -> turnSnake d >> pure True
    Nothing -> pure True
  where keys = [('k', U), ('h', L), ('l', R), ('j', D)]
proceedGame TickEvent = do
  p <- canPlay
  if p then do
    eat <- canEat
    when eat eatFood
    moveSnake
    pure True
  else pure False

turnSnake :: HasDirection env => Direction -> RIO env ()
turnSnake d' = do
  dvar <- asks getDirection
  atomically $ do
    d <- readTVar dvar
  -- if requested dir. isn't opposite of current dir., update the dir.
    unless (isOpposite d' d) $ writeTVar dvar d'
    where
    opps = [(U, D), (L, R), (R, L), (D, U)]
    isOpposite d1 d2 = fromJust (lookup d1 opps) == d2

canPlay :: (HasScreenSize env, HasSnake env) => RIO env Bool
canPlay = do
  g  <- ask
  sn <- readTVarIO (getSnake g)
  let
  -- get max y and x coordinates
    (my, mx)           = getScreenSize g
  -- determine if snake's head is outside screen boundaries or intersecting tail
    (hp@(y, x) :| tps) = getSnakeCoords sn
  pure . not $ (x < 0) || (x >= mx) || (y < 0) || (y >= my) || hp `elem` tps

canEat :: (HasSnake env, HasFood env) => RIO env Bool
canEat = do
  g <- ask
  -- determine if snake head is at food position
  Snake (sh :| _) <- readTVarIO $ getSnake g
  Food   fp       <- readTVarIO $ getFood g
  pure $ sh == fp

eatFood :: (HasSnake env, HasFood env, HasScore env, HasScreenSize env) => RIO env ()
eatFood = do
  g      <- ask
  fcol   <- liftIO randomIO
  frow   <- liftIO randomIO
  let
    fvar  = getFood g
    snvar = getSnake g
    scvar = getScore g
    (scol, srow) = getScreenSize g
    newFood = (fcol `mod` scol, frow `mod` srow)
  atomically $ do
    Food  oldFood  <- readTVar fvar
    Snake oldSnake <- readTVar snvar
    writeTVar  snvar $ Snake (oldFood <| oldSnake) -- extend length of snake
    writeTVar  fvar  $ Food   newFood
    modifyTVar scvar (+ 1)

moveSnake :: (HasSnake env, HasDirection env) => RIO env ()
moveSnake = do
  g <- ask
  let
    snvar = getSnake g
    dvar  = getDirection g
  atomically $ do
    Snake s@(h :| _ ) <- readTVar snvar
    d                 <- readTVar dvar
    writeTVar snvar $ Snake (movePoint d h :| init s)
  where
    movePoint d (y, x) = case d of
        U -> (y - 1, x)
        D -> (y + 1, x)
        L -> (y, x - 1)
        R -> (y, x + 1)
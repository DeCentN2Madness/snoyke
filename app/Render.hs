module Render (
  renderOver,
  renderNext
) where

import Prelude (putChar, putStr, putStrLn)
import RIO
import System.Console.ANSI  ( clearScreen, setCursorPosition )

import Data
import Util ( after )

renderNext :: (HasSnake env, HasFood env, HasScore env, HasScreenSize env) => RIO env ()
renderNext = do
  g  <- ask
  sn <- readTVarIO (getSnake g)
  f  <- readTVarIO $ getFood g
  sc <- readTVarIO $ getScore g
  let (y, x) = getScreenSize g
  liftIO clearScreen
  liftIO $ renderSnake 'o' 'x' sn
  liftIO $ renderCharacter '&' (getFoodCoords f)
  liftIO $ setCursorPosition (y + 2) (x - 7)
  liftIO . putStrLn $ "Score " ++ show sc

renderCharacter :: Char -> Point -> IO ()
renderCharacter c (y, x) = do
  setCursorPosition y x
  putChar c
  pure ()

renderSnake :: Char -> Char -> Snake -> IO ()
renderSnake c d (Snake (hp :| tps)) = do
  renderCharacter c hp
  mapM_ (renderCharacter d) tps

renderOver :: (HasScreenSize env, HasBlinkRate env) => RIO env ()
renderOver = do
  c <- ask
  let (row, col) = getScreenSize c
  forever $ do
    after (getBlinkRate c) $ liftIO clearScreen
    after (getBlinkRate c) $ do
      liftIO $ setCursorPosition (div row 2) (div col 2 - 7)
      liftIO $ putStr "Game Over"
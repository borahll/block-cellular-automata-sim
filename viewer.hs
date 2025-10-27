module Main (main) where

import BlockAutomaton.Server
import Control.Exception (bracket)
import Control.Monad (forever)
import Galapagos.Parser
import Galapagos.Rules
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Random.Stateful

enterAlt, exitAlt, hideCursor, showCursor :: String
enterAlt = "\ESC[?1049h"
exitAlt = "\ESC[?1049l"
hideCursor = "\ESC[?25l"
showCursor = "\ESC[?25h"

-- ANSI escapes for 24-bit background/foreground
bgCode :: Colour -> String
bgCode (RGB r g b) =
  "\ESC[48;2;" <> show r <> ";" <> show g <> ";" <> show b <> "m"

fgCode :: Colour -> String
fgCode (RGB r g b) =
  "\ESC[38;2;" <> show r <> ";" <> show g <> ";" <> show b <> "m"

-- Reset attributes
reset :: String
reset = "\ESC[0m"

-- Move cursor back to upper-left corner of a grid of size (h * w)
-- h = number of color rows, w = number of color columns
moveGridHome :: Int -> Int -> String
moveGridHome h w =
  "\ESC["
    <> show h
    <> "A"
    <> "\ESC["
    <> show w
    <> "D"

-- Render a single "pixel cell" using ▀
renderPixelCell :: Colour -> Colour -> String
renderPixelCell top bottom =
  fgCode top <> bgCode bottom <> "▀" <> reset

-- Render a row of cells from two color rows
renderRow :: [Colour] -> [Colour] -> String
renderRow tops bottoms = concat (zipWith renderPixelCell tops bottoms) <> "\n"

-- Render the whole grid (height must be even)
renderGrid :: [[Colour]] -> String
renderGrid [] = ""
renderGrid (a : b : rest) = renderRow a b <> renderGrid rest
renderGrid [_] = error "Grid height must be an even number."

prepareTerminal :: IO ()
prepareTerminal = do
  putStr (enterAlt <> hideCursor)
  hFlush stdout

restoreTerminal :: IO ()
restoreTerminal = do
  putStr (reset <> showCursor <> exitAlt)
  hFlush stdout

labels :: Config -> [String]
labels cfg =
  let (_, l, ls) = foldl add (0, "", []) $ cfgStrategies cfg
   in ls <> [l]
  where
    add (n, cur, ls) (name, colour, _)
      | length name + n < cfgWidth cfg =
          (n + length name + 1, cur <> fgCode colour <> name <> " ", ls)
      | otherwise =
          (length name, fgCode colour <> name, ls <> [cur])

run :: Config -> IO ()
run cfg = do
  let h = cfgHeight cfg
      w = cfgWidth cfg
      header = labels cfg

  seed <- applyAtomicGen uniform globalStdGen

  s <- startAutomaton (galapagos seed cfg) h w

  let display = do
        mapM_ putStrLn header
        putStr =<< renderGrid <$> observeAutomaton s
        hFlush stdout

  display

  forever $ do
    stepAutomaton s
    putStr $ moveGridHome ((h `div` 2) + length header) w
    display

main :: IO ()
main = do
  args <- getArgs

  case args of
    [fname] -> do
      s <- readFile fname
      case parseGalapagos fname s of
        Left e ->
          error e
        Right cfg ->
          bracket prepareTerminal (const restoreTerminal) (const (run cfg))
    _ ->
      error $ "Usage: galapagos FILE"

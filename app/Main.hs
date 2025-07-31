{-# LANGUAGE BangPatterns #-}

import System.IO
import System.Console.ANSI
import System.Random
import System.Timeout (timeout)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.List (partition, find)
import Data.Char (toUpper)
import Text.Printf (printf)

-- Configurações
colWidth :: Int
colWidth = 7

columns :: [Char]
columns = ['a', 's', 'j', 'k']

height :: Int
height = 25

type Note = (Char, Int, Bool) -- tecla, linha, acertada?

data GameState = GameState
  { notes    :: [Note]
  , score    :: Int
  , combo    :: Int
  , gen      :: StdGen
  , gameOver :: Bool
  }

-- Inicialização
initGame :: IO GameState
initGame = do
  g <- newStdGen
  return $ GameState [] 0 0 g False

-- Geração e movimentação
addRandomNote :: StdGen -> [Note] -> ([Note], StdGen)
addRandomNote g ns =
  let (ix, g') = randomR (0, length columns - 1) g
      c = columns !! ix
  in ((c, 0, False) : ns, g')

moveNotesDown :: [Note] -> [Note]
moveNotesDown = map (\(c, y, h) -> (c, y + 1, h))

clearHitFlags :: [Note] -> [Note]
clearHitFlags = map (\(c, y, _) -> (c, y, False))

-- Função funcional de atualização (tick)
tick :: GameState -> (Bool, GameState)
tick gs =
  let movedNotes = moveNotesDown (notes gs)
      (shouldAdd, g1) = random (gen gs)
      (newNotes, g2) = if shouldAdd then addRandomNote g1 movedNotes else (movedNotes, g1)
      filteredNotes = filter (\(_, y, _) -> y < height + 2) newNotes
  in (shouldAdd, gs { notes = filteredNotes, gen = g2 })

-- Função funcional de entrada
applyInput :: Char -> GameState -> GameState
applyInput ' ' gs | gameOver gs = gs { notes = [], score = 0, combo = 0, gameOver = False }
applyInput c gs
  | c `elem` columns =
      let (hit, rest) = partition (\(nc, y, _) -> nc == c && y >= height - 2 && y <= height + 1) (notes gs)
          markedHits = map (\(nc, y, _) -> (nc, y, True)) hit
          newScore = if null hit then score gs - 1 else score gs + 1 + combo gs
          newCombo = if null hit then 0 else combo gs + 1
          isGameOver = newScore < -10
      in gs { notes = markedHits ++ rest, score = newScore, combo = newCombo, gameOver = isGameOver }
applyInput _ gs = gs

-- Composição funcional
gameStep :: GameState -> Maybe Char -> GameState
gameStep gs mInput =
  let afterInput = maybe gs (`applyInput` gs) mInput
      (_, ticked) = tick afterInput
      cleaned = ticked { notes = clearHitFlags (notes ticked) }
  in cleaned

-- Velocidade adaptativa
speed :: Int -> Int
speed sc = max 20000 (90000 - sc * 1000)

-- Renderização
centerText :: String -> String
centerText str =
  let pad = colWidth - length str
      l = pad `div` 2
      r = pad - l
  in replicate l ' ' ++ str ++ replicate r ' '

drawCell :: Char -> Int -> [Note] -> String
drawCell col y ns =
  let symbol = case find (\(c, ny, _) -> c == col && ny == y) ns of
                 Just (_, _, True)  -> "*"
                 Just (_, _, False) -> [toUpper col]
                 Nothing            -> " "
  in centerText symbol

drawKey :: Char -> String
drawKey c = centerText ("[" ++ [toUpper c] ++ "]")

drawGame :: GameState -> IO ()
drawGame gs = do
  putStr "\ESC[2J\ESC[H"
  hFlush stdout
  putStrLn $ "Score: " ++ show (score gs)
  putStrLn $ concatMap (const $ replicate colWidth '─') columns
  forM_ [0..height] $ \y -> do
    let line = concat [drawCell col y (notes gs) | col <- columns]
    putStrLn line
  putStrLn $ concatMap (const $ replicate colWidth '─') columns
  putStrLn $ concatMap drawKey columns
  putStrLn "\nUse A, S, J, K para tocar. Espaço para reiniciar."

  when (gameOver gs) $ do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "GAME OVER!"
    setSGR [Reset]

-- Loop principal de I/O
mainLoop :: GameState -> IO ()
mainLoop !gs = do
  drawGame gs
  input <- timeout (speed $ score gs) getChar
  let nextGS = gameStep gs input
  threadDelay (speed $ score nextGS `div` 2)
  mainLoop nextGS

-- Inicialização
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  hideCursor
  initGame >>= mainLoop

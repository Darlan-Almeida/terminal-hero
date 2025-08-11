{-# LANGUAGE BangPatterns #-}

import System.IO
import System.Console.ANSI
import System.Timeout (timeout)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Text.Printf (printf)
import Assets.Title (asciiTitle)
import Data.Time.Clock
import TerminalHero

-- Renderização da interface
drawGame :: GameState -> IO ()
drawGame gs = do
  setCursorPosition 0 0
  hFlush stdout
  let (diffColor, diffLevel) = case diff gs of
                  Easy   -> (Green,"Easy")
                  Medium -> (Yellow,"Medium")
                  Hard   -> (Red,"Hard")
  let header = "Score: " ++ show (score gs) ++ "  Combo: " ++ show (combo gs) ++ "  Dificuldade: " ++ setSGRCode [SetColor Foreground Vivid diffColor] ++ diffLevel ++ setSGRCode [Reset]
  putStrLn header

  putStrLn $ concatMap (const $ replicate colWidth '─') columns

  -- Game area
  forM_ [0..height] $ \y -> do
    if y == hitLine
      then putStrLn drawHitLineBackground
      else do
        let line = concatMap (\col -> drawCell col y (notes gs)) columns
        putStrLn line

  putStrLn $ concatMap (const $ replicate colWidth '─') columns
  putStrLn $ concatMap drawKey columns

  when (gameOver gs) $ do
    setSGR [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
    putStrLn "GAME OVER!" 
    setSGR [Reset]
    putStrLn "Aperte 'espaço' para reiniciar ou 'm' para voltar ao Menu"
  
  when (gameWon gs) $ do
    setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity]
    putStrLn "YOU ROCK!"
    setSGR [Reset]
    putStrLn "Aperte 'm' para voltar ao Menu"

-- Loop principal de I/O
mainLoop :: GameState -> IO ()
mainLoop !gs
  | gameOver gs || gameWon gs = do
      drawGame gs
      input <- getChar  
      case input of
        ' ' -> clearScreen >> setCursorPosition 0 0 >> initGame (diff gs) >>= mainLoop
        'm' -> mainMenu
        _ -> mainLoop gs
  | otherwise = do
      let frameTime = speed (score gs)
      startTime <- getCurrentTime
      drawGame gs
      input <- timeout frameTime getChar
      let nextGS = gameStep gs input
      endTime <- getCurrentTime
      let elapsedMicros = round $ 1000000 * realToFrac (diffUTCTime endTime startTime)
          remainingTime = max 0 (frameTime - elapsedMicros)
      threadDelay remainingTime
      mainLoop nextGS

--Encapsulamento das instruções
gameInstructions :: IO ()
gameInstructions = putStrLn "\n========CONTROLES========\nA, S, J, K para tocar!\nAcertos em sequência geram combos.\nCombos garantem bônus de pontuação!"
 
--Menu de seleção de dificuldade
difficultyMenu :: IO ()
difficultyMenu = do
  clearScreen
  setCursorPosition 0 0

  putStrLn $ setSGRCode [SetColor Foreground Vivid Red] ++ asciiTitle ++ setSGRCode [Reset] 
  putStrLn "Selecione a Dificuldade:"
  putStrLn $ setSGRCode [SetColor Foreground Vivid Green] ++ "1. Easy" ++ setSGRCode [Reset]
  putStrLn $ setSGRCode [SetColor Foreground Vivid Yellow] ++ "2. Medium" ++ setSGRCode [Reset]
  putStrLn $ setSGRCode [SetColor Foreground Vivid Red] ++ "3. Hard" ++ setSGRCode [Reset]
  gameInstructions
  hFlush stdout

  choice <- getChar
  case choice of
    '1' -> do clearScreen; setCursorPosition 0 0; initGame Easy >>= mainLoop
    '2' -> do clearScreen; setCursorPosition 0 0; initGame Medium >>= mainLoop
    '3' -> do clearScreen; setCursorPosition 0 0; initGame Hard >>= mainLoop
    _   -> difficultyMenu

-- Menu
mainMenu :: IO ()
mainMenu = do
  clearScreen
  setCursorPosition 0 0
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  hideCursor
 
  putStrLn $ setSGRCode [SetColor Foreground Vivid Red] ++ asciiTitle ++ setSGRCode [Reset] 
  putStrLn "1. Iniciar Jogo"
  putStrLn "2. Sair"
  putStr "\nEscolha uma opção: "
  hFlush stdout

  choice <- getChar
  case choice of
    '1' -> difficultyMenu
    '2' -> do
      clearScreen
      showCursor
      setSGR [Reset]
      putStrLn "Saindo de Terminal Hero"      
    _ -> mainMenu 

-- Inicialização
main :: IO ()
main = mainMenu
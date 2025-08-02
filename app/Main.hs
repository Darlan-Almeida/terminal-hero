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
import Assets.Title (asciiTitle)

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
  , gameWon  :: Bool
  }

-- Inicialização
initGame :: IO GameState
initGame = do
  g <- newStdGen
  return $ GameState [] 0 0 g False False

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

-- Zona de acerto das notas
hitLine :: Int
hitLine = height - 3  -- Ajustada para melhor jogabilidade

-- Visualização da zona de acerto das notas
drawHitLineBackground :: String
drawHitLineBackground = concatMap (\_ -> setSGRCode [SetConsoleIntensity BoldIntensity] ++ centerText "=" ++ setSGRCode [Reset]) columns

-- Verifica se a linha está na zona de acerto 
isHitZone :: Int -> Bool
isHitZone y = y >= hitLine - 1 && y <= hitLine + 1

-- Função funcional de atualização (tick)
tick :: GameState -> (Bool, GameState)
tick gs =
  let 
      activeNotes = filter (\(_, _, hit) -> not hit) (notes gs) --Remove todas as notas acertadas
    
      movedNotes = moveNotesDown activeNotes
      
      (shouldAdd, g1) = random (gen gs)
      (newNotes, g2) = if shouldAdd then addRandomNote g1 movedNotes else (movedNotes, g1)
      
      -- Checagem das notas perdidas
      (missedNotes, remainingNotes) = partition (\(_, y, hit) -> y >= height && not hit) newNotes
      missPenalty = length missedNotes * (1 + combo gs `div` 2)
      newScore = score gs - missPenalty
      newCombo = if null missedNotes then combo gs else 0
      isGameOver = newScore < -10
      filteredNotes = filter (\(_, y, _) -> y < height + 2) remainingNotes
  in (shouldAdd, gs { notes = filteredNotes, 
                      score = newScore, 
                      combo = newCombo, 
                      gen = g2, 
                      gameOver = isGameOver })

-- Função funcional de entrada
applyInput :: Char -> GameState -> GameState
applyInput ' ' gs | gameOver gs || gameWon gs = 
  gs { notes = [], score = 0, combo = 0, gameOver = False, gameWon = False }
applyInput c gs
  | c `elem` columns =
      let (hit, rest) = partition (\(nc, y, _) -> nc == c && isHitZone y) (notes gs)
          markedHits = map (\(nc, y, _) -> (nc, y, True)) hit
          newScore = if null hit then score gs - 1 else score gs + 1 + combo gs
          newCombo = if null hit then 0 else combo gs + 1
          isGameOver = newScore < -10
          isGameWon = newScore >= 1000  -- Win condition
      in gs { notes = markedHits ++ rest, 
              score = newScore, 
              combo = newCombo, 
              gameOver = isGameOver,
              gameWon = isGameWon }
applyInput _ gs = gs

-- Composição funcional
gameStep :: GameState -> Maybe Char -> GameState
gameStep gs mInput
  | gameOver gs || gameWon gs = gs -- Se acabar o game não faz nada
  | otherwise =
      let afterInput = maybe gs (`applyInput` gs) mInput
          (_, ticked) = tick afterInput
          cleaned = ticked { notes = clearHitFlags (notes ticked) }
          isGameWon = score cleaned >= 100  -- Condição de vitória, pode mudar com a dificuldade
      in if isGameWon 
         then cleaned { gameWon = True }
         else cleaned

-- Velocidade adaptativa
speed :: Int -> Int
speed sc 
  | sc < 10   = 150000  
  | sc < 30   = 120000  
  | sc < 50   = 100000  
  | sc < 100  = 80000  
  | otherwise = max 50000 (100000 - sc * 300) 

-- Renderização
centerText :: String -> String
centerText str =
  let pad = colWidth - length str
      l = pad `div` 2
      r = pad - l
  in replicate l ' ' ++ str ++ replicate r ' '

-- Paleta de cores da Palheta
getNoteColor :: Char -> Color
getNoteColor 'a' = Green    
getNoteColor 's' = Red      
getNoteColor 'j' = Yellow   
getNoteColor 'k' = Blue     
getNoteColor _   = White

drawCell :: Char -> Int -> [Note] -> String
drawCell col y ns =
  case find (\(c, ny, _) -> c == col && ny == y) ns of
    Just (_, _, hit) ->
      let color = if isHitZone y then Magenta else getNoteColor col
          symbol = if hit then "★" else [toUpper col]
      in setSGRCode [SetColor Foreground Vivid color] ++ centerText symbol ++ setSGRCode [Reset]
    Nothing -> centerText " "

drawKey :: Char -> String
drawKey c = 
  let color = getNoteColor c
  in setSGRCode [SetColor Foreground Vivid color] ++ centerText ("[" ++ [toUpper c] ++ "]") ++ setSGRCode [Reset]

drawGame :: GameState -> IO ()
drawGame gs = do
  setCursorPosition 0 0
  hFlush stdout
  
  let header = "Score: " ++ show (score gs) ++ "  Combo: " ++ show (combo gs)
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
  putStrLn "\nUse A, S, J, K para tocar. Pule com 'espaço' para recomeçar o show."

  when (gameOver gs) $ do
    setSGR [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
    putStrLn "GAME OVER! Aperte 'espaço' para reiniciar"
    setSGR [Reset]
  
  when (gameWon gs) $ do
    setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity]
    putStrLn "YOU ROCK!"
    setSGR [Reset]

-- Loop principal de I/O
mainLoop :: GameState -> IO ()
mainLoop !gs
  | gameOver gs || gameWon gs = do
      drawGame gs
      input <- getChar  
      case input of
        ' ' -> do
          clearScreen  -- Limpa a tela antes de reiniciar
          setCursorPosition 0 0 
          initGame >>= mainLoop  -- Resetar o game no espaço
        _   -> mainLoop gs          
  | otherwise = do
      drawGame gs
      input <- timeout (speed (score gs)) getChar
      let nextGS = gameStep gs input
      threadDelay (speed (score nextGS) `div` 2)
      mainLoop nextGS

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
    '1' -> do
      clearScreen
      setCursorPosition 0 0
      initGame >>= mainLoop
    '2' -> do
      clearScreen
      showCursor
      setSGR [Reset]
      putStrLn "Saindo de Terminal Hero"      
    _ -> mainMenu 

-- Inicialização
main :: IO ()
main = mainMenu
{-# LANGUAGE BangPatterns #-}

module TerminalHero
  ( Note
  , Difficulty(..)
  , GameState(..)
  , columns
  , height
  , colWidth
  , moveNotesDown
  , clearHitFlags
  , hitLine
  , isHitZone
  , noteGenChance
  , addRandomNote
  , tick
  , applyInput
  , gameStep
  , speed
  , centerText
  , initGame
  , getNoteColor
  , drawHitLineBackground
  , drawCell
  , drawKey
  ) where

import System.Random (StdGen, randomR, newStdGen)
import Data.List (partition, find)
import Data.Char (toUpper)
import System.Console.ANSI
  ( Color(..)
  , setSGRCode
  , ConsoleLayer(..)
  , ColorIntensity(..)
  , ConsoleIntensity(..)
  , SGR(..)
  )

-- Configurações
colWidth :: Int
colWidth = 7

columns :: [Char]
columns = ['a', 's', 'j', 'k']

height :: Int
height = 25

type Note = (Char, Int, Bool) -- tecla, linha, acertada?

data Difficulty = Easy | Medium | Hard deriving (Eq, Show)
data GameState = GameState
  { notes    :: [Note]
  , score    :: Int
  , combo    :: Int
  , gen      :: StdGen
  , gameOver :: Bool
  , gameWon  :: Bool
  , diff     :: Difficulty
  }

-- Inicialização
initGame :: Difficulty -> IO GameState
initGame diff = do
  g <- newStdGen
  return $ GameState [] 0 0 g False False diff

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

-- Verifica se a linha está na zona de acerto 
isHitZone :: Int -> Bool
isHitZone y = y >= hitLine - 1 && y <= hitLine + 1

-- A chance da nota ser gerada é baseada na dificuldade escolihda
noteGenChance :: Difficulty -> Float
noteGenChance Easy   = 0.25
noteGenChance Medium = 0.5
noteGenChance Hard   = 0.75

-- Função funcional de atualização (tick)
tick :: GameState -> (Bool, GameState)
tick gs =
  let 
      activeNotes = filter (\(_, _, hit) -> not hit) (notes gs) --Remove todas as notas acertadas
    
      movedNotes = moveNotesDown activeNotes
      
      (randVal, g1) = randomR (0.0, 1.0) (gen gs)
      shouldAdd = randVal < noteGenChance (diff gs)

      (newNotes, g2) = if shouldAdd then addRandomNote g1 movedNotes else (movedNotes, g1)
      
      -- Checagem das notas perdidas
      (missedNotes, remainingNotes) = partition (\(_, y, hit) -> y >= height && not hit) newNotes
      missPenalityPerDiff = case diff gs of
        Easy -> 1
        Medium -> 2
        Hard -> 3
      missPenalty = length missedNotes * (missPenalityPerDiff + combo gs `div` 2)
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
          gainPoints = case diff gs of
            Easy -> 3
            Medium -> 2
            Hard -> 1
          lossPoints = case diff gs of
            Easy -> 1
            Medium -> 2
            Hard -> 3
          newScore = if null hit then score gs - lossPoints else score gs + gainPoints + combo gs
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
  | sc < 20   = 140000  
  | sc < 50   = 130000  
  | sc < 80   = 125000  
  | sc < 120  = 100000  
  | otherwise = max 70000 (100000 - sc * 300) 

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

-- Visualização da zona de acerto das notas
drawHitLineBackground :: String
drawHitLineBackground = concatMap (\_ -> setSGRCode [SetConsoleIntensity BoldIntensity] ++ centerText "=" ++ setSGRCode [Reset]) columns

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
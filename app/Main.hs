import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, mkStdGen, randomRs)
import Data.List (partition)

type Position = (Float, Float)
type KeyChar = Char

data Note = Note
  { position :: Position
  , key :: KeyChar
  }

data GameState = GameState
  { notes     :: [Note]
  , score     :: Int
  , timer     :: Float
  , gen       :: [Int]
  , gameOver  :: Bool
  }

columns :: [(KeyChar, Float)]
columns = [('d', -150), ('f', -50), ('j', 50), ('k', 150)]

initialState :: GameState
initialState = GameState
  { notes = []
  , score = 0
  , timer = 0
  , gen = randomRs (0, 3) (mkStdGen 42)
  , gameOver = False
  }

render :: GameState -> Picture
render gs
  | gameOver gs = Translate (-150) 0 $ Scale 0.3 0.3 $ Color red $ Text "Game Over!\nPress SPACE to restart"
  | otherwise = Pictures $
      [ Pictures
          [ Translate x (-250) $ Color green $ rectangleSolid 80 20
          , Translate (x - 10) (-260) $ Scale 0.2 0.2 $ Color black $ Text [c]
          ]
        | (c, x) <- columns
      ] ++
      [ uncurry Translate pos $ Color blue $ circleSolid 20 | Note pos _ <- notes gs ] ++
      [ Translate (-280) 250 $ Scale 0.15 0.15 $ Color black $ Text ("Score: " ++ show (score gs)) ]

-- Velocidade aumenta com o tempo
noteSpeed :: Float -> Float
noteSpeed t = 100 + min 300 (t * 20)

-- Atualiza estado
update :: Float -> GameState -> GameState
update dt gs
  | gameOver gs = gs
  | otherwise =
      let movedNotes = map moveNote (notes gs)
          newTimer = timer gs + dt
          (noteIndex:restGen) = gen gs
          (k, x) = columns !! noteIndex
          newNote
            | newTimer >= 0.8 = [Note (x, 300) k]
            | otherwise       = []
          updatedScore = score gs
          gameOverNow = updatedScore < -10
      in GameState
          { notes = movedNotes ++ newNote
          , score = updatedScore
          , timer = if null newNote then newTimer else 0
          , gen = restGen
          , gameOver = gameOverNow
          }
  where
    moveNote (Note (x, y) k) = Note (x, y - noteSpeed (timer gs) * dt) k

-- Evento do teclado
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (Char c) Down _ _) gs
  | gameOver gs = gs
  | Just x <- lookup c columns =
      let (hit, rest) = partition (\(Note (nx, ny) kc) -> kc == c && abs (ny + 250) < 25) (notes gs)
          colHasNote = any (\(Note _ kc) -> kc == c) (notes gs)
      in if not (null hit)
         then gs { notes = rest, score = score gs + 1 }
         else let newScore = score gs - 1
     in gs { score = newScore, gameOver = newScore < -10 }

handleEvent (EventKey (SpecialKey KeySpace) Down _ _) gs
  | gameOver gs = initialState
handleEvent _ gs = gs

main :: IO ()
main = play
  (InWindow "Haskell Hero" (600, 600) (100, 100))
  white
  60
  initialState
  render
  handleEvent
  update

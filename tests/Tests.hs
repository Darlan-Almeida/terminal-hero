{-# LANGUAGE BangPatterns #-}
module Main where

import Test.HUnit
import System.Random (mkStdGen)
import TerminalHero
import System.Exit (exitFailure)

-- Estado base para os testes
testState :: GameState
testState = GameState
  { notes    = [('a', 5, False), ('s', 10, True), ('j', 22, False)]
  , score    = 100
  , combo    = 5
  , gen      = mkStdGen 42
  , gameOver = False
  , gameWon  = False
  , diff     = Medium
  }

-- Testa se moveNotesDown incrementa a linha das notas corretamente
test_moveNotesDown = TestCase $
  assertEqual "moveNotesDown" 
    [('a', 6, False), ('s', 11, True)]
    (moveNotesDown [('a', 5, False), ('s', 10, True)])

-- Testa se clearHitFlags limpa o flag de acerto das notas
test_clearHitFlags = TestCase $
  assertEqual "clearHitFlags"
    [('a', 5, False), ('s', 10, False)]
    (clearHitFlags [('a', 5, True), ('s', 10, False)])

-- Testa se isHitZone identifica corretamente a zona de acerto
test_isHitZone = TestCase $ do
  assertBool "hitLine" (isHitZone hitLine)
  assertBool "hitLine-1" (isHitZone (hitLine-1))
  assertBool "hitLine+1" (isHitZone (hitLine+1))
  assertBool "not hitLine-2" (not (isHitZone (hitLine-2)))
  assertBool "not hitLine+2" (not (isHitZone (hitLine+2)))

-- Testa se noteGenChance retorna a chance correta para cada dificuldade
test_noteGenChance = TestCase $ do
  assertEqual "Easy"   0.25 (noteGenChance Easy)
  assertEqual "Medium" 0.5  (noteGenChance Medium)
  assertEqual "Hard"   0.75 (noteGenChance Hard)

-- Testa se addRandomNote adiciona uma nota válida na posição inicial
test_addRandomNote = TestCase $ do
  let (ns, _) = addRandomNote (gen testState) []
      (c, y, h) = head ns
  assertBool "tecla válida" (c `elem` columns)
  assertEqual "y==0" 0 y
  assertEqual "hit==False" False h

-- Testa se applyInput marca a nota como acertada, soma pontos e aumenta combo ao acertar
test_applyInput_acerto = TestCase $ do
  let st = testState { notes = [('a', hitLine, False)] }
      st' = applyInput 'a' st
      (_,_,h) = head (notes st')
  assertBool "acertou" h
  assertEqual "score" 107 (score st')
  assertEqual "combo" 6 (combo st')

-- Testa se applyInput penaliza e zera combo ao errar a tecla
test_applyInput_erro = TestCase $ do
  let st = testState { notes = [('a', hitLine, False)] }
      st' = applyInput 's' st
      (_,_,h) = head (notes st')
  assertBool "não acertou" (not h)
  assertEqual "score" 98 (score st')
  assertEqual "combo" 0 (combo st')

-- Testa se applyInput penaliza ao tentar acertar fora da zona de acerto
test_applyInput_foraZona = TestCase $ do
  let st = testState { notes = [('a', hitLine-2, False)] }
      st' = applyInput 'a' st
      (_,_,h) = head (notes st')
  assertBool "não acertou" (not h)
  assertEqual "score" 98 (score st')

-- Testa se applyInput reinicia o jogo corretamente ao pressionar espaço após game over
test_applyInput_restart = TestCase $ do
  let st = testState { gameOver = True }
      st' = applyInput ' ' st
  assertBool "reiniciou" (not (gameOver st'))
  assertEqual "score" 0 (score st')
  assertEqual "combo" 0 (combo st')

-- Testa se tick move a nota para baixo corretamente
test_tick_movimento = TestCase $ do
  let st = testState { notes = [('a', 5, False)] }
      (_, st') = tick st
      (_, y, _) = last (notes st')
  assertEqual "nota moveu" 6 y

-- Testa se speed retorna o tempo correto para diferentes scores
test_speed = TestCase $ do
  assertEqual "speed 0" 140000 (speed 0)
  assertEqual "speed 30" 130000 (speed 30)
  assertEqual "speed 70" 125000 (speed 70)
  assertEqual "speed 150" 70000 (speed 150)

-- Testa se centerText centraliza corretamente textos de diferentes tamanhos
test_centerText = TestCase $ do
  assertEqual "curto" "   a   " (centerText "a")
  assertEqual "medio" "  abc  " (centerText "abc")
  assertEqual "longo" "abcdefg" (centerText "abcdefg")

tests :: Test
tests = TestList
  [ test_moveNotesDown
  , test_clearHitFlags
  , test_isHitZone
  , test_noteGenChance
  , test_addRandomNote
  , test_applyInput_acerto
  , test_applyInput_erro
  , test_applyInput_foraZona
  , test_applyInput_restart
  , test_tick_movimento
  , test_speed
  , test_centerText
  ]

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts > 0 || failures counts > 0
    then exitFailure
    else return ()
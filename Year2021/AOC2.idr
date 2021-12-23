module Year2021.AOC2

import Data.List
import Data.String

import Control.Monad.State

data Command = Forward Int | Down Int | Up Int

readCommand : IO (Maybe Command)
readCommand = do
  line <- getLine
  let [dir, num] = words line
    | _ => pure Nothing
  pure $ case (dir, parseInteger num) of
    ("forward", Just i) => Just $ Forward i
    ("down", Just i) => Just $ Down i
    ("up", Just i) => Just $ Up i
    _ => Nothing

readCommands : IO (List Command)
readCommands = do
  Just command <- readCommand
    | Nothing => pure []
  (command ::) <$> readCommands

sumCommands1 : List Command -> (Int, Int)
sumCommands1 [] = (0, 0)
sumCommands1 (h::t) =
  let
    (h_change, v_change) = change
    (h_tail, v_tail) = tail
  in (h_change + h_tail, v_change + v_tail)
  where
    change : (Int, Int)
    change =
      case h of
        Forward i => (i, 0)
        Up i => (0, -i)
        Down i => (0, i)
    tail : (Int, Int)
    tail = sumCommands1 t

main1 : IO ()
main1 = do
  commands <- readCommands
  let (h_final, v_final) = sumCommands1 commands
  printLn (h_final, v_final)
  printLn $ h_final * v_final

runCommand : Command -> State (Int, Int, Int) ()
runCommand command = do
  (h, v, a) <- get
  put $ case command of
    Forward i => (h + i, v + a * i, a)
    Up i => (h, v, a - i)
    Down i => (h, v, a + i)

sumCommands2 : List Command -> State (Int, Int, Int) ()
sumCommands2 l = sequence_ $ (runCommand <$> l)

main2 : IO ()
main2 = do
  commands <- readCommands
  let (h_final, v_final, a_final) = execState (0, 0, 0) $ sumCommands2 commands
  printLn (h_final, v_final, a_final)
  printLn $ h_final * v_final

module Main where

import Quests
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then runQuest 6
    else runQuest $ read $ args !! 0
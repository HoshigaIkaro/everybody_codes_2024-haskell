module Main where

import Quests
import System.Environment (getArgs)
import Control.Concurrent (setNumCapabilities)

main :: IO ()
main = do
  setNumCapabilities 32
  args <- getArgs
  if length args == 0
    then runQuest 7
    else runQuest $ read $ args !! 0
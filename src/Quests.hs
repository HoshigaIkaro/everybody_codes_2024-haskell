{-# LANGUAGE OverloadedStrings #-}

module Quests where

import Data.Text qualified as T
import Quests.Q1 qualified as Q1
import Quests.Q2 qualified as Q2
import Quests.Q3 qualified as Q3
import Quests.Q4 qualified as Q4
import Quests.Q5 qualified as Q5
import Quests.Q6 qualified as Q6
import Quests.Q7 qualified as Q7
import Quests.Q8 qualified as Q8

import Control.Monad (void)

runQuest :: Int -> IO ()
runQuest quest =
    case quest of
        1 -> Q1.run
        2 -> Q2.run
        3 -> Q3.run
        4 -> Q4.run
        5 -> Q5.run
        6 -> Q6.run
        7 -> Q7.run
        _ -> do
            let prefix = "./input/q"
                firstInputName = prefix <> show quest <> "/p1.txt"
                secondInputName = prefix <> show quest <> "/p2.txt"
                thirdInputName = prefix <> show quest <> "/p3.txt"
                trim = (T.unpack . T.strip . T.pack)
            firstInput <- trim <$> readFile firstInputName
            secondInput <- trim <$> readFile secondInputName
            thirdInput <- trim <$> readFile thirdInputName
            print $ Q8.part1 firstInput
            print $ Q8.part2 secondInput
            print $ Q8.part3 thirdInput

-- return ()
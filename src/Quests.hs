{-# LANGUAGE OverloadedStrings #-}

module Quests where

import Data.Text qualified as T
import Quests.Q1 qualified as Q1
import Quests.Q2 qualified as Q2
import Quests.Q3 qualified as Q3

-- import Control.Monad (void)

runQuest :: Int -> IO ()
runQuest quest =
    case quest of
        1 -> Q1.run
        2 -> Q2.run
        _ -> do
            let prefix = "./input/q"
                firstInputName = prefix <> show quest <> "p1.txt"
                secondInputName = prefix <> show quest <> "p2.txt"
                thirdInputName = prefix <> show quest <> "p3.txt"
                trim = (T.strip . T.pack)
            firstInput <- trim <$> readFile firstInputName
            secondInput <- trim <$> readFile secondInputName
            thirdInput <- trim <$> readFile thirdInputName
            print $ Q2.part1 firstInput
            print $ Q2.part2 secondInput
            print $ Q2.part3 thirdInput
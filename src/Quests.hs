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
import Quests.Q9 qualified as Q9
import Quests.Q10 qualified as Q10
import Quests.Q11 qualified as Q11
import Quests.Q12 qualified as Q12
import Quests.Q13 qualified as Q13
import Quests.Q14 qualified as Q14

-- import Control.Monad (void)

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
        8 -> Q8.run
        9 -> Q9.run
        10 -> Q10.run
        11 -> Q11.run
        12 -> Q12.run
        13 -> Q13.run
        14 -> Q14.run
        _ -> do
            let prefix = "./input/q"
                firstInputName = prefix <> show quest <> "/p1.txt"
                secondInputName = prefix <> show quest <> "/p2.txt"
                thirdInputName = prefix <> show quest <> "/p3.txt"
                trim = T.unpack . T.strip . T.pack
            firstInput <- trim <$> readFile firstInputName
            secondInput <- trim <$> readFile secondInputName
            thirdInput <- trim <$> readFile thirdInputName
            print $ Q14.part1 firstInput
            print $ Q14.part2 secondInput
            print $ Q14.part3 thirdInput

-- value <- Q10.part3 thirdInput
-- print value

-- return ()
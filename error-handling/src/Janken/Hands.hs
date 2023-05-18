module Janken.Hands
    (
        Hands (..)
      , Result (..)
      , game
    ) where

data Hands = Goo | Choki | Par
    deriving (Show, Eq)

data Result = Win | Lose | Draw
    deriving (Show, Eq)

game :: Hands -> Hands -> Result
game self other 
    | self == other  = Draw
    | otherwise = judgeWin self other
        where
            judgeWin :: Hands -> Hands -> Result
            judgeWin Goo Choki = Win
            judgeWin Choki Par = Win
            judgeWin Par Goo   = Win
            judgeWin _ _       = Lose

module Main (main) where

import Janken.Hands as Janken

f :: Int -> Int
f a = a * 2

test :: Maybe Int
test = do
    s <- fmap f $ Just 1
    return s

main :: IO ()
main = do
    print $ test

module Main (main) where

import Janken.Hands as Janken

main :: IO ()
main = do
    print $ g
    print $ Janken.game Janken.Goo Janken.Goo
    print $ Janken.game Janken.Goo Janken.Par
    print $ Janken.game Janken.Goo Janken.Choki

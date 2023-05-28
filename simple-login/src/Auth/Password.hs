{-# LANGUAGE OverloadedStrings #-}
module Auth.Password where

import Text.Printf
import Data.ByteString hiding (map, concat)
import Data.Word
import qualified Crypto.Hash.SHA512 as SHA512

salt :: ByteString
salt = "mhSJA3+:d}}+e=.NS3Yq[T{'ptB?L72k[LkAGf8cy*\\/Be+KoC;C?Mv&t!|XA*WZ2,~#;Q;nNz4Y6MGe8j5O(ANL9/}}vaNN[t}2"

hashCount :: Int 
hashCount = 90000

hashedPassword :: ByteString -> String
hashedPassword password = concat $ map (printf "%02x") $ unpack $ hashingN hashCount password

hashingN :: Int -> ByteString -> ByteString
hashingN 0 s = SHA512.hash s
hashingN n s = hashingN (n - 1) $ mappend salt $ SHA512.hash s


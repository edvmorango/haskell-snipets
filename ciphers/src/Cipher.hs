module Cipher where

import Data.Char

type Shift = Int

encode :: Shift -> String -> String
encode c a = map (\x ->  chr $ (ord x) + c ) a

decode :: Shift -> String -> String
decode c a = map (\x ->  chr $ (ord x) - c ) a


dynamicEncode :: IO ()
dynamicEncode = do
  putStr "Put the Shift size: "
  size <- getLine
  putStr "Put a word to encode: "
  word <- getLine
  putStrLn $ encode (read size :: Int) word


dynamicDecode :: IO ()
dynamicDecode = do
  putStr "Put the Shift size: "
  size <- getLine
  putStr "Put a word to decode: "
  word <- getLine
  putStrLn $ decode (read size :: Int) word

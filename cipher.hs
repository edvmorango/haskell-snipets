module Cipher where

import Data.Char

type Shift = Int

encode :: Shift -> String -> String
encode c a = map (\x ->  chr $ (ord x) + c ) a

decode :: Shift -> String -> String
decode c a = map (\x ->  chr $ (ord x) - c ) a

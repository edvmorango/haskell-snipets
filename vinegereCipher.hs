module VinegereChiper where

import Data.Char

type Shift = [Int]

alphSize = 26

makeShift :: String -> Shift
makeShift = map (\x -> ord (toUpper x) - 65  )

calcShift :: Shift -> String -> [(Int, Int)]
calcShift _ [] = []
calcShift s (' ':t) = [(ord ' ',0)] ++ calcShift s t
calcShift (sh:st) (h:t) =  [(ord h,sh)] ++ calcShift (st ++ [sh]) t

encode :: Shift -> String -> String
encode sh msg = map (\(c,s) -> case s of
                                0 -> chr c
                                _ -> if (c + s) >= alphSize
                                     then chr $ 65 + ((c - 65 + s) `mod` alphSize)
                                     else chr $ c + s ) toShift
  where toShift =  calcShift sh msg

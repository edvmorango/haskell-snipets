module Palindrome where

import Control.Monad
import Data.Char
import System.Exit (exitSuccess)



isPalindrome :: String -> Bool
isPalindrome w =  fw == reverse fw
  where fw =  (filter (\a ->  a >= 'a' && a <= 'z') . map (toLower) ) w


palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (isPalindrome line1) of
    True  -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

module Print1 where

main :: IO()
main = putStrLn "Hello String!"

main2 :: IO()
main2 = do
  putStrLn "1"
  putStrLn "2"
  putStrLn "3"


greeting :: String
greeting = "hello" ++ " concat"

hello :: String
hello = "Hello"

helloConcat :: String
helloConcat = "concat!"

main3 :: IO()
main3 = do
  putStrLn greeting
  putStrLn greeting2
  where
    greeting2 = concat [hello, " ", helloConcat]


area :: Double -> Double
area d = pi * ( r * r)
  where r = d / 2

ex1 :: String -> String
ex1 v = tail v

ex2 :: String -> String
ex2 v = v ++ "!"

ex3 :: String -> String
ex3 v = drop 4 v

ex4 :: String -> String
ex4 v = drop 9 v

ex5 :: String -> Char
ex5 v = head (drop 2 v)

ex6 :: Int -> Char
ex6 v = "Curry is awesome!" !! v

ex7 :: String
ex7 =  c ++ b ++ a
  where
    v = "Curry is awesome!"
    a = take 6 v
    b = take 4 (drop 5 v)
    c = take 8 (drop 9 v)

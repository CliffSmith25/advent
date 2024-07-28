module Main where

import Text.Regex.Posix

main :: IO ()
main = 
  readFile "input.txt"
    >>= return . lines
    >>= return . map keepDigits
    >>= print

keepDigits :: String -> String
keepDigits input =
  let regex = "[0-9]|one|two|three|four|five|six|seven|eight|nine|zero" 
      outputs = getAllTextMatches (input =~ regex) :: [String]
  in head outputs

keepFirstAndLast :: String -> Integer
keepFirstAndLast input =
  let stringNums = [head input, last input]
  in read stringNums :: Integer

module Main where

import Text.Regex.Posix

main :: IO ()
main = 
  readFile "input.txt"
    >>= return . lines
    >>= return . map keepDigits
    >>= return . map keepFirstAndLast
    >>= print

keepDigits :: String -> String
keepDigits input =
  let regex = "[0-9]|one|two|three|four|five|six|seven|eight|nine|zero" 
      outputs = getAllTextMatches (input =~ regex) :: [String]
  in head outputs

replaceNums :: String -> String
replaceNums nums = 
      let replaces = zip ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [0..9]
          newString = [[b] | (a, b) <- replaces, b == nums]
      if length newString == 0 then nums else newStrinsg


keepFirstAndLast :: String -> Integer
keepFirstAndLast input =
  let stringNums = [head input, last input]
  in read stringNums :: Integer

module Main where

import Text.Regex.Posix

main :: IO ()
main =
  readFile "input.txt"
    >>= return . lines
    >>= return . map keepDigits
    >>= return . map keepFirstAndLast
    >>= return . sum
    >>= print

keepDigits :: String -> String
keepDigits input =
  let regex = "[0-9]|one|two|three|four|five|six|seven|eight|nine|zero"
      outputs = getAllTextMatches (input =~ regex) :: [String]
   in concatMap replaceNums outputs

replaceNums :: String -> String
replaceNums nums =
  let replaces =
        zip
          ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
          ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
      newString = [b | (a, b) <- replaces, a == nums]
   in if null newString then nums else head newString

keepFirstAndLast :: String -> Integer
keepFirstAndLast input =
  let stringNums = [head input, last input]
   in read stringNums :: Integer

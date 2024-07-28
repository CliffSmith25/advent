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
  [c | c <- input, c >= '0' && c <= '9']

keepFirstAndLast :: String -> Integer
keepFirstAndLast input =
  let stringNums = [head input, last input]
  in read stringNums :: Integer

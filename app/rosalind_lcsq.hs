module Main (main) where

-- calculateLCS :: String -> String -> String -> Int -> Int -> String
-- calculateLCS acc str1 str2 i j
--   | i == length str1 = calculateLCS ("" <> acc) str1 str2 i (j + 1)
--   | j == length str2 = calculateLCS ("" <> acc) str1 str2 (i + 1) j
--   | (str1 !! i) == (str2 !! j) = calculateLCS (printf "%s%c" acc (str1 !! i)) str1 str2 i (j + 1)
--   | i < length str1 && j < i = calculateLCS ("" <> acc) str1 str2 i (j + 1)
--   | j < length str2 && i < j = calculateLCS ("" <> acc) str1 str2 (i + 1) j
--   | otherwise = acc

checkMatch :: String -> String -> Int -> Int -> Maybe String
checkMatch str1 str2 i j
  | ((i < length str1) && (j < length str2)) && (str1 !! i == str2 !! j) =
    Just $ str1 !! i : masterLoop str1 str2 (i + 1) (j + 1)
  | otherwise = Nothing

skipFirst :: String -> String -> Int -> Int -> String
skipFirst str1 str2 i j
  | i == length str1 = ""
  | otherwise = case checkMatch str1 str2 i j of
    Just result -> result
    Nothing -> masterLoop str1 str2 (i + 1) j

skipSecond :: String -> String -> Int -> Int -> String
skipSecond str1 str2 i j
  | j == length str2 = ""
  | otherwise = case checkMatch str1 str2 i j of
    Just result -> result
    Nothing -> masterLoop str1 str2 i (j + 1)

masterLoop :: String -> String -> Int -> Int -> String
masterLoop str1 str2 i j
  | i >= length str1 && j >= length str2 = ""
  | case checkMatch str1 str2 i j of
      Just _ -> True
      Nothing -> False = case checkMatch str1 str2 i j of
    Just result -> result
    Nothing -> max (skipFirst str1 str2 i j) (skipSecond str1 str2 i j)
  | i == length str1 = skipSecond str1 str2 i j
  | j == length str2 = skipFirst str1 str2 i j
  | otherwise = max (skipFirst str1 str2 i j) (skipSecond str1 str2 i j)

main :: IO ()
main = do
  let str1 = "AACCTTGG"
  let str2 = "ACACTGTGA"

  -- let lcs = calculateLCS "" str1 str2 0 1

  let result = masterLoop str1 str2 0 0

  putStrLn result

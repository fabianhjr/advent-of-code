module Year2021.AOC1

import Data.List
import Data.String

readNumber : IO (Maybe Int)
readNumber = parseInteger <$> getLine

readNumbers : IO (List Int)
readNumbers = do
  m <- readNumber
  case m of
    Nothing => pure []
    Just n => (n ::) <$> readNumbers


main1 : IO ()
main1 = do
  numbers <- readNumbers
  let increments = zipWith (<) numbers (drop 1 numbers)
  printLn $ length $ filter (== True) increments

main2 : IO ()
main2 = do
  numbers <- readNumbers
  let summed = zipWith3 (\x, y, z => x + y + z) numbers (drop 1 numbers) (drop 2 numbers)
  let increments = zipWith (<) summed (drop 1 summed)
  printLn $ length $ filter (== True) increments

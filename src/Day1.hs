module Day1 (main) where

import           Paths_aoc2019         (getDataFileName)

type Mass = Integer

type Fuel = Integer

fuelModule :: Mass -> Fuel
fuelModule m = (m `div` 3) -2


fuelTotal :: (Mass, Fuel) -> (Mass, Fuel)
fuelTotal (n,f)
  | n<9 = (0,f)
  | otherwise = fuelTotal(fm,f + fm) where
      fm = fuelModule n

parseInput :: String -> [Mass]
parseInput  = map read . lines

loadInput :: IO String
loadInput = getDataFileName "inputs/day1.txt" >>= readFile

main :: IO ()
main = day1

day1 :: IO ()
day1 = do
  input <- fmap parseInput loadInput
  print $ foldr (+) 0 $ map fuelModule input
  print $ foldr (+) 0 $ map (snd . fuelTotal . (\x -> (x,0))) input 

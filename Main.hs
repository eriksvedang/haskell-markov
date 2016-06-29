module Main where

import System.Random
import Control.Monad.State
import System.IO
import System.Environment
import Data.List
import qualified Data.Map.Strict as Map

type Freq = Int
type Weights = [(Char, Freq)]
type Mapping = Map.Map Char Weights

addToAL :: Eq key => [(key, elt)] -> key -> elt -> [(key, elt)]
addToAL l key value = (key, value) : delFromAL l key

delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = filter (\a -> (fst a) /= key) l

-- | Sums up the frequency count of a list of weights.
-- | i.e. totalFreq [('a',10),('b',7)] => 17
totalFreq :: [(t, Int)] -> Int
totalFreq weights = foldl' (\total (_, x) -> total + x) 0 weights

-- | Given a list of frequency weigths, find the weight at the position between 0 and the total frequency count.
-- | i.e. findSlice [('a',10),('b',7)] 12 => Just 'b'
findSlice weights n = 
  let finder (totalIndex, result) weight = case result of
        Just _ -> (totalIndex, result)
        Nothing -> let (c, freq) = weight
                       totalIndex' = totalIndex + freq
                       result' = if totalIndex' >= n then (Just c) else Nothing
                   in (totalIndex', result')
  in snd $ foldl' finder (0 :: Int, Nothing) weights

nextChar :: Mapping -> Char -> State StdGen Char
nextChar mapping currentChar =
  case (Map.lookup currentChar mapping) of
    Just weights -> do
      g <- get
      let max = totalFreq weights
          (r, g') = (randomR (0 :: Int, max) g)
      put g'
      return $ case (findSlice weights r) of
        Just x -> x
        Nothing -> '?'
    Nothing ->
      return '?'

chain :: Mapping -> Char -> Int -> State StdGen [Char]
chain mappings start n = 
  let folder total _ = do
        c <- (nextChar mappings (head total))
        return $ c : total
  in do result <- foldM folder [start] [1..n :: Int]
        return (reverse result)       

increaseFreq :: Weights -> Char -> Weights
increaseFreq weights c =
  addToAL weights c $ case lookup c weights of
                        (Just freq) -> freq + 1
                        Nothing -> 1

visit :: Mapping -> (Char, Char) -> Mapping
visit mappings (c, next) =
  case (Map.lookup c mappings) of
    (Just weights) -> Map.insert c (increaseFreq weights next) mappings
    Nothing -> Map.insert c [(next, 1)] mappings

analyze :: [Char] -> Mapping
analyze input = foldl' visit Map.empty (zip input (tail input))

run randomGen mapping start = evalState (chain mapping start 120) randomGen

main :: IO ()
main = do
  g <- newStdGen
  args <- getArgs
  text <- case args of
            (file : _) -> readFile file
            _ -> return "erorikokkk"  
  --putStrLn (show $ analyze text)
  let mapping = analyze text
      result = run g mapping (head text)
  putStrLn (show result)
  

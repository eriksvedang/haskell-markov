module Main where

import System.Random
import Control.Monad.State
import System.IO
import System.Environment
import Data.List
import Data.Char
import qualified Data.Map.Strict as Map

type Freq = Int
type Weights = Map.Map Char Freq
type Mapping = Map.Map Char Weights

totalFreq :: Weights -> Int
totalFreq weights = Map.foldl' (\total x -> total + x) 0 weights

findCharSlice :: Weights -> Int -> Maybe Char
findCharSlice weights n = 
  let finder (totalIndex, result) weight = case result of
        Just _ -> (totalIndex, result)
        Nothing -> let (c, freq) = weight
                       totalIndex' = totalIndex + freq
                       result' = if totalIndex' >= n then (Just c) else Nothing
                   in (totalIndex', result')
  in snd $ foldl' finder (0 :: Int, Nothing) (Map.toList weights)

nextChar :: Mapping -> Char -> State StdGen Char
nextChar mapping currentChar =
  case (Map.lookup currentChar mapping) of
    Just weights -> do
      g <- get
      let max = totalFreq weights
          (r, g') = (randomR (0 :: Int, max) g)
      put g'
      return $ case (findCharSlice weights r) of
        Just x -> x
        Nothing -> error "Failed to find a slice."
    Nothing ->
      return '?' -- no entry for the char

chain :: Mapping -> Char -> Int -> State StdGen [Char]
chain mappings start n = 
  let folder total _ = do
        c <- (nextChar mappings (head total))
        return $ c : total
  in do result <- foldM folder [start] [1..n :: Int]
        return (reverse result)       

increaseFreq :: Weights -> Char -> Weights
increaseFreq weights char =
  let newFreq = case Map.lookup char weights of
                  (Just freq) -> freq + 1
                  Nothing -> 1
  in Map.insert char newFreq weights

visitChar :: Mapping -> (Char, Char) -> Mapping
visitChar mappings (currentChar, nextChar) =
  let newWeights = case (Map.lookup currentChar mappings) of
                     (Just weights) -> increaseFreq weights nextChar
                     Nothing -> Map.singleton nextChar 1
  in Map.insert currentChar newWeights mappings

analyze :: [Char] -> Mapping
analyze input = foldl' visitChar Map.empty (zip input (tail input))

run randomGen mapping start = evalState (chain mapping start 5000) randomGen

goodChar c = isAlpha c || c == ' '

main :: IO ()
main = do
  g <- newStdGen
  args <- getArgs
  text <- case args of
            (file : _) -> fmap (filter goodChar) (readFile file)
            _ -> return "Ajabaja hokus pokus filijokus haha"
  let mapping = analyze text
      result = run g mapping (head text)
  putStrLn result
  

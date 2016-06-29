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

-- | Sums up the frequency count of a list of weights.
totalFreq :: Weights -> Int
totalFreq weights = Map.foldl' (\total x -> total + x) 0 weights

-- | Given a list of frequency weigths, find the weight at the position between 0 and the total frequency count.
findSlice :: Weights -> Int -> Maybe Char
findSlice weights n = 
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
      return $ case (findSlice weights r) of
        Just x -> x
        Nothing -> '?'
    Nothing ->
      return '.'

chain :: Mapping -> Char -> Int -> State StdGen [Char]
chain mappings start n = 
  let folder total _ = do
        c <- (nextChar mappings (head total))
        return $ c : total
  in do result <- foldM folder [start] [1..n :: Int]
        return (reverse result)       

increaseFreq :: Weights -> Char -> Weights
increaseFreq weights c =
  let x = case Map.lookup c weights of
                 (Just freq) -> freq + 1
                 Nothing -> 1
  in Map.insert c x weights

visit :: Mapping -> (Char, Char) -> Mapping
visit mappings (c, next) =
  case (Map.lookup c mappings) of
    (Just weights) -> Map.insert c (increaseFreq weights next) mappings
    Nothing -> Map.insert c (Map.singleton next 1) mappings

analyze :: [Char] -> Mapping
analyze input = foldl' visit Map.empty (zip input (tail input))

run randomGen mapping start = evalState (chain mapping start 5000) randomGen

goodChar c = isAlpha c || c == ' '

main :: IO ()
main = do
  g <- newStdGen
  args <- getArgs
  text <- case args of
            (file : _) -> readFile file
            _ -> return "ajabaja hokus pokus filijokus haha"
  let goodText = filter goodChar text
  -- putStrLn goodText
  -- putStrLn (show $ analyze goodText)
  -- putStrLn $ show (head goodText)
  let mapping = analyze goodText
      result = run g mapping (head goodText)
  putStrLn result
  

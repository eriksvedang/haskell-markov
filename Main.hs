module Main where

import System.Random
import Control.Monad.State
import System.IO
import System.Environment

type Freq = Int
type Weights = [(Char, Freq)]
type Mapping = [(Char, Weights)]

addToAL :: Eq key => [(key, elt)] -> key -> elt -> [(key, elt)]
addToAL l key value = (key, value) : delFromAL l key

delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = filter (\a -> (fst a) /= key) l

mappings1 :: Mapping
mappings1 = [('a', [('a', 10)
                   ,('b', 7)])
            ,
             ('b', [('a', 3)
                   ,('c', 20)])
            ,
             ('c', [('c', 1)])]

(Just freq1) = lookup 'a' mappings1

-- | Sums up the frequency count of a list of weights.
-- | i.e. totalFreq [('a',10),('b',7)] => 17
totalFreq :: [(t, Int)] -> Int
totalFreq weights = foldl (\total (_, x) -> total + x) 0 weights

-- | Given a list of frequency weigths, find the weight at the position between 0 and the total frequency count.
-- | i.e. findSlice [('a',10),('b',7)] 12 => Just 'b'
findSlice weights n = 
  let finder (totalIndex, result) weight = case result of
        Just _ -> (totalIndex, result)
        Nothing -> let (c, freq) = weight
                       totalIndex' = totalIndex + freq
                       result' = if totalIndex' >= n then (Just c) else Nothing
                   in (totalIndex', result')
  in snd $ foldl finder (0 :: Int, Nothing) weights

nextChar :: Mapping -> Char -> State StdGen Char
nextChar mapping currentChar =
  case (lookup currentChar mapping) of
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

analyze :: [Char] -> Mapping
analyze input =
  let folder mappings (c, next) =
        case (lookup c mappings) of
          (Just weights) -> addToAL mappings c (increaseFreq weights next)
          Nothing -> (c, [(next, 1)]) : mappings
  in foldl folder [] (zip input (tail input))

run randomGen mapping start = evalState (chain mapping start 1200) randomGen

main :: IO ()
main = do
  g <- newStdGen
  (file : _) <- getArgs
  text <- readFile file
  --putStrLn (show $ analyze text)
  let mapping = analyze text
      result = run g mapping (head text)
  putStrLn (show result)
  

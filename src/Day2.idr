module Day2

import Control.Monad.Reader
import Control.Monad.Identity
import Data.String
import Data.List1

partial
parse : String -> List (Int, Int)
parse input = forget $ 
  (\i => case forget i of
    [sa, sb] => case (parseInteger sa, parseInteger sb) of
      (Just a, Just b) => (a, b)) 
  <$> split (== '-') 
  <$> split (== ',') input

invalidWithLength : Int -> Int -> Bool
invalidWithLength l i = let
  s := show i
  len := strLength s
  first := strSubstr 0 l s
  in   l /= 0 
    && len `div` l > 1 
    && len `mod` l == 0
    && foldr (\i, acc => (strSubstr i l s == first) && acc) 
             True 
             [0,l..len-l]

invalidP1 : Int -> Bool
invalidP1 i = if (strLength (show i) `mod` 2 == 0)
  then invalidWithLength (strLength (show i) `div` 2) i
  else False

invalidP2 : Int -> Bool
invalidP2 i =
  foldr 
    (\n, acc => invalidWithLength n i || acc)
    False
    [1..(strLength . show) i `div` 2]

countInvalids : List (Int, Int) -> Reader (Int -> Bool) Int
countInvalids xs = go xs 0
  where
    invalids : Int -> Int -> Int -> Reader (Int -> Bool) Int
    invalids x y acc = do
      invalid <- ask
      if x <= y
        then invalids (x+1) y (acc + if invalid x then x else 0)
        else pure acc
    go : List (Int, Int) -> Int -> Reader (Int -> Bool) Int
    go ((a, b)::xs) acc = go xs (acc + !(invalids a b 0))
    go [] acc = pure acc

export
partial
day2 : String -> String
day2 input = let
  parsedInput := parse input
  resultP1 := runReader invalidP1 $ countInvalids parsedInput
  resultP2 := runReader invalidP2 $ countInvalids parsedInput
  in show (resultP1, resultP2)

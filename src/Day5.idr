module Day5

import Data.List1
import Data.List
import Data.String

partial
unwrap : Maybe Int -> Int
unwrap (Just v) = v

inRange : List (Int, Int) -> Int -> Bool
inRange [] _ = False
inRange ((low, high)::ranges) i = if i >= low && i <= high 
  then True 
  else inRange ranges i

solve : List (Int, Int) -> List Int -> Int
solve ranges = cast . count (==True). map (inRange ranges)

overlap : (Int, Int) -> (Int, Int) -> Bool
overlap (l1,h1) (l2,h2) = h2 >= l1 && h1 >= l2

||| Adds a new range in to the list of ranges, combined with the existing ranges where appropriate
addRange : List (Int, Int) -> (Int, Int) -> List (Int, Int)
addRange [] t = [t]
addRange ((l,h)::[]) (low,high) = if high < l then [(low,high), (l,h)] else
  if overlap (l,h) (low,high) 
  then [(min l low, max h high)]
  else [(l,h), (low,high)]
addRange (t1@(l1,h1)::t2@(l2,h2)::ts) t@(low,high) = if high < l1 then t::t1::t2::ts else
  case (overlap t1 t, overlap t2 t) of
    (True, True) => addRange ((min low l1, max high h2)::ts) t
    (True, False) => (min low l1, max high h1)::t2::ts
    (False, True) => t1 :: addRange ((min low l2, max high h2)::ts) t
    (False, False) => t1 :: addRange (t2::ts) t

solveP2 : List (Int, Int) -> Int
solveP2 = foldl (\n, (a,b) => n + b - a + 1) 0 . foldl addRange []

partial
parse : String -> (List (Int, Int), List Int)
parse = (\(l1::l2::_) => 
    (map ((\(a::b::_) => (a, b)) . map (unwrap . parseInteger) . forget . split (=='-')) l1
    , map (unwrap . parseInteger) l2))
  . forget 
  . splitOn "" 
  . lines

export
partial
day5 : String -> String
day5 = show . (\t => (uncurry solve t, solveP2 (fst t))) . parse

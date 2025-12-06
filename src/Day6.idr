module Day6

import Data.String
import Data.List
import Util

partial
parseOp : String -> ((Int -> Int -> Int), Int)
parseOp s = case s of
  "+" => ((+), 0)
  "*" => ((*), 1)

Funny = List String -> (List (List Int), List (Int -> Int -> Int, Int))

partial
p1 : Funny
p1 l = let
  left := transpose . map (map (unwrap . parseInteger {a=Int}) . words) . unwrap . init' $ l
  right := map parseOp . words . unwrap . last' $ l
  in (left, right)

transposeStrings2 : List String -> List String
transposeStrings2 = map pack . transpose . map unpack

||| transforms [[1], [2], [], [3], [4]] into [[1, 2], [3, 4]]
collect : List (List a) -> List (List a)
collect [] = []
collect [v] = [v]
collect (x::y::xs) = case y of
  [] => x :: collect xs
  ys => collect ((x++ys)::xs)

partial
p2 : Funny
p2 l = let
  transposeStrings : List String -> List String
  transposeStrings = map pack . transpose . map unpack
  left := collect . map (map (unwrap . parseInteger {a=Int}) . words) . transposeStrings . unwrap . init' $ l
  right := map parseOp . words . unwrap . last' $ l
  in (left, right)

partial
solve : Funny -> String -> Int
solve f =
  foldl (\acc,(l,(op,start)) => acc + foldl op start l) 0
  . uncurry zip
  . f
  . lines

export
partial
day6 : String -> String
day6 s = show (solve p1 s, solve p2 s)

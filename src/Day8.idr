module Day8

import Data.String
import Data.List
import Data.List1
import Util
import Data.SortedSet
import Data.SortedMap

record Point where
  constructor MkPoint
  x : Int
  y : Int
  z : Int

toTuple : Point -> (Int, Int, Int)
toTuple (MkPoint x y z) = (x, y, z)

implementation Show Point where
  show (MkPoint x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

implementation Eq Point where
  (MkPoint a b c) == (MkPoint d e f) = a == d && b == e && c == f

implementation Ord Point where
  compare = compare `on` toTuple

square : Int -> Double
square i = pow (cast i) 2

combinations2 : List a -> List (a,a)
combinations2 [] = []
combinations2 (x::xs) = 
  ([(x,)] <*> xs) ++ combinations2 xs

distance : Point -> Point -> Double
distance (MkPoint xA yA zA) (MkPoint xB yB zB) = 
  sqrt $ square (xA - xB) + square (yA - yB) + square (zA - zB)

distances : List Point -> List (Double, Point, Point)
distances = map (\(a,b) => (distance a b, a, b)) . combinations2

partial
parsePoint : String -> Point
parsePoint = (\[x,y,z] => MkPoint x y z) . map (unwrap . parseInteger {a=Int}) . forget . split (==',')

EdgeList p = List (p, p)

||| Takes disconnected subgraphs from the input and returns sets of nodes which are connected
connectedNodes : Ord a => EdgeList a -> List (SortedSet a)
connectedNodes [] = []
connectedNodes ((l, r)::restEdges) = 
  go (insert l (singleton r)) [] restEdges
  where
    go : SortedSet a -> EdgeList a -> EdgeList a -> List (SortedSet a)
    go accSet remainder [] = accSet :: connectedNodes remainder
    go accSet remainder ((l,r)::edges) = 
      if contains l accSet || contains r accSet
      then go (insert l (insert r accSet)) [] (remainder++edges)
      else go accSet ((l,r)::remainder) edges

noDisconnectedNodes : Ord a => EdgeList a -> Bool
noDisconnectedNodes [] = True
noDisconnectedNodes ((l, r)::restEdges) = 
  go (insert l (singleton r)) [] restEdges
  where
    go : SortedSet a -> EdgeList a -> EdgeList a -> Bool
    go accSet [] [] = True
    go accSet (remainder::_) [] = False
    go accSet remainder ((l,r)::edges) = 
      if contains l accSet || contains r accSet
      then go (insert l (insert r accSet)) [] (remainder++edges)
      else go accSet ((l,r)::remainder) edges

isAllConnected : SortedSet Point -> SortedSet Point -> EdgeList Point -> Bool
isAllConnected allNodes nodes edges = 
  allNodes == nodes && noDisconnectedNodes edges
  
||| TODO this will stop early because isAllConnected doesn't quite take a list of all points to compare!!!
partial
lastConnection : EdgeList Point -> (Point, Point)
lastConnection points = go [] empty points
  where
    allNodes : SortedSet Point
    allNodes = foldl (\acc, (l,r) => insert l $ insert r acc ) empty points

    go : EdgeList Point -> SortedSet Point -> EdgeList Point -> (Point, Point)
    go acc nodes ((l, r)::points) = 
      if isAllConnected allNodes (insert l $ insert r nodes) ((l,r)::acc)
      then (l, r)
      else go ((l,r)::acc) (insert l $ insert r nodes) points

export
partial
day8 : String -> String
day8 s = show (p1, p2)
  where
    parse : String -> List Point
    parse = map parsePoint . lines

    sortedEdges : EdgeList Point
    sortedEdges = (map snd . sortBy (compare `on` fst) . distances . parse) s

    biggest : Int -> List (List a) -> List (List a)
    biggest n = take 3 . sortBy (flip compare `on` length)

    p1 : Nat
    p1 = (product . map length . biggest 3 . map Prelude.toList . connectedNodes . take 1000) sortedEdges

    p2 : Int
    p2 = ((\(a,b) => x a * x b) . lastConnection) sortedEdges

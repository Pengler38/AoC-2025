module Day4

import Data.List
import Data.String
import Data.SortedSet

total
safeIndex : List (List a) -> (Int, Int) -> Maybe a
safeIndex [] (x,y) = Nothing
safeIndex ([]::ass) (x,y) = Nothing
safeIndex ((a::as)::ass) (0,0) = Just a
safeIndex ((a::as)::ass) (x,0) = safeIndex (as::ass) (x-1,0)
safeIndex ((a::as)::ass) (x,y) = if x < 0 || y < 0 then Nothing
  else safeIndex ass (x,y-1)

||| indexes a 2d list along with a set of 'removed' indices which should return Nothing
total
indexRemoved : SortedSet (Int, Int) -> List (List a) -> (Int, Int) -> Maybe a
indexRemoved s arr t = if contains t s then Nothing else safeIndex arr t

total
accessible : List (List Char) -> SortedSet (Int, Int) -> (Int, Int) -> Bool
accessible arr set (x,y) = 
  let 
    positions : List (Int, Int)
    positions = do
      a <- [-1, 0, 1] 
      b <- [-1, 0, 1]
      pure $ (x+a, y+b)
  in ((<5) . count (==Just '@') . map (indexRemoved set arr)) positions

total
solve : List (List Char) -> Int
solve arr =
  let
    go : List (List Char) -> (Int, Int) -> Int -> Int
    go [] (x,y) acc = acc
    go ([]::css) (x,y) acc = go css (0,y+1) acc
    go ((c::cs)::css) (x,y) acc = 
      let acc' := if c == '@' && accessible arr empty (x,y) then acc+1 else acc
      in go (cs::css) (x+1,y) acc'
  in go arr (0,0) 0

fLength : Foldable f => f a -> Int
fLength = foldl (\acc,_ => acc+1) 0

removeNext : List (List Char) -> SortedSet (Int, Int) -> SortedSet (Int, Int)
removeNext arr = go arr (0, 0)
  where 
    go : List (List Char) -> (Int, Int) -> SortedSet (Int, Int) -> SortedSet (Int, Int)
    go [] (x,y) set = set
    go ([]::css) (x,y) set = go css (0,y+1) set
    go ((c::cs)::css) (x,y) set = let
        set' = if c == '@' && accessible arr set (x,y) then insert (x,y) set else set
      in go (cs::css) (x+1,y) set'

solveP2 : List (List Char) -> Int
solveP2 arr =
  let
    go : SortedSet (Int, Int) -> SortedSet (Int, Int)
    go currentSet =
      let nextSet = removeNext arr currentSet
      in if currentSet == nextSet then currentSet else go nextSet
  in fLength $ go empty

export
day4 : String -> String
day4 = show . (\x => (solve x, solveP2 x)) . map unpack . lines

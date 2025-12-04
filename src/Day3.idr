module Day3

import Data.Fin
import Data.String

partial
unwrap : Maybe a -> a
unwrap (Just v) = v

||| Takes all (i, True), ignores all (i, False)
yoink : List (Int, Bool) -> List Int
yoink [] = []
yoink ((x, b)::xs) = if b then x::yoink xs else yoink xs

||| Sets n tuples to True starting with the max and preferring to True tuples to the right of the max
||| Returns the number of excess Trues it couldn't set along with the resulting list
partial
trueTheMax : Nat -> List (Int, Bool) -> (Nat, (List (Int, Bool)))
trueTheMax Z xs = (Z, xs)
trueTheMax (S k) [] = ((S k), [])
trueTheMax (S k) xs =
  let
    maximum : Int
    maximum = foldl (\acc, (i, b) => if b then acc else max acc i) 0 xs
    idx := finToNat $ unwrap $ List.findIndex (==(maximum,False)) xs
    (left, right') := splitAt idx xs
    right := unwrap $ tail' right'
  in case trueTheMax k right of
    (0, tRight) => (0, left ++ (maximum,True) :: tRight)
    (extra, tRight) => case trueTheMax extra left of
      (extra', tLeft) => (extra', tLeft ++ (maximum,True) :: tRight)

partial
joltage : Nat -> List Int -> Int
joltage nums = 
  unwrap . parseInteger 
  . foldr (\i, acc => show i ++ acc) ""
  . yoink . snd . trueTheMax nums . map (\i => (i, False))

run : (List Int -> Int) -> List (List Int) -> Int
run f = sum . map f

partial
export
day3 : String -> String
day3 = show 
  . (\ls => (run (joltage 2) ls, run (joltage 12) ls))
  . map (map (unwrap . parseInteger . singleton) . unpack) 
  . lines

module Day1

import Data.String

%default total

partial
solve : List String -> Int -> (Int, Int) -> (Int, Int)
solve [] deg (acc, acc2) = (acc, acc2)
solve (s::ss) deg (acc, acc2) = case unpack s of
  (d::digit) => case parseInteger $ pack digit of
    Just i => 
      let 
        dir := case d of
          'L' => -1
          'R' => 1
        deg' := (deg + dir * i) `mod` 100 
        acc' := if deg' == 0 then acc + 1 else acc
        acc2' := acc2 
          + (i `div` 100)
          -- YEAH THIS IS A TERRIBLE WAY TO DO THIS BUT I'M REALLY RUSHING HERE
          + (if (deg /= 0 || dir == 1) && ((deg + dir * (i `mod` 100) >= 100) || (deg + dir * (i `mod` 100) <= 0)) then 1 else 0) 
      in
        solve ss deg' (acc', acc2')

export
partial
day1 : String -> String
day1 s = show $ solve (lines s) 50 (0, 0)


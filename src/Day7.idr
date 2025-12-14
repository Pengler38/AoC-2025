module Day7

import Data.String
import Data.List
import Util

iter : Int -> Char -> List Int -> List Char -> (Int, List Int)
iter prevN prevC (n::ns) (c::cs) =
  let
    left = if prevC == '^' then prevN else 0 
    right : Maybe Int
    right = do
      nextC <- head' cs
      nextN <- head' ns
      Just $ if nextC == '^' then nextN else 0
    center = case c of
      'S' => 1
      '^' => 0
      _   => n
    (nextSplits, next) := iter n c ns cs
    thisSplits = if c == '^' && n > 0 then 1 else 0
  in (nextSplits + thisSplits, left + center + maybe 0 id right :: next)
iter prevN prevC _ _ = (0, [])

partial
export
day7 : String -> String
day7 str = 
  let 
    ls := map unpack $ lines str
    init := map (const 0) (unwrap $ head' ls)
  in
  show 
  $ mapSnd sum 
  $ foldl 
    (\(i, l), cs => mapFst (i+) $ iter 0 '.' l cs) 
    (0, init) 
    ls

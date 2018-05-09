module Glushkov where

--If we se:e epsilon in regex pattern it will be Letter (_, 'eps'), so Epsilon cons has a role of more like Nil
data RegT
  = Epsilon
  | Letter (Int, Char)
  | Star RegT
  | Concat (RegT, RegT)
  | Or (RegT, RegT)
  deriving (Show, Eq)

initS :: RegT
initS = Letter (-1, '#')

empty :: RegT -> Bool
empty t =
  case t of
    Concat (l, r) -> empty l && empty r
    Or (l, r) -> empty l || empty r
    Letter _ -> False
    _ -> True

--all the states that can be reached from the initS statef
firstS :: RegT -> [RegT]
firstS t =
  case t of
    Or (r1, r2) -> firstS r1 ++ firstS r2
    Concat (r1, r2) ->
      if empty r1
        then firstS r1 ++ firstS r2
        else firstS r1
    x@(Letter _) -> [x]
    Star r1 -> firstS r1
    Epsilon -> []

--the next possible states for a particular leaf
nextS :: RegT -> RegT -> [RegT]
nextS t leaf = nextS t leaf []
  where
    nextS t leaf acc =
      case t of
        Concat (r1, r2) -> nextS r1 leaf r1' ++ nextS r2 leaf acc
          where r1' =
                  if empty r2
                    then firstS r2 ++ acc
                    else firstS r2
        Or (r1, r2) -> nextS r1 leaf acc ++ nextS r2 leaf acc
        Star r1 -> nextS r1 leaf (firstS r1 ++ acc)
        x@(Letter _) ->
          if leaf == x
            then acc
            else []
        Epsilon -> []

--all the states which are accept states without the initS state
lastS :: RegT -> [RegT]
lastS t =
  case t of
    Concat (r1, r2) ->
      if empty r2
        then lastS r1 ++ lastS r2
        else lastS r2
    Or (r1, r2) -> lastS r1 ++ lastS r2
    Star r1 -> lastS r1
    x@(Letter _) -> [x]
    Epsilon -> []

--all the states which are accept states, taking in account if initS state is empty
acceptS :: RegT -> [RegT]
acceptS t =
  if not (empty t)
    then lastS t
    else initS : lastS t


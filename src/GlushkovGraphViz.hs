module GlushkovGraphViz where

import Data.List (find, nub)
import Glushkov

type StateOfStates = [RegT]

--TODO: maybe check first if the word is valid and only then return a path ?
--because otherwise only the successfully passed states will be yielded
--returns the path travelled by the word in the automaton as a list of states
automatonPath :: RegT -> String -> [StateOfStates]
automatonPath t word = [initS] : path t word [initS]
  where
    filterByLetter l = filter (\(Letter (_, l')) -> l' == l)
    path t [] _ = []
    path t (l:ls) (initS:[]) =
      let next = filterByLetter l (firstS t)
      in next : path t ls next
    path t (l:ls) next =
      let next' = filterByLetter l $ concatMap (nextS t) next
      in next' : (path t ls next')

--returns itself if the StateOfStates is qualified as an accept state
--TODO: returning a Maybe StateOfStates only made code uglier
maybeAcceptS :: RegT -> StateOfStates -> StateOfStates
maybeAcceptS t s =
  if or (map (\trueAcceptS -> elem trueAcceptS s) $ acceptS t)
    then s
    else []

--identifier of a state/(letter leaf)
state2Str :: StateOfStates -> String
state2Str [] = ""
state2Str s =
  "\"{" ++ tail ((concatMap (\(Letter (i, _)) -> ',' : show i) s) ++ "}\"")

trans2Str :: (StateOfStates, StateOfStates) -> String
trans2Str (s, s'@(Letter (_, l):_)) =
  state2Str s ++ "->" ++ state2Str s' ++ " [label=\"" ++ show l ++ "\"]"
trans2Str _ = ""

--given a word it creates the repr. for a graphViz automaton
graphVizAutomaton :: RegT -> String -> String
graphVizAutomaton t word =
  let path = automatonPath t word
      trans = nub $ zip path (tail path) --extract unique "edges/transitions"
  in "\ndigraph nfa {\n\
      \rankdir=LR; node [shape=none,width=0,height=0,margin=0]; start [label=\"\"];\n\
      \node [shape=doublecircle];\n" ++
     concatMap ((++ "\n") . state2Str) (nub (maybeAcceptS t <$> path)) ++
     "node [shape=circle];\n" ++
     concatMap ((++ "\n") . trans2Str) trans ++
     "start->" ++ state2Str [initS] ++ "\n}"

--(a|b)*a(a|b)
regEx :: RegT
regEx =
  Concat
    ( Star (Or (Letter (0, 'a'), Letter (1, 'b')))
    , Concat (Letter (2, 'a'), Or (Letter (3, 'a'), Letter (4, 'b'))))

--(a)*
regEx2 :: RegT
regEx2 = Star (Letter (0, 'a'))

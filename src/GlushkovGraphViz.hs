module GlushkovGraphViz where

import Data.List (nub)
import Glushkov

--identifier of a state/(letter leaf)
state2Str :: StateOfStates -> String
state2Str [] = ""
state2Str s =
  "\"{" ++ tail (concatMap (\(Letter (i, _)) -> ',' : show i) s) ++ "}\""

trans2Str :: (StateOfStates, StateOfStates) -> String
trans2Str (s, s'@(Letter (_, l):_)) =
  state2Str s ++ "->" ++ state2Str s' ++ " [label=\"" ++ show l ++ "\"]"
trans2Str _ = ""

--returns the path travelled by the word in the automaton as a list of states
automatonPath :: RegT -> String -> [StateOfStates]
automatonPath t word = [initS] : path t word [initS]
  where
    filterByLetter l = filter (\(Letter (_, l')) -> l' == l)
    path t [] _ = []
    path t (l:ls) [initS] =
      let next = filterByLetter l (firstS t)
      in next : path t ls next
    path t (l:ls) next =
      let next' = filterByLetter l $ concatMap (nextS t) next
      in next' : path t ls next'

--given a word it creates the repr. for a graphViz automaton
graphVizAutomaton :: RegT -> String -> String
graphVizAutomaton t word =
  let path = automatonPath t word
      trans = nub $ zip path (tail path) --extract unique transitions
  in "\ndigraph nfa {\n\
      \rankdir=LR; node [shape=none,width=0,height=0,margin=0]; start [label=\"\"];\n\
      \node [shape=doublecircle];\n" ++
     concatMap ((++ "\n") . state2Str) (nub (maybeAcceptS t <$> path)) ++
     "node [shape=circle];\n" ++
     concatMap ((++ "\n") . trans2Str) trans ++
     "start->" ++ state2Str [initS] ++ "\n}"


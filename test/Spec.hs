import Glushkov
import GlushkovGraphViz

--(a|b)*a(a|b)
regEx :: RegT
regEx =
  Concat
    ( Star (Or (Letter (0, 'a'), Letter (1, 'b')))
    , Concat (Letter (2, 'a'), Or (Letter (3, 'a'), Letter (4, 'b'))))

--(a)*
regEx2 :: RegT
regEx2 = Star (Letter (0, 'a'))

main :: IO ()
main = putStrLn "Test suite not yet implemented"

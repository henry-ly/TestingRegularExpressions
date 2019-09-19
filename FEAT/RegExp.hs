module RegExp where

import FEAT
import System.Random( randomRIO )

--------------------------------------------------------------------------------

-- datatype for regular expressions

data E
  = Atom Char
  | Nil
  | Eps
  | E :+: E
  | E :>: E
  | Star E
 deriving ( Eq, Show, Ord )

-- 'space p' creates an enumeration for all strings matching p.
-- the size of the string is the length of the string.

space :: E -> Space String
space (Atom c)  = pay (unit [c])
space Nil       = empty
space Eps       = unit ""
space (p :+: q) = space p +++ space q
space (p :>: q) = unit (++) `app` space p `app` space q
space (Star p)  = w
 where
  w = unit "" +++ pay (unit (++) `app` unpay (space p) `app` w)

  -- we use 'unpay (space p)' because we want to enumerate all strings
  -- matching p, but not the empty string.
  
  -- we use 'pay (...)' to compensate for the size loss of unpay.

--------------------------------------------------------------------------------
-- example

letter :: E
letter = foldr1 (:+:) [ Atom c | c <- ['a'..'z'] ]

r :: E
r = Atom '<' :>: Star (letter :>: Atom ',') :>: letter :>: Atom '>'

sp :: Space String
sp = space r

main :: IO ()
main =
  do putStrLn "-- regular expression:"
     print r
     putStrLn ""

     putStrLn "-- enumeration space:"
     putStrLn (take 1000 (showSpace sp) ++ "...")
     putStrLn ""

     sequence_
       [ do putStrLn ( "-- pick a random string of size "
                    ++ show sz
                    ++ ", from "
                    ++ show n
                    ++ " possible strings:"
                     )
            if n == 0 then
              do putStrLn "(there are no such strings)"
             else
              do i <- randomRIO (0,n-1)
                 print (h i)   
            putStrLn ""
       | (sz, (n,h)) <- [0..20] `zip` sp
       ]

-- enumerates matching strings up to size n given a regex r
validString s r = (concat) [enum(n, h)|(sz, (n,h)) <- [0..s] `zip` space r ]

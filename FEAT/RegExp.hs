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

r :: E
r = Atom 'a' :>: Star ((Atom 'b' :+: Atom 'c') :>: Atom 'x') :>: Atom 'd'

sp :: Space String
sp = space r

main :: IO ()
main =
  do putStrLn "-- regular expression:"
     print r
     putStrLn ""

     putStrLn "-- enumeration space:"
     putStrLn (take 500 (showSpace sp) ++ "...")
     putStrLn ""

     sequence_
       [ do putStrLn ("-- pick a random string of size " ++ show sz ++ ":")
            if n == 0 then
              do putStrLn "(there are no such strings)"
             else
              do i <- randomRIO (0,n-1)
                 print (h i)   
            putStrLn ""
       | (sz, (n,h)) <- [0..20] `zip` sp
       ]


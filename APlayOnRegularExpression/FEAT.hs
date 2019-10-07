module FEAT where

import Data.List( intersperse )
import System.IO

infixl 1 `app`

--------------------------------------------------------------------------------

-- a sequence Seq is like a (finite) list. It is a pair of the size of the
-- list, and the function we can use to index in it. We can represent HUGE
-- lists in this way. The operator that can construct huge such lists
-- (in constant time) is product (*!*).

type Seq a = (Integer, Integer -> a)

nil :: Seq a
nil = (0, error "nothing here!")

single :: a -> Seq a
single x = (1, \0 -> x)

(+!+) :: Seq a -> Seq a -> Seq a
(n1,h1) +!+ (n2,h2) = (n1+n2, \i -> if i < n1 then h1 i else h2 (i-n1))

(*!*) :: Seq a -> Seq b -> Seq (a,b)
(n1,h1) *!* (n2,h2) = (n1*n2, \i -> (h1 (i `mod` n1), h2 (i `div` n1)))

showSeq :: Show a => Seq a -> String
showSeq (n,h) = show [ h i | i <- [0..n-1] ]

enum :: Seq a -> [a]
enum (n,f) = [ f i | i <- [0..n-1] ]

--------------------------------------------------------------------------------

-- a Space is a collection of sequences, one such sequence for each size.
-- We can use the pay function to control size.

type Space a = [Seq a]

empty :: Space a
empty = []

unit :: a -> Space a
unit x = [single x]

(+++) :: Space a -> Space a -> Space a
xs     +++ []     = xs
[]     +++ ys     = ys
(x:xs) +++ (y:ys) = (x +!+ y) : (xs +++ ys)

(***) :: Space a -> Space b -> Space (a,b)
[]     *** w = []
(x:xs) *** w = map (x *!*) w +++ pay (xs *** w)
  
-- use pay to increase the size
-- use unpay to decrease the size (and throw away everything of size 0)
pay, unpay :: Space a -> Space a
pay   xs     = nil : xs

unpay (x:xs) = xs
unpay []     = []

join :: Space (Space a) -> Space a
join []     = []
join (x:xs) = foldr (+++) (pay (join xs)) (enum x)

bind :: Space a -> (a -> Space b) -> Space b
m `bind` k = join (mapSpace k m)

--------------------------------------------------------------------------------

mapSpace :: (a->b) -> Space a -> Space b
mapSpace f v = [ (n, f . h) | (n,h) <- v ]

app :: Space (a->b) -> Space a -> Space b
f `app` x = mapSpace (uncurry ($)) (f *** x)

--------------------------------------------------------------------------------

-- some examples

bool :: Space Bool
bool = pay (unit False +++ unit True)

list :: Space a -> Space [a]
list x = list_x
 where
  list_x = pay ( unit []
             +++ (unit (:) `app` x `app` list_x)
               )

-- try this:
-- take 300 (showSpace (list bool))

--------------------------------------------------------------------------------

showSpace :: Show a => Space a -> String
showSpace v = "{" ++ concat (intersperse "," [ show s ++ ":" ++ showSeq p | (p@(n,_),s) <- v `zip` [0..], n > 0 ]) ++ "}"

--------------------------------------------------------------------------------


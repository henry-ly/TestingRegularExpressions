module Helper where
import Control.Monad( liftM, liftM2 )
import Test.QuickCheck


--------------------------------------------------------------------------------
-- POSIX regular expressions Work in progress
data R a 
  = Nil
  | Eps
  | Atom a
  | R a :+: R a
  | R a :>: R a
 deriving (Eq, Ord, Show)

--newtype PlainChar = PlainChar Char
--instance Show PlainChar where
--  show (PlainChar c) = c

showExpr :: R Char -> String
showExpr (Atom a) = [a]
showExpr (a :+: b) = showExpr a ++ "|" ++ showExpr b
showExpr (a :>: b) = showExpr a ++ showExpr b
--showExpr (a :>: b) = showFactor a ++ showFactor b
--showExpr (Star a) = showFactor a : '*'




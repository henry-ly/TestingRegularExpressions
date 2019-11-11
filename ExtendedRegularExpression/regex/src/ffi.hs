{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FFI where

import Test.QuickCheck
import Control.Monad(liftM, liftM2)

-- for unix grep
import System.Process 
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe
import Test.QuickCheck.Monadic

import Foreign.C.Types 
import Foreign.C.String
import Foreign.Ptr 
import Foreign.Marshal.Array 
import System.IO.Unsafe 
import FEAT

foreign import ccall "regex"
     c_regex :: CString -> CString -> CInt

data Regex a =
           Eps
         | Lit a
         | Wildcard
         | Cat (Regex a) (Regex a)
         | Clo (Regex a)
         | Alt (Regex a) (Regex a)
         | Plus (Regex a)
         deriving (Eq, Show)


matcher :: String -> String -> IO Bool
matcher a b = do  
              newRegex <- newCString a
              newString <- newCString b
              return $ toBool(c_regex newRegex newString)

space :: Regex Char -> Space String
space (Lit c)  = pay (unit [c])
space Eps      = unit ""
space (Wildcard) = foldr (+++) empty (map (space.Lit) (['1'..'9']++['a'..'z']))
space (p `Alt` q) = space p +++ space q
space (p `Cat` q) = unit (++) `app` space p `app` space q
space (Plus p)  = space (p `Cat` Clo p)
space (Clo p)  = w
 where
  w = unit "" +++ pay (unit (++) `app` unpay (space p) `app` w)


toBool :: CInt -> Bool
toBool 1 = True
toBool _ = False

alphabet :: Gen Char
alphabet = elements ['a' .. 'c']

instance Arbitrary (Regex Char) where
     arbitrary = sized arbitraryExpression
     shrink (Lit a) = [Eps] ++ [Lit a' | a' <- shrink a]
     shrink (a `Cat` b) = [a, b]
                       ++ [a' `Cat` b | a' <- shrink a]
                       ++ [a `Cat` b' | b' <- shrink b ] 
     shrink (Clo a) = [a, Eps]  
                   ++ [ Clo a' | a' <- shrink a ] 
     shrink _ = []
arbitraryExpression 0 = frequency[(1, liftM Lit alphabet)
                                 ,(1, return Eps)
                                 ,(1, return Wildcard)
                                 ]
arbitraryExpression n = frequency[(1, liftM2 Cat subexpr subexpr)
                                 ,(1, liftM Clo subexpr)
                                 ,(1, liftM2 Alt subexpr subexpr)
                                 ]
                   where subexpr = arbitraryExpression (n `div` 2)

showExpr :: Regex Char -> String
showExpr Eps = ""
showExpr (Lit a) = [a]
showExpr (Clo a) = showExpr a ++ "*"
showExpr (Plus a) = showExpr a ++ "+"
showExpr (Cat a b) = showExpr a ++ showExpr b
showExpr (Alt a b) = "(" ++ showExpr a ++ "|" ++ showExpr b ++ ")"
showExpr _ = "."

unixGrep :: String -> String -> IO Bool
unixGrep s r = do
            exitCode <- system $ "echo \"" ++ s ++ "\" | grep -E -q \"" ++ r ++"\""
            case exitCode of
                ExitSuccess ->  return True
                _           ->  return False
genInputStrings :: Int -> Regex Char -> Gen String
genInputStrings s r = oneof[genMatching s r, genNotMatching s]

genNotMatching :: Int -> Gen String
genNotMatching n = do
                   r <- choose (0, n)
                   vectorOf r alphabet


genMatching :: Int -> Regex Char -> Gen String
genMatching s r = if not (null (space r)) then
                      oneof[do
                               a <- choose(0, n-1)
                               if n == 0 then
                                   return ""
                               else
                                   return(h a)
                           |(sz, (n,h)) <- [0..s] `zip` space r]
                  else
                      return ""

prop_Grep :: Regex Char -> Property
prop_Grep r = monadicIO $ do
                            ts <- run $ generate (genInputStrings 10 r)
                            grep <- run $ unixGrep ts (showExpr r)
                            match <- run $ matcher ts (showExpr r) 
                            monitor(counterexample ts)
                            assert (grep == match)



testMatcher ::  Regex Char -> Regex Char -> Property
testMatcher r1 r2 = monadicIO  $ do
                                 testString <- run(generate(genInputStrings 10 r1))
                                 match <- run $ matcher testString (showExpr r1)
                                 match2 <- run $ matcher testString (showExpr r2)
                                 monitor(counterexample testString)
                                 assert $ match == match2

--Alternation Laws
prop_AltAssoc ::  Regex Char -> Regex Char -> Regex Char -> Property
prop_AltAssoc a b c = testMatcher (a `Alt` (b `Alt` c)) ((a `Alt` b) `Alt` c)

prop_AltCom :: Regex Char -> Regex Char -> Property
prop_AltCom a b = testMatcher (a `Alt` b) (b `Alt` a)

prop_AltIdem :: Regex Char -> Property
prop_AltIdem a = testMatcher (a `Alt` a) a

--Concatenation Laws
prop_CatAssoc :: Regex Char -> Regex Char -> Regex Char -> String -> Property
prop_CatAssoc a b c s = testMatcher (a `Cat` (b `Cat` c)) ((a `Cat` b) `Cat` c)

prop_DistLeft :: Regex Char -> Regex Char -> Regex Char -> Property
prop_DistLeft a b c = testMatcher (a `Cat` (b `Alt` c)) ((a `Cat` b) `Alt` (a `Cat` c))

prop_DistRight :: Regex Char -> Regex Char -> Regex Char -> Property
prop_DistRight a b c = testMatcher ((a `Alt` b) `Cat` c) ((a `Cat` c) `Alt` (b `Cat` c))

prop_Clo2 :: Regex Char -> Property
prop_Clo2 a = testMatcher (Clo(Clo a)) (Clo a)

deepCheck 1 p = quickCheckWith (stdArgs {maxSuccess = 100, maxSize = 8}) p
deepCheck n p = do
                quickCheckWith (stdArgs {maxSuccess = 100, maxSize = 8}) p
                deepCheck (n-1) p


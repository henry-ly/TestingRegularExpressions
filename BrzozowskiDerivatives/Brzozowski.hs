{-# LANGUAGE FlexibleInstances #-}

import Control.Monad.IO.Class
import Control.Monad(liftM, liftM2)
import Data.Char
-- regular expression matching using Brzozowski's algorithm
import Test.QuickCheck

-- for unix grep
import System.Process 
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe
import Test.QuickCheck.Monadic
--
import FEAT

data Regex a = Nil --epsilon; zero-width match pattern that matches the empty string.
	     | End --zero-width non-match
	     | Lit a --a literal symbol in the input alphabet
	     | Cat (Regex a) (Regex a) --cat-node
	     | Alt (Regex a) (Regex a) --or-node
	     | Clo (Regex a) --star-node; Kleene closure
	     deriving (Eq, Show)

space :: Regex Char -> Space String
space (Lit c)  = pay (unit [c])
space End       = empty
space Nil       = unit ""
space (p `Alt` q) = space p +++ space q
space (p `Cat` q) = unit (++) `app` space p `app` space q
space (Clo p)  = w
 where
  w = unit "" +++ pay (unit (++) `app` unpay (space p) `app` w)


regex :: String -> Regex Char
--A simple but effective parser for regular expressions
--Epsilon can be represented by "()" or vacancy for the last alternation branch such as "a|b|".
--The "*" and "|" not preceded by any character or unmatched parentheses will lead to a
--non-exhaustive pattern exception.
--The regular expression is automatically augmented with terminating End, which helps to make
--the otherwise non-important accepting states as important.(?)
--regex cs | (r, "") <- regexCat0 cs = Cat r End
regex cs | (r, "") <- regexCat0 cs = r
    --Unmatched parantheses for lone ')' is caught here.

regexCat0 :: String -> (Regex Char, String)
regexCat0 "" = (Nil, "")
    --This Nil needs to be further checked for its validity in the outside of regexCat0
regexCat0 cs @(')':_) = (Nil, cs)
regexCat0 ('(':cs) | (r, cs) <- regexPar0 cs = regexCat1 r cs
regexCat0 (c:cs) | c /= '*', c /= '|' = regexCat1 (Lit c) cs

regexCat1 :: Regex Char -> String -> (Regex Char, String)
regexCat1 r "" = (r, "")
regexCat1 r cs @(')':_) = (r, cs)
regexCat1 r ('(':cs) | (s, cs) <- regexPar0 cs = regexCat2 r s cs
regexCat1 r ('|':cs) | (s, cs) <- regexCat0 cs = (Alt r s, cs)
    --We could compare the old cs with the resulting cs from regexCat0 and have the match to fail
    --if equal, but then the last definition of regexCat1 will be matched and '|' will be treated
    --as a normal character.
    --The first pattern of alternation goes to the left branch of the alternation syntax tree and
    --the rest, to the right branch. Going to the left-oriented syntax tree for this case, however,
    --would need to define another function to read up to '|' like regexPar0.
regexCat1 r ('*':cs) = regexCat1 (Clo r) cs
regexCat1 r (c:cs) = regexCat2 r (Lit c) cs

regexCat2 :: Regex Char -> Regex Char -> String -> (Regex Char, String)
regexCat2 r s "" = (Cat r s, "")
regexCat2 r s cs @(')':_) = (Cat r s, cs)
regexCat2 r s ('(':cs) | r <- Cat r s, (s, cs) <- regexPar0 cs = regexCat2 r s cs
regexCat2 r s ('|':cs) | r <- Cat r s, (s, cs) <- regexCat0 cs = (Alt r s, cs)
regexCat2 r s ('*':cs) = regexCat2 r (Clo s) cs
regexCat2 r s (c:cs) = regexCat2 (Cat r s) (Lit c) cs

regexPar0 :: String -> (Regex Char, String)
regexPar0 cs | (r, ')':cs) <- regexCat0 cs = (r, cs)
    --Unmatched parantheses for lone '(' is caught here.
    --We doesn't ban a Nil from regexCat0 here so that "()" may represent Nil.



nullable :: Regex a -> Bool
nullable Nil = True
nullable (Clo _) = True
nullable (Cat r s) = (nullable r) && (nullable s)
nullable (Alt r s) = (nullable r) || (nullable s)
nullable _ = False --on End or Lit



delta :: Regex a -> Regex a
--d(r) = Nil if r can match the empty string; End, otherwise.
delta r = if nullable r then Nil else End

derivative :: Eq a => Regex a -> a -> Regex a
--D[r,x]: Brzozowski's derivative of a regular expression.
--The derivative of a regular expression with respect to a character computes a new regular
--expression that will match the remaining characters that the old expression would match further,
--after it had just matched the character.
derivative (Lit c) x = if x == c then Nil else End
derivative (Cat r s) x =
    --D(rs,x) = D(r,x)s | d(r)D(s,x)
    alt (cat (derivative r x) s) (cat (delta r) (derivative s x))

derivative (Alt r s) x =
    --D(r|s,x) = D(r,x)|D(s,x)
    alt (derivative r x) (derivative s x)

derivative s @(Clo r) x =
    --D(r*,x) = D(r,x)r*
    cat (derivative r x) s

derivative _ _ = End
    --D(Nil,x) = D(End,x) = End

--all the derivatives of "a*" are the same.
--but, "a*a*" explodes.

cat :: Regex a -> Regex a -> Regex a
cat Nil r = r
cat r Nil = r
cat End _ = End
cat _ End = End
cat r s = Cat r s

alt :: Regex a -> Regex a -> Regex a
alt Nil Nil = Nil
--alt Nil (Alt Nil r) = r
--alt r (Alt Nil r) = Alt r s
--alt Nil r = Zer r
alt End r = r
alt r End = r
--alt r s | r == s = r
alt r s = Alt r s


{- bug fix false alarm
alt r Nil = r
alt Nil s = s
alt End s = s
alt r End = r
alt r s = Alt r s
-}

clo :: Regex a -> Regex a
clo Nil = Nil
clo End = Nil
--clo r @(Clo r) = r
clo r = (Clo r)



regexMatch :: Eq a => Regex a -> [a] -> Bool
--regexMatch Nil cs = null cs
--regexMatch Nil [] = True
regexMatch r [] = nullable r
    --If the string to match is empty and the current pattern matches empty, then the match
    --succeeds.
regexMatch r (c:cs) = regexMatch (derivative r c) cs
    --If the string to match is non-empty, the new pattern is the derivative of the current
    --pattern with respect to the first character of the current string, and the new string to
    --match is the remainder of the current string.


-- sample regular expression from Dragon Book
{-main = do
    print $ regexMatch (regex "(a|b)*abb") "abaabb" -- must be True
    print $ regexMatch (regex "(a|b)*abb") "abaab"  -- must be False
-}


alphabet :: Gen Char
alphabet = elements ['a' .. 'c']

instance Arbitrary (Regex Char) where
     arbitrary = sized arbitraryExpression

arbitraryExpression 0 = frequency[(1, liftM Lit alphabet)
                                 ,(1, return End)
                                 ,(1, return Nil)
                                 ]
arbitraryExpression n = frequency[(1, liftM2 Alt subexpr subexpr)
                                 ,(1, liftM2 Cat subexpr subexpr)
                                 ,(1, liftM Clo subexpr)
                                 ]
                   where subexpr = arbitraryExpression (n `div` 2)



unixGrep :: String -> String -> IO Bool
unixGrep s r = do  
            exitCode <- system $ "echo \"" ++ s ++ "\" | grep -E -w -q \"" ++ r ++"\""
            case exitCode of
                ExitSuccess ->  return True
                _           ->  return False

-- convert a regular expression to UNIX regex in a string representation
showExpr :: Regex Char -> String
showExpr Nil = []
showExpr (Lit a) = [a] 
showExpr (Clo a) = "(" ++ showExpr a ++ ")" ++ "*" 
showExpr (Alt a b) = "(" ++ showExpr a ++"|"++ showExpr b ++ ")" 
showExpr (Cat a b) = showExpr a ++ showExpr b
showExpr _ = ".&&."

prop_Grep :: Regex Char -> Property
prop_Grep r1 =  monadicIO  $ do  
                             testString <-  run(generate(genInputStrings 10 r1))
                             booleanGrep <- run(unixGrep testString (showExpr r1))
                             monitor (counterexample testString)
                             assert $ regexMatch r1 testString == booleanGrep
-- Nil is actually epsilon
prop_Nil :: Property
prop_Nil = monadicIO $ do
                       testString <- run(generate(genInputStrings 10 End)) 
                       monitor(counterexample testString)
                       assert $ regexMatch End testString == False

prop_Eps :: Property
prop_Eps = monadicIO $ do
                       testString <- run(generate(genInputStrings 10 Nil))
                       monitor(counterexample testString)
                       assert $ regexMatch Nil testString == null testString

prop_Atom :: Char -> Property
prop_Atom a = monadicIO $ do
                          testString <- run(generate(genInputStrings 10 (Lit a)))
                          monitor(counterexample testString)
                          assert $ regexMatch (Lit a) testString == (testString == [a])

prop_Plus :: Regex Char -> Regex Char -> Property
prop_Plus r1 r2 = monadicIO $ do
                              testString <- run(generate(genInputStrings 10 r1))
                              monitor(counterexample testString)
                              assert $ regexMatch (r1 `Alt` r2) testString == regexMatch r1 testString || regexMatch r2 testString 

prop_Seq :: Regex Char -> Regex Char -> Property
prop_Seq r1 r2 = monadicIO $ do
                             testString <- run(generate(genInputStrings 10 r1))
                             monitor(counterexample testString)
                             assert $ regexMatch (r1 `Cat` r2) testString == 
                               or[regexMatch r1 (take i testString) && regexMatch r2 (drop i testString) | i <- [0 .. length testString]]
-- will be called from properties
testMatcher ::  Regex Char -> Regex Char -> Property
testMatcher r1 r2 = monadicIO  $ do
                                 testString <- run(generate(genInputStrings 10 r1))
                                 monitor(counterexample testString)
                                 assert $ regexMatch r1 testString == regexMatch r2 testString

--Alternation Laws
prop_AltAssoc ::  Regex Char -> Regex Char -> Regex Char -> Property
prop_AltAssoc a b c = testMatcher (a `Alt` (b `Alt` c)) ((a `Alt` b) `Alt` c)
 
prop_AltCom :: Regex Char -> Regex Char -> String -> Bool
prop_AltCom a b s = regexMatch (Alt a b) s == regexMatch (Alt b a) s

prop_AltIdem :: Regex Char -> String -> Bool
prop_AltIdem a s = regexMatch (Alt a a) s == regexMatch a s

--Concatenation Laws
prop_CatAssoc :: Regex Char -> Regex Char -> Regex Char -> String -> Bool
prop_CatAssoc a b c s = regexMatch (Cat a (Cat b c)) s == regexMatch (Cat (Cat a b) c) s


prop_DistLeft :: Regex Char -> Regex Char -> Regex Char -> String -> Bool
prop_DistLeft a b c s = regexMatch (Cat a (Alt b c)) s == regexMatch (Alt (Cat a b) (Cat a c)) s

prop_DistRight :: Regex Char -> Regex Char -> Regex Char -> String -> Bool
prop_DistRight a b c s = regexMatch (Cat (Alt a b) c) s == regexMatch (Alt (Cat a c)(Cat b c)) s

--Closure Properties
prop_Clo :: Regex Char -> String -> Bool
prop_Clo a s = regexMatch (Clo(Clo a)) s == regexMatch (Clo a) s

prop_Clo2 :: Regex Char -> String -> Property
prop_Clo2 a s = testMatcher (Nil `Alt` (a `Cat` Clo a)) (Clo a)

prop_Derived :: Regex Char -> Regex Char -> Regex Char -> Property
prop_Derived a b c = testMatcher (Clo(Clo (b `Alt` (a `Cat`( b `Alt` c)) `Alt` b ))) (Clo (b `Alt` ((a `Cat` b) `Alt` (a `Cat` c)) `Alt` b ))


genInputStrings :: Int -> Regex Char -> Gen String
genInputStrings s r = oneof[genMatching s r, genNotMatching s] 

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
-- pseudo non-matching string will generate strings at random
genNotMatching :: Int -> Gen String
genNotMatching n = do
                    r <- choose (0, n) 
                    vectorOf r alphabet 

-- enumerates matching strings up to size s given a regex r
validStrings s r = concat [enum(n, h)|(sz, (n,h)) <- [0..s] `zip` space r ] 

-- attempt to falsify a valid matching String AB/=BA
--swapHalf (x:y:xs)  = y : x : xs
--swapHalf x = x
swapHalf xs | right /= left = right ++ left
            | otherwise = map (chr.(+1).ord) xs
    where block = splitAt (length xs `div` 2) xs
          left = fst block
          right = snd block

deepCheck 1 p = quickCheckWith (stdArgs {maxSuccess = 100, maxSize = 8}) p
deepCheck n p = do 
                quickCheckWith (stdArgs {maxSuccess = 100, maxSize = 7}) p
                deepCheck (n-1) p
main = do
       putStrLn "prop_Nil"
       deepCheck iterations prop_Nil
       putStrLn "prop_Eps"
       deepCheck iterations prop_Eps
       putStrLn "prop_Atom"
       deepCheck iterations prop_Atom
       putStrLn "prop_Plus"
       deepCheck iterations prop_Plus
       putStrLn "prop_Seq"
       deepCheck iterations prop_Seq
       

       putStrLn "prop_Grep:"
       deepCheck iterations prop_Grep
       putStrLn "prop_AltAssoc:"
       deepCheck iterations prop_AltAssoc
       putStrLn "prop_AltCom:"
       deepCheck iterations prop_AltCom
       putStrLn "prop_AltIdem:"
       deepCheck iterations prop_AltIdem
       putStrLn "prop_CatAssoc:"
       deepCheck iterations prop_CatAssoc
       putStrLn "prop_DistLeft"
       deepCheck iterations prop_DistLeft
       putStrLn "prop_DistRight"
       deepCheck iterations prop_DistRight     
       putStrLn "prop_Clo"
       deepCheck iterations prop_Clo     
       putStrLn "prop_Clo2"
       deepCheck iterations prop_Clo2     
       
       putStrLn "prop_Derived"
       deepCheck iterations prop_Derived
       putStrLn "END"
       where iterations = 100



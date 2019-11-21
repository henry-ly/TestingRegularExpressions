-- Taken from A Play on Regular Expressions - A Functional Pearl Act 1 Scene 1
-- https://sebfisch.github.io/haskell-regexp/regexp-play.pdf

import Test.QuickCheck
import System.Process 
import System.Exit (ExitCode(ExitSuccess)) 
import System.IO.Unsafe
import Test.QuickCheck.Monadic  
import FEAT



data Reg = 
    Eps         -- Epsilon
  | Sym Char    -- Token
  | Alt Reg Reg -- Alternation
  | Seq Reg Reg -- Sequence
  | Rep Reg     -- Repetition
  deriving Show

space :: Reg -> Space String
space (Sym c)  = pay (unit [c])
space Eps       = unit ""
space (p `Alt` q) = space p +++ space q
space (p `Seq` q) = unit (++) `app` space p `app` space q
space (Rep p)  = w
 where
  w = unit "" +++ pay (unit (++) `app` unpay (space p) `app` w)

accept :: Reg -> String -> Bool
accept Eps u       = null u
accept (Sym c) u   = u == [c]
accept (Alt p q) u = accept p u || accept q u
accept (Seq p q) u = or [accept p u1 && accept q u2 | (u1, u2) <- split u]
accept (Rep r) u   = or [and [accept r ui | ui <- ps] | ps <- parts u]

split :: [a] -> [([a], [a])]
split []     = [([], [])]
split (c:cs) = ([], c : cs) : [(c : s1, s2) | (s1, s2) <- split cs]

parts :: [a] -> [[[a]]]
parts []     = [[]]
parts [c]    = [[[c]]]
parts (c:cs) = concat [[(c : p) : ps, [c] : p : ps] | p:ps <- parts cs]

alphabet :: Gen Char
alphabet = elements ['a' .. 'c']


literal = do  
          x <- alphabet
          return $ Sym x

regexPlusGen s1 s2 = do
                     x <- s1
                     y <- s2
                     return $ Alt x y

regexManyGen s1 = do
                  x <- s1
                  return $ Rep x

regexCatGen s1 s2 = do
                    x <- s1
                    y <- s2
                    return $ Seq x y

instance Arbitrary Reg where
    arbitrary = sized arbitraryExpression

arbitraryExpression n = frequency[(1, return Eps)
                                 ,(1, literal)
                                 ,(n, regexCatGen subexpr subexpr)
                                 ,(n, regexPlusGen subexpr subexpr)
                                 ,(n, regexManyGen subexpr)
                                 ]
                    where 
                      subexpr = arbitraryExpression (n `div` 2)

unixGrep :: String -> String -> IO Bool
unixGrep s r = do
            exitCode <- system $ "echo \"" ++ s ++ "\" | grep -E -w -q \"" ++ r ++"\""
            case exitCode of
                ExitSuccess ->  return True
                _           ->  return False

-- convert a regular expression to UNIX regex in a string representation
showExpr :: Reg -> String
showExpr Eps = []
showExpr (Sym a) = [a]
showExpr (Rep a) = "(" ++ showExpr a ++ ")" ++ "*"
showExpr (Alt a b) = "(" ++ showExpr a ++"|"++ showExpr b ++ ")"
showExpr (Seq a b) = showExpr a ++ showExpr b

genInputStrings :: Int -> Reg -> Gen String
genInputStrings s r = oneof[genMatching s r, genNotMatching s]  

genMatching :: Int -> Reg -> Gen String
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

{-
insertAt :: Char -> Int -> [Char] -> [Char]
insertAt newChar 0 xs = newChar : xs
insertAt newChar n (x:xs) = x : insertAt newChar (n - 1) xs

genNotMatching :: Int -> Reg -> Gen String
genNotMatching s r = do
                     m <- genMatching s r
                     i <- choose(0, length m)
                     return $ insertAt '!' i m
-}

{-
removeAt :: Int -> [Char] -> [Char]
removeAt 0 (x:xs) = xs
removeAt n (x:xs) = x : removeAt (n - 1) xs
removeAt _ _ = []

genNotMatching:: Int -> Reg -> Gen String
genNotMatching s r = do
                    m <- genMatching s r 
                    i <- choose(0, length m)
                    return $ removeAt i m 
-}

prop_Grep :: Reg -> Property
prop_Grep r1 = monadicIO $ do  
                           testString <-  run(generate(genInputStrings 10 r1))
                           booleanGrep <- run(unixGrep testString (showExpr r1))
                           monitor (counterexample testString)
                           assert $ accept r1 testString == booleanGrep
prop_Eps :: Property
prop_Eps = monadicIO $ do
                       testString <- run(generate(genInputStrings 10 Eps))
                       monitor(counterexample testString)
                       assert $ accept Eps testString == null testString

prop_Atom :: Char -> Property
prop_Atom a = monadicIO $ do
                          testString <- run(generate(genInputStrings 10 (Sym a)))
                          monitor(counterexample testString)
                          assert $ accept (Sym a) testString == (testString == [a])

prop_Plus :: Reg -> Reg -> Property
prop_Plus r1 r2 = monadicIO $ do
                              testString <- run(generate(genInputStrings 10 r1))
                              monitor(counterexample testString)
                              assert $ accept (r1 `Alt` r2) testString == accept r1 testString || accept r2 testString

prop_Seq :: Reg -> Reg -> Property
prop_Seq r1 r2 = monadicIO $ do
                             testString <- run(generate(genInputStrings 10 r1))
                             monitor(counterexample testString)
                             assert $ accept  (r1 `Seq` r2) testString ==
                               or[accept r1 (take i testString) && accept r2 (drop i testString) | i <- [0 .. length testString]]

prop_Clo :: Reg -> Property
prop_Clo r1 = monadicIO $ do
                          testString <- run(generate(genInputStrings 10 r1))
                          monitor(counterexample testString)
                          assert $ accept (Rep r1) testString == null testString || (accept (r1 `Seq` Rep r1) testString)

-- will be called from properties
testMatcher ::  Reg -> Reg -> Property
testMatcher r1 r2 = monadicIO  $ do
                                 testString <- run(generate(genInputStrings 10 r1))
                                 monitor(counterexample testString)
                                 assert $ accept r1 testString == accept r2 testString

--Alternation Laws
prop_AltAssoc ::  Reg -> Reg -> Reg -> Property
prop_AltAssoc a b c = testMatcher (a `Alt` (b `Alt` c)) ((a `Alt` b) `Alt` c)

prop_AltCom :: Reg -> Reg -> Property
prop_AltCom a b = testMatcher (a `Alt` b) (b `Alt` a)

prop_AltIdem :: Reg -> Property
prop_AltIdem a = testMatcher (a `Alt` a) a

--Concatenation Laws
prop_CatAssoc :: Reg -> Reg -> Reg -> String -> Property
prop_CatAssoc a b c s = testMatcher (a `Seq` (b `Seq` c)) ((a `Seq` b) `Seq` c)

prop_DistLeft :: Reg -> Reg -> Reg -> Property
prop_DistLeft a b c = testMatcher (a `Seq` (b `Alt` c)) ((a `Seq` b) `Alt` (a `Seq` c))

prop_DistRight :: Reg -> Reg -> Reg -> Property
prop_DistRight a b c = testMatcher ((a `Alt` b) `Seq` c) ((a `Seq` c) `Alt` (b `Seq` c))

--Closure Properties
prop_Clo2 :: Reg -> Property
prop_Clo2 a = testMatcher (Rep(Rep a)) (Rep a)

prop_Clo3 :: Reg -> String -> Property
prop_Clo3 a s = testMatcher (Eps `Alt` (a `Seq` Rep a)) (Rep a)

deepCheck 1 p = quickCheckWith (stdArgs {maxSuccess = 100, maxSize = 8}) p
deepCheck n p = do
                quickCheckWith (stdArgs {maxSuccess = 100, maxSize = 8}) p
                deepCheck (n-1) p

main = do
       putStrLn "prop_Nil"
       --deepCheck iterations prop_Nil
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
       putStrLn "prop_Clo1"
       deepCheck iterations prop_Clo
       putStrLn "prop_Clo2"
       deepCheck iterations prop_Clo2
       putStrLn "prop_Clo3"
       deepCheck iterations prop_Clo3
       putStrLn "END"
       where iterations = 500


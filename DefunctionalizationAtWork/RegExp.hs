{-# LANGUAGE OverloadedStrings #-}
import GHC.Exts (IsString(..))
import Test.QuickCheck
import System.Process 
import System.Exit
import System.IO.Unsafe
import Test.QuickCheck.Monadic
import Control.Monad.IO.Class
import Control.Monad(liftM, liftM2)
import FEAT



data Regexp = Zero                  -- empty
            | One                   -- epsilon
            | Lit Char              -- single character
            | Plus Regexp Regexp    -- union (+)
            | Cat  Regexp Regexp    -- concatenation (.)
            | Many Regexp           -- repetition (*)
            deriving Show


infixl 6 <+>
infixl 7 <>

space :: Regexp -> Space String
space (Lit c)  = pay (unit [c])
space Zero       = empty
space One       = unit ""
space (p `Plus` q) = space p +++ space q
space (p `Cat` q) = unit (++) `app` space p `app` space q
space (Many p)  = w
 where
  w = unit "" +++ pay (unit (++) `app` unpay (space p) `app` w)

(<+>) :: Regexp -> Regexp -> Regexp
Zero <+> e = e
e <+> Zero = e
e1 <+> e2  = Plus e1 e2

(<>) :: Regexp -> Regexp -> Regexp
Zero <> _   = Zero
_ <> Zero   = Zero
One <> e    = e
e <> One    = e
e1 <> e2    = Cat e1 e2

many :: Regexp -> Regexp 
many Zero     = One
many One       = One
many (Many e)  = Many e
many e         = Many e

type Cont= String -> Bool

accept :: Regexp -> String -> Cont -> Bool  -- worker function
accept Zero    cs      k = False
accept One     cs      k = k cs
accept (Lit c) (c':cs) k = c==c' && k cs
accept (Lit c) []      k = False
accept (Cat e1 e2) cs  k = accept e1 cs (\cs' -> accept e2 cs' k)
accept (Plus e1 e2) cs k = accept e1 cs k || accept e2 cs k
accept (Many e) cs k     = acceptMany e cs k
  where 
     acceptMany e cs k 
       = k cs || accept e cs (\cs' -> cs'/=cs && acceptMany e cs' k)


match :: Regexp -> String -> Bool
match re s = accept re s null

instance IsString Regexp where
  fromString cs = foldr ((<>) . Lit) One cs

-- show
--main = print (match ("ab" <> many "ba") "abbaba")
-- /show


alphabet :: Gen Char
alphabet = elements ['a' .. 'c']

literal = do  
          x <- alphabet
          return $ Lit x

regexCatGen s1 s2 = do
                    x <- s1
                    y <- s2
                    return $ Cat x y

regexPlusGen s1 s2 = do
                     x <- s1
                     y <- s2
                     return $ Plus x y

regexManyGen s1 = do
                  x <- s1
                  return $ Many x


instance Arbitrary Regexp where
    arbitrary = sized arbitraryExpression

arbitraryExpression n = frequency[(1, return Zero)
                                 ,(1, return One)
                                 ,(1, literal)
                                 ,(n, regexPlusGen subexpr subexpr)
                                 ,(n, regexCatGen subexpr subexpr)
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
showExpr :: Regexp -> String
showExpr One = []
showExpr (Lit a) = [a] 
showExpr (Many a) = "(" ++ showExpr a ++ ")" ++ "*" 
showExpr (Plus a b) = "(" ++ showExpr a ++"|"++ showExpr b ++ ")" 
showExpr (Cat a b) = showExpr a ++ showExpr b
showExpr _ = "a&&b"


prop_Grep :: Regexp -> Property
prop_Grep r1 =  monadicIO  $ do  
                             testString <-  run(generate(genInputStrings 10 r1))
                             booleanGrep <- run(unixGrep testString (showExpr r1))
                             monitor (counterexample testString)
                             assert $ match r1 testString == booleanGrep

prop_Nil :: Property
prop_Nil = monadicIO $ do
                       testString <- run(generate(genInputStrings 10 Zero)) 
                       monitor(counterexample testString)
                       assert $ match Zero testString == False

prop_Eps :: Property
prop_Eps = monadicIO $ do
                       testString <- run(generate(genInputStrings 10 One))
                       monitor(counterexample testString)
                       assert $ match One testString == null testString

prop_Atom :: Char -> Property
prop_Atom a = monadicIO $ do
                          testString <- run(generate(genInputStrings 10 (Lit a)))
                          monitor(counterexample testString)
                          assert $ match (Lit a) testString == (testString == [a])

prop_Plus :: Regexp -> Regexp -> Property
prop_Plus r1 r2 = monadicIO $ do
                              testString <- run(generate(genInputStrings 10 r1))
                              monitor(counterexample testString)
                              assert $ match (r1 `Plus` r2) testString == match r1 testString || match r2 testString

prop_Seq :: Regexp -> Regexp -> Property
prop_Seq r1 r2 = monadicIO $ do
                             testString <- run(generate(genInputStrings 10 r1))
                             monitor(counterexample testString)
                             assert $ match (r1 `Cat` r2) testString ==
                               or[match r1 (take i testString) && match r2 (drop i testString) | i <- [0 .. length testString]]


prop_Clo :: Regexp -> Property
prop_Clo r1 = monadicIO $ do
                          testString <- run(generate(genInputStrings 10 r1))
                          monitor(counterexample testString)
                          assert $ match (Many r1) testString == null testString || (match (r1 `Cat` Many r1) testString)


-- will be called from properties
testMatcher ::  Regexp -> Regexp -> Property
testMatcher r1 r2 = monadicIO  $ do
                                 testString <- run(generate(genInputStrings 10 r1))
                                 monitor(counterexample testString)
                                 assert $ match r1 testString == match r2 testString

--Alternation Laws
prop_AltAssoc :: Regexp -> Regexp -> Regexp -> Property
prop_AltAssoc a b c = testMatcher (a `Plus` (b `Plus` c)) ((a `Plus` b) `Plus` c) 

prop_AltCom :: Regexp -> Regexp -> Property
prop_AltCom a b = testMatcher (a `Plus` b) (b `Plus` a)

prop_AltIdem :: Regexp -> Property
prop_AltIdem a = testMatcher (a `Plus` a) a 

--Concatenation Laws
prop_CatAssoc :: Regexp -> Regexp -> Regexp -> Property
prop_CatAssoc a b c = testMatcher (a `Cat` (b `Cat` c))  ((a `Cat` b) `Cat` c) 


prop_DistLeft :: Regexp -> Regexp -> Regexp -> Property
prop_DistLeft a b c = testMatcher (a `Cat` (b `Plus` c)) ((a `Cat` b) `Plus` (a `Cat` c))

prop_DistRight :: Regexp -> Regexp -> Regexp -> Property
prop_DistRight a b c = testMatcher ((a `Plus` b) `Cat` c) ((a `Cat` c) `Plus` (b `Cat` c))

--Closure Properties
prop_Clo2 :: Regexp -> Property
prop_Clo2 a = testMatcher (Many (Many a)) (Many a) 

prop_Clo3 :: Regexp -> Property
prop_Clo3 a = testMatcher (One `Plus` (a `Cat` (Many a))) (Many a)



genInputStrings :: Int -> Regexp -> Gen String
genInputStrings s r = oneof[genMatching s r, genNotMatching s] 

genMatching :: Int -> Regexp -> Gen String
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
insertAt newChar 0 xs = newChar:xs
insertAt newChar n (x:xs)=x:insertAt newChar (n - 1) xs

genNotMatching:: Int -> Regexp -> Gen String
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

genNotMatching:: Int -> Regexp -> Gen String
genNotMatching s r = do
                    m <- genMatching s r
                    i <- choose(0, length m)
                    return $ removeAt i m
-}

deepCheck 1 p = quickCheckWith (stdArgs {maxSize = 8}) p
deepCheck n p = do
                quickCheckWith (stdArgs {maxSize = 8}) p
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
       putStrLn "prop_Clo1"
       deepCheck iterations prop_Clo     
       putStrLn "prop_Clo2"
       deepCheck iterations prop_Clo2     
       putStrLn "prop_Clo3"
       deepCheck iterations prop_Clo3     
       putStrLn "prop_Brz"
       deepCheck iterations prop_Brz     
       putStrLn "prop_Brz2"
       deepCheck iterations prop_Brz2
       putStrLn "END"
       where iterations = 500

(.+.),(.>.) :: Regexp -> Regexp -> Regexp
Zero .+. q   = q 
p   .+. Zero = p 
p   .+. q   = p `Plus` q


Zero .>. q   = Zero
p   .>. Zero = Zero
One .>. q   = q 
p   .>. One = p 
p   .>. q   = p `Cat` q

eps :: Regexp -> Bool
eps One      = True
eps (p `Plus` q) = eps p || eps q
eps (p `Cat` q) = eps p && eps q
eps (Many _)  = True
eps _         = False

step :: Regexp -> Char -> Regexp
step (Lit a)  x  | a == x    = One 
                 | otherwise = Zero
step (p `Plus` q) x = step p x .+. step q x 
step (p `Cat` q) x = (step p x .>. q) .+. if eps p then step q x else Zero
step (Many p)  x = step p x .>. Many p
step _         x = Zero

prop_Brz ::  Regexp -> Property
prop_Brz r1 = monadicIO  $ do
                                 testString <- run(generate(genInputStrings 10 r1))
                                 assert $ match r1 "" == eps r1

prop_Brz2 ::  Regexp -> Property
prop_Brz2 r1 = monadicIO  $ do

                                 testString <- run(generate(genInputStrings 10 r1))
                                 monitor(counterexample testString)
                                 pre (not (null testString))
                                 assert $ match r1 testString == match (step r1 (head testString)) (tail testString)


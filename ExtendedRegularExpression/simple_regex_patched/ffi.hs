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
-- note to self: ghci ffi.hs simple_re_match.o

data Regex a =
           Eps 
         | Lit a 
         | Wildcard
         | Cat (Regex a) (Regex a)
         | Clo (Regex a)
         | Plus (Regex a)
         deriving (Eq, Show)

foreign import ccall "match"
     c_match :: CString -> CString -> CInt

matcher :: String -> String -> IO Bool
matcher a b = do 
              newRegex <- newCString a
              newString <- newCString b
              return $ toBool (c_match newRegex newString)

space :: Regex Char -> Space String
space (Lit c)  = pay (unit [c])
space Eps      = unit ""
space (Wildcard) = foldr (+++) empty (map (space.Lit) (['1'..'9']++['a'..'z']))
space (p `Cat` q) = unit (++) `app` space p `app` space q
space (Clo p)  = w
 where
  w = unit "" +++ pay (unit (++) `app` unpay (space p) `app` w)
space (Plus p) = space(p `Cat` Clo p)

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
                                 ,(1, liftM Plus subexpr)
                                 ]
                   where subexpr = arbitraryExpression (n `div` 2)


showExpr :: Regex Char -> String
showExpr Eps = ""
showExpr (Lit a) = [a] 
showExpr (Clo a) = showExpr a ++ "*"
showExpr (Plus a) = showExpr a ++ "+"
showExpr (Cat a b) = showExpr a ++ showExpr b
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
                            match <- run $ matcher (showExpr r) ts
                            monitor(counterexample ts)
                            assert (grep == match)
prop_Eps :: Property
prop_Eps = monadicIO $ do
                       testString <- run $ generate(genInputStrings 10 Eps)
                       match <- run $ matcher (showExpr Eps) testString
                       monitor(counterexample testString)
                       assert $ match == null testString

prop_Atom :: Property
prop_Atom = monadicIO $ do
                          a <- run $ generate alphabet
                          testString <- run $ generate(genInputStrings 10 (Lit a)) 
                          match <- run $ matcher ("^" ++ [a] ++"$") testString
                          monitor(counterexample ("^"++ [a] ++"$"))
                          monitor(counterexample testString)
                          assert $ match == (testString == [a])


prop_Seq :: Regex Char -> Regex Char -> Property
prop_Seq r1 r2 = monadicIO $ do
                             testString <- run $ generate(genInputStrings 10 r1)
                             match <- run $ matcher (showExpr (r1 `Cat` r2)) testString
                             monitor(counterexample testString)
                             assert $ match == 
                               or[(unsafePerformIO(matcher (showExpr r1) (take i testString))) 
                                 && (unsafePerformIO(matcher (showExpr r2) (drop i testString))) 
                                 | i <- [0 .. length testString]]
prop_Clo :: Regex Char -> Property
prop_Clo r1 = monadicIO $ do
                          testString <- run(generate(genInputStrings 10 r1))
                          match <- run $ matcher(showExpr(Clo r1)) testString
                          match2 <-  run $ matcher(showExpr (r1 `Cat` Clo r1)) testString
                          monitor(counterexample testString)
                          assert $ match == null testString || match2



testMatcher ::  Regex Char -> Regex Char -> Property
testMatcher r1 r2 = monadicIO  $ do
                                 testString <- run(generate(genInputStrings 10 r1))
                                 match <- run $ matcher (showExpr r1) testString
                                 match2 <- run $ matcher (showExpr r2) testString
                                 monitor(counterexample testString)
                                 assert $ match == match2

prop_CatAssoc :: Regex Char -> Regex Char -> Regex Char -> Property
prop_CatAssoc a b c = testMatcher (a `Cat` (b `Cat` c)) ((a `Cat` b) `Cat` c)

prop_Clo2 :: Regex Char -> Property
prop_Clo2 a = testMatcher (Clo (Clo a)) (Clo a)


deepCheck 1 p = quickCheckWith (stdArgs {maxSuccess = 100, maxSize = 8}) p
deepCheck n p = do 
                quickCheckWith (stdArgs {maxSuccess = 100, maxSize = 8}) p
                deepCheck (n-1) p
main = do
       putStrLn "prop_Nil"
  --     deepCheck iterations prop_Nil
       putStrLn "prop_Eps"
       deepCheck iterations prop_Eps
       putStrLn "prop_Atom"
       deepCheck iterations prop_Atom
       putStrLn "prop_Plus"
      -- deepCheck iterations prop_Plus
       putStrLn "prop_Seq"
       deepCheck iterations prop_Seq
       

       putStrLn "prop_Grep:"
       deepCheck iterations prop_Grep
       putStrLn "prop_AltAssoc:"
    --   deepCheck iterations prop_AltAssoc
       putStrLn "prop_AltCom:"
       --deepCheck iterations prop_AltCom
       putStrLn "prop_AltIdem:"
       --deepCheck iterations prop_AltIdem
       putStrLn "prop_CatAssoc:"
       deepCheck iterations prop_CatAssoc
       putStrLn "prop_DistLeft"
       --deepCheck iterations prop_DistLeft
       putStrLn "prop_DistRight"
       --deepCheck iterations prop_DistRight     
       putStrLn "prop_Clo1"
       deepCheck iterations prop_Clo     
       putStrLn "prop_Clo2"
       deepCheck iterations prop_Clo2     
       putStrLn "prop_Clo3"
      -- deepCheck iterations prop_Clo3     
       

       putStrLn "END"
       where iterations = 500


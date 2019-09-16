{-# LANGUAGE OverloadedStrings #-}
import GHC.Exts (IsString(..))
import Test.QuickCheck

data Regexp = Zero                  -- empty
            | One                   -- epsilon
            | Lit Char              -- single character
            | Plus Regexp Regexp    -- union (+)
            | Cat  Regexp Regexp    -- concatenation (.)
            | Many Regexp           -- repetition (*)
            deriving Show

infixl 6 <+>
infixl 7 <>

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
alphabet = elements ['a', 'b', 'c']

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

arbitraryExpression 0 = frequency[(1, return Zero)
                                 ,(1, return One)
                                 ,(1, literal)
                                 ]
                                   
arbitraryExpression n = frequency[(1, regexPlusGen subexpr subexpr)
                                 ,(1, regexCatGen subexpr subexpr)
                                 ,(1, regexManyGen subexpr)
                                 ]
                          where subexpr = arbitraryExpression (n `div` 2)

prop_zero :: String -> Bool
prop_zero s = match Zero s == null s

prop_one :: String -> Bool
prop_one s = match One s == null s

--Alternation Laws
prop_AltAssoc :: Regexp -> Regexp -> Regexp -> String -> Bool
prop_AltAssoc a b c s = match (Plus a (Plus b c)) s == match (Plus (Plus a b) c) s

prop_AltCom :: Regexp -> Regexp -> String -> Bool
prop_AltCom a b s = match (Plus a b) s == match (Plus b a) s

prop_AltIdem :: Regexp -> String -> Bool
prop_AltIdem a s = match (Plus a a) s == match a s

prop_AltIden :: Regexp -> String -> Bool
prop_AltIden a s = match (Plus a Zero) s == match a s

--Concatenation Laws
prop_CatAssoc :: Regexp -> Regexp -> Regexp -> String -> Bool
prop_CatAssoc a b c s = match (Cat a (Cat b c)) s == match (Cat (Cat a b) c) s

prop_CatIden :: Regexp -> String -> Bool
prop_CatIden a s = match (Cat a Zero) s == match a s

prop_DistLeft :: Regexp -> Regexp -> Regexp -> String -> Bool
prop_DistLeft a b c s = match (Cat a (Plus b c)) s == match (Plus (Cat a b) (Cat a c)) s

prop_DistRight :: Regexp -> Regexp -> Regexp -> String -> Bool
prop_DistRight a b c s = match (Cat (Plus a b) c) s == match (Plus (Cat a c)(Cat b c)) s

--Closure Properties
prop_Clo :: Regexp -> String -> Bool
prop_Clo a s = match (Many (Many a)) s == match (Many a) s

prop_Clo2 :: Regexp -> String -> Bool
prop_Clo2 a s = match (Plus One (Cat a (Many a))) s == match (Many a) s

deepCheck 1 p = quickCheckWith (stdArgs {maxSuccess = 10000}) p
deepCheck n p = do
                quickCheckWith (stdArgs {maxSuccess = 10000}) p
                deepCheck (n-1) p
main = do
       putStrLn "prop_AltAssoc:"
       deepCheck iterations prop_AltAssoc
       putStrLn "prop_AltCom:"
       deepCheck iterations prop_AltCom
       putStrLn "prop_AltIdem:"
       deepCheck iterations prop_AltIdem
       putStrLn "prop_AltIden:"
       deepCheck iterations prop_AltIden
       putStrLn "prop_CatAssoc:"
       deepCheck iterations prop_CatAssoc
       putStrLn "prop_CatIden:"
       deepCheck iterations prop_CatIden
       putStrLn "prop_DistLeft"
       deepCheck iterations prop_DistLeft
       putStrLn "prop_DistRight"
       deepCheck iterations prop_DistRight
       putStrLn "prop_Clo"
       deepCheck iterations prop_Clo
       putStrLn "prop_Clo2"
       deepCheck iterations prop_Clo2
       putStrLn "End"
       where iterations = 100


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
main = print (match ("ab" <> many "ba") "abbaba")
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

--zero matched the empty string bug or a feature?
prop_zero :: String -> Bool
prop_zero s = match Zero s == null s

prop_one :: String -> Bool
prop_one s = match One s == null s

prop_AltAssoc :: Regexp -> Regexp -> Regexp -> String -> Bool
prop_AltAssoc a b c s = match (Plus a (Plus b c)) s == match (Plus (Plus a b) c) s


prop_AltCom :: Regexp -> Regexp -> String -> Bool
prop_AltCom a b s = match (Plus a b) s == match (Plus b a) s

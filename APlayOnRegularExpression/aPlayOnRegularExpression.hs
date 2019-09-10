import Test.QuickCheck
-- Taken from A Play on Regular Expressions - A Functional Pearl Act 1 Scene 1
-- https://sebfisch.github.io/haskell-regexp/regexp-play.pdf

data Reg = 
    Eps         -- Epsilon
  | Sym Char    -- Token
  | Alt Reg Reg -- Alternation
  | Seq Reg Reg -- Sequence
  | Rep Reg     -- Repetition
  deriving Show
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
alphabet = elements "abc"


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

-- this implementation is very slow for larger strings
instance Arbitrary Reg where
    arbitrary = sized arbitraryExpression

arbitraryExpression 0 = frequency[(1, return Eps),(1, literal)]  
arbitraryExpression n = frequency[(1, regexCatGen subexpr subexpr), (1, regexPlusGen subexpr subexpr), (1, regexManyGen subexpr)]
                    where subexpr = arbitraryExpression (n `div` 2)


prop_one :: String -> Bool
prop_one s = accept Eps s == null s


prop_Alt :: Reg -> Reg -> String -> Bool
prop_Alt a b s = accept (Alt a b) s == (accept a s || accept b s)


prop_AltCom :: Reg -> Reg -> String -> Bool
prop_AltCom a b s = accept (Alt a b) s == accept (Alt b a) s


prop_SeqAssoc :: Reg -> Reg -> Reg -> String -> Bool
prop_SeqAssoc a b c s = accept (Seq a (Seq b c)) s == accept (Seq(Seq a b) c) s

prop_Clo :: Reg -> String -> Bool
prop_Clo a s = accept (Rep a) s == accept (Rep (Rep a)) s



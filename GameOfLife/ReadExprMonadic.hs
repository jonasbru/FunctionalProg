module ReadExprMonadic where
-- ReadExprRefactored now using the Parser monad

import Parsing -- final version: monadic parser

import Data.Char(isSpace)
import Data.Maybe
import Test.QuickCheck

data Expr
  = Num Integer
  | Add Expr Expr
  | Mul Expr Expr
 deriving (Eq
          ,Show
          )
-- instance Show Expr where 
--    show = showExpr

-------------------------------------------------------------
-- parse an integer
-- with help from Read class
integer :: Parser Integer 
integer = nat +++ neg -- natural or negative
  where 
  nat = -- pmap read $ oneOrMore digit
      do ds <- oneOrMore digit
         return (read ds)

  neg = -- char '-' >> pmap negate nat
      do char '-'
         n <- nat
         return $ negate n
                      
num :: Parser Expr
num  = pmap Num integer

-- BNF grammar (not Haskell!)
-- <expression> ::=  <term>   "+" <expression> | <term>
-- <term>       ::=  <factor> "*" <term>       | <factor>
-- <factor>     ::= "(" <expression> ")" | <number>

chain p c f = -- pmap (foldr1 f) $ p <:> zeroOrMore (char c >-> p )
         do a <- p 
            char c
            e <- chain p c f 
            return $ f a e
         +++ p 

expr   = chain term '+' Add
term   = chain factor '*' Mul

factor = -- char '(' >-> expr <-< char ')' +++ num
    do char '('   
       e <- expr
       char ')'
       return e
    +++ num

-- top level parser
-- remove spaces and expect no junk!
readExpr :: String -> Maybe Expr              

readExpr s = let s' = filter (not.isSpace) s
             in case parse expr s' of
                     Just (e,"") -> Just e
                     _           -> Nothing
----------------------------------------------
-- show, eval and quickCheck

showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2

showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

eval :: Expr -> Integer
eval (Num n)    = n
eval (Add e1 e2)    = eval e1 + eval e2
eval (Mul e1 e2)= eval e1 * eval e2


rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum),(s,rOp)]
  where
   s' = s `div` 2
   rNum = fmap Num arbitrary
   --   do 
   --    n <- arbitrary
   --    return $ Num n

   rOp = do 
      op <- elements [Add,Mul]
      e1 <- rExpr s'
      e2 <- rExpr s'
      return $ op e1 e2

instance Arbitrary Expr where
  arbitrary = sized rExpr
----------------------------
-- Testing 

prop_readExpr e =  isJust e' &&  eval e == eval (fromJust e')
  where e' = readExpr (showExpr e)

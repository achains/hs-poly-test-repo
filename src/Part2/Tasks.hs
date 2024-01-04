module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) lhs rhs = BinaryTerm Plus lhs rhs
infixl 7 |+|

(|-|) :: Term -> Term -> Term
(|-|) lhs rhs =  BinaryTerm Minus lhs rhs
infixl 7 |-|

(|*|) :: Term -> Term -> Term
(|*|) lhs rhs =  BinaryTerm Times lhs rhs
infixl 8 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = notImplementedYet

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expr = case expr of
   IntConstant{intValue=x} -> IntConstant{intValue=x}
   BinaryTerm{op=op, lhv=lhv, rhv=rhv} -> case op of 
      Plus -> lhv |+| rhv
      Minus -> lhv |-| rhv 
      Times -> lhv |*| rhv
   otherwise -> IntConstant(1)

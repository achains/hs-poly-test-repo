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
replaceVar varName replacement (IntConstant x) = IntConstant x 
replaceVar varName replacement (Variable name) | name == varName = replacement
                                               | otherwise       = Variable name 
replaceVar varName replacement (BinaryTerm op lhs rhs) = 
   let lhs' = replaceVar varName replacement lhs in 
      let rhs' = replaceVar varName replacement rhs in 
         BinaryTerm op lhs' rhs'                                         


-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm op lhs rhs) = 
   let lhs' = evaluate lhs in 
      let rhs' = evaluate rhs in 
         case (lhs', rhs') of 
            (IntConstant lhv, IntConstant rhv) -> case op of 
               Plus -> IntConstant (lhv + rhv)
               Minus -> IntConstant (lhv - rhv)
               Times -> IntConstant (lhv * rhv)
            _ -> BinaryTerm op lhs' rhs'
evaluate expression = expression

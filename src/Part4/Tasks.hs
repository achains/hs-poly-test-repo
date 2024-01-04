module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = 
    let helper x acc = x :< acc 
    in foldl helper REmpty

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    showsPrec _ lst = 
        let helper REmpty = showString ""
            helper (REmpty :< x) = shows x 
            helper (xs :< x) = helper xs . showString "," . shows x 
        in showString "[" . helper lst . showString "]" 

    show lst = showsPrec 0 lst ""

instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) (lx :< lxs) (rx :< rxs) = lx == rx && lxs == rxs
    (==) _ _ = False
    (/=) lhs rhs = not (lhs == rhs)
 
instance Semigroup (ReverseList a) where
instance Monoid (ReverseList a) where
instance Functor ReverseList where
instance Applicative ReverseList where
instance Monad ReverseList where

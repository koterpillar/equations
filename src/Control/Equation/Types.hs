{-# LANGUAGE RankNTypes #-}
module Control.Equation.Types (
    Operand (..),
    Expression (..),
    Equation (..),
    (=:=),
) where

import Control.Lens (Lens')

data Operand world value = Constant value | Variable (Lens' world value)

-- TODO: Non-linear expressions

data Expression world value = Expression [(value, Operand world value)]

-- TODO: Is there a LinearSpace typeclass?
instance Num value => Num (Expression world value) where
    Expression a + Expression b = simplify $ Expression (a ++ b)
    Expression a * b = case simplify b of
                           -- TODO: either a or b can be constant
                           Expression [(c, Constant v)] -> Expression $ map (opMul (c * v)) a
                           _ -> error "Cannot multiple two non-constant expressions"

    fromInteger i = Expression [(fromInteger 1, Constant (fromInteger i))]

    negate (Expression a) = Expression $ map (opMul (fromInteger (-1))) a

    -- These have to correspond to each other
    abs = id
    signum = const 1

-- Simplify the expression by cancelling out the constants.
-- The variables cannot be cancelled out because they cannot be compared to
-- each other.
simplify :: Num value => Expression world value -> Expression world value
simplify (Expression exp) = Expression $ [(fromInteger 1, Constant csum)] ++ vars
    where vars = filter isVarExp exp
          csum = sum $ map cvalue $ filter (not . isVarExp) exp
          isVarExp (_, Constant _) = True
          isVarExp (_, Variable _) = False
          cvalue (i, Constant j) = i * j

opMul :: Num value => value -> (value, Operand world value) -> (value, Operand world value)
opMul i (j, k) = (i * j, k)

data Equation world value = Equation (Expression world value)

(=:=) :: Num value => Expression world value -> Expression world value -> Equation world value
a =:= b = Equation (a - b)

infix 4 =:=

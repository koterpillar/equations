{-# LANGUAGE RankNTypes #-}
module Control.Equation.Types (
    Operand (..),
    Expression (..),
    Equation (..),
    (=:=),
) where

import Control.Lens (Lens')

import Data.Maybe

data Operand world value = Constant value | Variable (Lens' world value)

-- TODO: Non-linear expressions

data Expression world value = Expression { unExpression :: [(value, Operand world value)] }

-- TODO: Is there a LinearSpace typeclass?
instance Num value => Num (Expression world value) where
    Expression a + Expression b = simplify $ Expression (a ++ b)
    a * b = if isConstant b then constMul (fromJust $ constantValue b) a
            else if isConstant a then b * a
            else error "Cannot multiple two non-constant expressions"

    fromInteger i = Expression [(fromInteger 1, Constant (fromInteger i))]

    negate = constMul (-1)

    -- These have to correspond to each other
    abs = id
    signum = const 1

cvalue :: Num value => (value, Operand world value) -> Maybe value
cvalue (i, Constant j) = Just $ i * j
cvalue _ = Nothing

-- Simplify the expression by cancelling out the constants.
-- The variables cannot be cancelled out because they cannot be compared to
-- each other.
simplify :: Num value => Expression world value -> Expression world value
simplify (Expression exp) = Expression $ [(fromInteger 1, Constant csum)] ++ vars
    where vars = filter isVarExp exp
          csum = sum $ catMaybes $ map cvalue $ filter (not . isVarExp) exp
          isVarExp (_, Constant _) = True
          isVarExp (_, Variable _) = False

constantValue :: Num value => Expression world value -> Maybe value
constantValue = fmap sum . sequence . map cvalue . unExpression

isConstant :: Num value => Expression world value -> Bool
isConstant = isJust . constantValue

constMul :: Num value => value -> Expression world value -> Expression world value
constMul v = Expression . map (opMul v) . unExpression
    where opMul i (j, k) = (i * j, k)

data Equation world value = Equation (Expression world value)

(=:=) :: Num value => Expression world value -> Expression world value -> Equation world value
a =:= b = Equation (a - b)

infix 4 =:=

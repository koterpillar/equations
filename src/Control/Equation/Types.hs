{-# LANGUAGE RankNTypes #-}
module Control.Equation.Types (
    Expression (..),
    Equation (..),
    evar,
    (=:=),
) where

import Control.Lens (Lens')

import Data.Maybe

-- TODO: Non-linear expressions

-- Operands are either constants, or variables multiplied by constants
data Term world value = Constant value | Variable value (Lens' world value)

data Expression world value = Expression { unExpression :: [Term world value] }

evar :: Num value => Lens' world value -> Expression world value
evar var = Expression [Variable 1 var]

-- TODO: Is there a LinearSpace typeclass?
instance Num value => Num (Expression world value) where
    Expression a + Expression b = simplify $ Expression (a ++ b)
    a * b = if isConstant b then constMul (fromJust $ constantValue b) a
            else if isConstant a then b * a
            else error "Cannot multiple two non-constant expressions"

    fromInteger i = Expression [Constant $ fromInteger i]

    negate = constMul (-1)

    -- These have to correspond to each other
    abs = id
    signum = const 1

cvalue :: Num value => Term world value -> Maybe value
cvalue (Constant j) = Just j
cvalue _ = Nothing

-- Simplify the expression by cancelling out the constants.
-- The variables cannot be cancelled out because they cannot be compared to
-- each other.
simplify :: Num value => Expression world value -> Expression world value
simplify (Expression exp) = Expression $ Constant csum:vars
    where vars = filter isVarExp exp
          csum = sum $ catMaybes $ map cvalue $ filter (not . isVarExp) exp
          isVarExp (Constant _) = True
          isVarExp _ = False

constantValue :: Num value => Expression world value -> Maybe value
constantValue = fmap sum . sequence . map cvalue . unExpression

isConstant :: Num value => Expression world value -> Bool
isConstant = isJust . constantValue

constMul :: Num value => value -> Expression world value -> Expression world value
constMul v = Expression . map (opMul v) . unExpression
    where opMul i (Constant j) = Constant $ i * j
          opMul i (Variable j k) = Variable (i * j) k

data Equation world value = Equation (Expression world value)

(=:=) :: Num value => Expression world value -> Expression world value -> Equation world value
a =:= b = Equation (a - b)

infix 4 =:=

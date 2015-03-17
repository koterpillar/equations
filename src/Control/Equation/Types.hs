{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Equation.Types where

import Control.Lens

import Data.Maybe

-- Operands are either constants, or variables multiplied by constants
data Term world value = Constant value | Variable value (Lens' world value)

-- Value of a constant or Nothing
cvalue :: Num value => Term world value -> Maybe value
cvalue (Constant j) = Just j
cvalue _ = Nothing

data Expression world value = Expression { _terms :: [Term world value] }

makeLenses ''Expression

evar :: Num value => Lens' world value -> Expression world value
evar var = Expression [Variable 1 var]

econst :: value -> Expression world value
econst v = Expression [Constant v]

-- TODO: Is there a LinearSpace typeclass?
instance Num value => Num (Expression world value) where
    a + b = simplify $ Expression $ a ^. terms ++ b ^. terms
    a * b = if isConstant b then constMul (fromJust $ constantValue b) a
            else if isConstant a then b * a
            else error "Cannot multiple two non-constant expressions"

    fromInteger = econst . fromInteger

    negate = constMul (-1)

    -- These have to correspond to each other
    abs = id
    signum = const 1

-- Simplify the expression by cancelling out the constants.
-- The variables cannot be cancelled out because they cannot be compared to
-- each other.
simplify :: Num value => Expression world value -> Expression world value
simplify (Expression exp) = Expression $ Constant csum:vars
    where vars = filter isVarExp exp
          csum = sum $ catMaybes $ map cvalue $ filter (not . isVarExp) exp
          isVarExp (Constant _) = False
          isVarExp _ = True

-- Value of a constant expression, otherwise Nothing
constantValue :: Num value => Expression world value -> Maybe value
constantValue = fmap sum . sequence . map cvalue . (^. terms)

isConstant :: Num value => Expression world value -> Bool
isConstant = isJust . constantValue

-- Multiply an expression by a constant
constMul :: Num value => value -> Expression world value -> Expression world value
constMul v = over terms $ map $ opMul v
    where opMul i (Constant j) = Constant $ i * j
          opMul i (Variable j k) = Variable (i * j) k

data Equation world value = Equation { _expression :: Expression world value }

makeLenses ''Equation

(=:=) :: Num value => Expression world value -> Expression world value -> Equation world value
a =:= b = Equation (a - b)

infix 4 =:=

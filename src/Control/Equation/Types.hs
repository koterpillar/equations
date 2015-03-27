{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.Equation.Types where

import Control.Lens
import Control.Monad.Writer

import Data.List (nub)
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

-- Calculate the value of an expression given the world
value :: Num value => world -> Expression world value -> value
value w = sum . map (termValue w) . (^. terms)
    where termValue _ (Constant c) = c
          termValue w (Variable c v) = c * w ^. v

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

-- Encapsulate a variable reference to compare them for equality
-- Assume accessors aren't doing weird things and compare the variables by
-- comparing the values in two points
data VarRef world value = VarRef { vrVar :: Lens' world value
                                 , vrWorld :: world
                                 }

instance (Eq value, Num value) => Eq (VarRef world value) where
    a == b = w1 ^. vb == 1 && w2 ^. vb == 2
        where w1 = set va 1 (vrWorld a)
              w2 = set va 2 (vrWorld a)
              va = vrVar a
              vb = vrVar b

-- Calculate the derivative for the given variable in the current point
-- e.g. derivative x (x + 2 * x =:= 0) === 3
derivative :: Fractional value => VarRef world value -> Equation world value -> value
derivative = derivative' 0.0625

-- Find the derivative with given precision
derivative' :: Fractional value => value -> VarRef world value -> Equation world value -> value
derivative' epsilon var eq = (value w' ex - value w ex) / (x' - x)
    where ex = eq ^. expression
          v = vrVar var
          w = vrWorld var
          x = w ^. (vrVar var)
          x' = x + epsilon
          w' = set v x' w

-- Is the variable (Term) the same as a given VarRef?
sameVar :: (Eq value, Num value) => VarRef world value -> Term world value -> Bool
sameVar v (Constant _) = False
sameVar v (Variable _ v') = v == VarRef v' (vrWorld v)

-- Replace all occurrences of a variable with an expression
-- replaceVar x (2 * y) (x + y =:= 3) = (2 * y + y =:= 3)
replaceVar :: (Eq value, Fractional value) => VarRef world value -> Expression world value -> Equation world value -> Equation world value
replaceVar v replacement eq = over expression (((econst c * replacement) +) . over terms (filter (not . sameVar v))) eq
    where c = derivative v eq

-- Extract all the variables from a set of equations
-- Needs an instance of world to compare, etc.
extractVars :: (Eq value, Num value) => world -> [Equation world value] -> [VarRef world value]
extractVars w = nub . execWriter . mapM_ (extractVars' w)

extractVars' :: world -> Equation world value -> Writer [VarRef world value] ()
extractVars' w = mapM_ (extractVars'' w) . (^. expression.terms)

extractVars'' :: world -> Term world value -> Writer [VarRef world value] ()
extractVars'' w (Constant _) = return ()
extractVars'' w (Variable _ v) = tell [VarRef v w]

{-# LANGUAGE RankNTypes #-}
module Control.Equation.Solve where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.Maybe

import Control.Equation.Types

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

-- Assuming all variables are independent; this breaks use cases like:
-- radius + diameter = 3, where radius and diameter are distinct but dependent
-- perimeter + area = 10, this is not actually a linear equation at all!
-- In general, this can't be picked up automatically with any number of checks
-- because it's possible that a variable is a polynom of the other one of a
-- high enough power.

-- Extract all the variables from a set of equations
-- Needs an instance of world to compare, etc.
extractVars :: (Eq value, Num value) => world -> [Equation world value] -> [VarRef world value]
extractVars w = nub . execWriter . mapM_ (extractVars' w)

extractVars' :: world -> Equation world value -> Writer [VarRef world value] ()
extractVars' w = mapM_ (extractVars'' w) . (^. expression.terms)

extractVars'' :: world -> Term world value -> Writer [VarRef world value] ()
extractVars'' w (Constant _) = return ()
extractVars'' w (Variable _ v) = tell [VarRef v w]

-- Calculate the total coefficient for the given variable
-- e.g. coefficient x (x + 2 * x =:= 0) === 3
coefficient :: Num value => VarRef world value -> Equation world value -> value
coefficient var eq = value w2 ex - value w1 ex
    where ex = eq ^. expression
          w1 = set v 1 w
          w2 = set v 2 w
          v = vrVar var
          w = vrWorld var

-- Calculate the value of an expression given the world
value :: Num value => world -> Expression world value -> value
value w = sum . map (termValue w) . (^. terms)
    where termValue _ (Constant c) = c
          termValue w (Variable c v) = c * w ^. v

data SolutionStatus = NoSolutions | MultipleSolutions | SingleSolution
    deriving (Eq, Show)

solveLinear :: (Eq value, Fractional value) => [Equation world value] -> State world SolutionStatus
solveLinear eqs = do
    w <- get
    let vars = extractVars w eqs
    solveAgainst vars eqs

solveAgainst :: (Eq value, Fractional value) => [VarRef world value] -> [Equation world value] -> State world SolutionStatus
solveAgainst [] [] = return SingleSolution
solveAgainst _ [] = return MultipleSolutions
solveAgainst vs (eq:eqs) = do
    case findVar vs eq of
        Nothing -> do
            -- No variables in this equation. Is it 0=0 or 0=1?
            val <- gets $ flip value (eq ^. expression)
            if (val == 0) then solveAgainst vs eqs
                          else return NoSolutions
        Just (v, vs') -> do
            let vexp = excludeVar v eq
            let eqs' = map (replaceVar v vexp) eqs
            res <- solveAgainst vs' eqs'
            when (res == SingleSolution) $ do
                vval <- gets $ flip value vexp
                vrVar v .= vval
            return res

-- Find a variable in an equation with a nonzero coefficient
findVar :: (Eq value, Num value) => [VarRef world value] -> Equation world value -> Maybe (VarRef world value, [VarRef world value])
findVar [] eq = Nothing
findVar (v:vs) eq = case coefficient v eq of
    0 -> do
        (v', vs) <- findVar vs eq
        return $ (v', v:vs)
    _ -> Just (v, vs)

-- Is the variable (Term) the same as a given VarRef?
sameVar :: (Eq value, Num value) => VarRef world value -> Term world value -> Bool
sameVar v (Constant _) = False
sameVar v (Variable _ v') = v == VarRef v' (vrWorld v)

-- For a given equation, an expression that is equal to the value of the given
-- variable but does not include it
-- excludeVar x (2 * x + 3 * y =:= 5) = 2.5 - 1.5 * y
excludeVar :: (Eq value, Fractional value) => VarRef world value -> Equation world value -> Expression world value
excludeVar v eq = (econst $ (-1) / c) * (over terms (filter (not . sameVar v)) $ eq ^. expression)
    where c = coefficient v eq

-- Replace all occurrences of a variable with an expression
-- replaceVar x (2 * y) (x + y =:= 3) = (2 * y + y =:= 3)
replaceVar :: (Eq value, Num value) => VarRef world value -> Expression world value -> Equation world value -> Equation world value
replaceVar v replacement eq = over expression (((econst c * replacement) +) . over terms (filter (not . sameVar v))) eq
    where c = coefficient v eq

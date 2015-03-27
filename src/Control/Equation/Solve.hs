{-# LANGUAGE RankNTypes #-}
module Control.Equation.Solve where

import Control.Lens
import Control.Monad.State

import Data.Maybe

import Control.Equation.Types (
    Equation,
    expression,

    Expression,

    VarRef(..),

    value,
    derivative,
    replaceVar,
    extractVars,

    -- TODO: shouldn't expose this
    sameVar,
    econst,
    terms,
    )

-- Assuming all variables are independent; this breaks use cases like:
-- radius + diameter = 3, where radius and diameter are distinct but dependent
-- perimeter + area = 10, this is not actually a linear equation at all!
-- In general, this can't be picked up automatically with any number of checks
-- because it's possible that a variable is a polynom of the other one of a
-- high enough power.

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
findVar :: (Eq value, Fractional value) => [VarRef world value] -> Equation world value -> Maybe (VarRef world value, [VarRef world value])
findVar [] eq = Nothing
findVar (v:vs) eq = case derivative v eq of
    0 -> do
        (v', vs) <- findVar vs eq
        return $ (v', v:vs)
    _ -> Just (v, vs)

-- For a given equation, an expression that is equal to the value of the given
-- variable but does not include it
-- excludeVar x (2 * x + 3 * y =:= 5) = 2.5 - 1.5 * y
excludeVar :: (Eq value, Fractional value) => VarRef world value -> Equation world value -> Expression world value
excludeVar v eq = (econst $ (-1) / c) * (over terms (filter (not . sameVar v)) $ eq ^. expression)
    where c = derivative v eq

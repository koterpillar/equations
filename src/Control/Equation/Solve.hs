module Control.Equation.Solve (
    solveLinear,
) where

import Control.Monad.State

import Control.Equation.Types

solveLinear :: Num value => [Equation world value] -> State world ()
solveLinear = undefined

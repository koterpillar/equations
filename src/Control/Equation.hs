{-# LANGUAGE RankNTypes #-}
module Control.Equation (
    Expression,
    Equation,
    (=:=),
    evar,
    solveLinear,
    SolutionStatus(..),
) where

import Control.Equation.Solve (solveLinear, SolutionStatus(..))
import Control.Equation.Types

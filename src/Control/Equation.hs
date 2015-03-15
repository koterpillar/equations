{-# LANGUAGE RankNTypes #-}
module Control.Equation (
    Expression,
    Equation,
    (=:=),
    evar,
    solveLinear,
) where

import Control.Equation.Solve (solveLinear)
import Control.Equation.Types

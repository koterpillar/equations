{-# LANGUAGE RankNTypes #-}
module Control.Equation (
    Expression (..),
    Equation (..),
    (=:=),
    evar,
    econst,
    solveLinear,
) where

import Control.Lens (Lens')

import Control.Equation.Solve (solveLinear)
import Control.Equation.Types

evar :: Num value => Lens' world value -> Expression world value
evar var = Expression [(fromInteger 1, Variable var)]

econst :: Num value => value -> Expression world value
econst c = Expression [(fromInteger 1, Constant c)]

{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Equation.Test where

import Control.Lens
import Control.Monad.State

import Control.Equation

import Test.Framework
import Test.HUnit


data Playground = Playground { _sheep :: Float
                             , _cows :: Float
                             , _chicken :: Float
                             }

makeLenses ''Playground

test_solveLinear = do
    let equations = [ evar sheep + evar cows =:= 20
                    , evar cows + evar chicken =:= 30
                    , evar chicken + evar sheep =:= 40
                    ]

    -- TODO: the initial state is meaningless
    let playground = Playground 0 0 0
    let (result, solution) = runState (solveLinear equations) playground

    result @?= SingleSolution

    solution ^. sheep + solution ^. cows @?= 20
    solution ^. cows + solution ^. chicken @?= 30
    solution ^. chicken + solution ^. sheep @?= 40

    -- Actual solution
    solution ^. chicken @?= 25
    solution ^. sheep @?= 15
    solution ^. cows @?= 5

test_no_solutions = do
    let equations = [ evar _1 =:= 1
                    , evar _1 =:= 2
                    ]

    let result = evalState (solveLinear equations) (0, 0)

    result @?= NoSolutions

test_multiple_solutions = do
    let equations = [ evar _1 =:= evar _2
                    , evar _2 =:= evar _1
                    ]

    let result = evalState (solveLinear equations) (0, 0)

    result @?= MultipleSolutions

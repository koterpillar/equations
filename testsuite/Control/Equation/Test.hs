{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Equation.Test where

import Control.Lens
import Control.Monad.State

import Control.Equation

import Test.Framework
import Test.HUnit


data Playground = Playground { _sheep :: Int
                             , _cows :: Int
                             , _chicken :: Int
                             }

makeLenses ''Playground

test_solveLinear = do
    let equations = [ evar sheep + evar cows =:= 10
                    , evar cows + evar chicken =:= 15
                    , evar chicken + evar sheep =:= 20
                    ]

    -- TODO: the initial state is meaningless
    let playground = Playground 0 0 0
    let solution = execState (solveLinear equations) playground

    solution ^. sheep + solution ^. cows @?= 10
    solution ^. cows + solution ^. chicken @?= 15
    solution ^. chicken + solution ^. sheep @?= 20

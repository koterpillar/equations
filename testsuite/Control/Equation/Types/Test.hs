{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Equation.Types.Test where

import Control.Equation.Types

import Test.Framework
import Test.HUnit


-- Not useful in general case
instance Eq value => Eq (Term world value) where
    (Constant x) == (Constant y) = x == y
    _ == _ = False
deriving instance Eq value => Eq (Expression world value)
deriving instance Eq value => Eq (Equation world value)

instance Show value => Show (Term world value) where
    show (Constant c) = "Constant " ++ show c
    show (Variable c _) = "Variable " ++ show c ++ " v"
deriving instance Show value => Show (Expression world value)
deriving instance Show value => Show (Equation world value)


test_operators = do
    assertEqual
        (1 :: Expression Int Int)
        (Expression [ Constant 1
                    ])
    assertEqual
        (1 + 2 :: Expression Int Int)
        (Expression [ Constant 3
                    ])
    assertEqual
        (1 + 2 =:= 3 :: Equation Int Int)
        (Equation $ Expression [ Constant 0
                               ])

test_simplify = do
    assertEqual
        (Expression [Constant 1])
        (simplify $ Expression [Constant 1])

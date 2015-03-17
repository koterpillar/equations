{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} Control.Equation.Test
import {-@ HTF_TESTS @-} Control.Equation.Types.Test

main = htfMain htf_importedTests

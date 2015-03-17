Equation DSL based on lenses
----------------------------

A farmer has sheep, cows and chicken. If they count sheep and cows together,
there are 20. Cows and chicken come out to 30, and chicken and sheep come out
to 40. How many chicken, sheep and cows are there?

```haskell
data Playground = Playground { _sheep :: Float
                             , _cows :: Float
                             , _chicken :: Float
                             }

makeLenses ''Playground

equations = [ evar sheep + evar cows =:= 20
            , evar cows + evar chicken =:= 30
            , evar chicken + evar sheep =:= 40
            ]

solution = execState (solveLinear equations) (Playground 0 0 0)
```

This is a DSL and a solver for a system of linear equations which doesn't
require writing out matrices or massaging the data into a specific format. All
you need is a data type representing the solution space, a lens for every
variable and a set of equations.

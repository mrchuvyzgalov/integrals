# Functional programming final project
## Task: Computational mathematics
You can read about different computational methods for integrals [here](https://habr.com/ru/articles/420867/)

Minimum:
* (10 points) A library
* (10 points) The user choose the error and a definite integral as a Haskell function
* (30 points) The implementation of 3 different strategies
* (5 points) Error reporting
* (5 points) Unit tests

Additional tasks:
* (10 points) Console interface allowing the user to specify the integral without writing any Haskell code
* (10 points) Computation of the error and how many steps it took to get to the final result
* (10 points) Comparison of the methods
* (10 points) Property-based testing

## The architecture of the solution
### Data 
* **Error** - responsible for error types: **NsegIsNotPositive** (amount of segments <= 0), **ErrorIsNotPositive** (erro <= 0)
* **RectangleRule** - responsible for rectangle strategy type: **LeftPoint** (left rectangle rule), **RightPoint** (right rectangle rule), **MidPoint** (midpoint rectangle rule)
* **Strategy** - responsible for strategy type: **Rectangle** (rectangle strategy), **Trapezoid** (trapezoid strategy), **Simpson** (siimpson startegy)
* **Params** - responsible for computation integral params

## Computation intergal functions
* **compute'integral'by'rectangle'strategy** - responsible for computation integral by rectangle strategy
* **compute'integral'by'trapezoid'strategy'without'estimation** - responsible for computation integral by trapezoid strategy without estimation
* **compute'integral'by'trapezoid'strategy** - responsible for computation integral by trapezoid strategy
* **compute'integral'by'simpson'strategy** - responsible for computation integral by simpson strategy
* **compute'integral** - responsible for computation integral

The main function is **compute'integral**. You have to provide parameter (**Params**) and function (**Double -> Double**). The function compute integral by according strategy and then returns the answer (**Either Error Double**). If the result is computed, it is **Right Double**, otherwise it is **Left Error**

module TestFunctions where 


constantFunction :: Double -> Double
constantFunction _ = 5 

linearFunction :: Double -> Double 
linearFunction x = 2 * x + 3

quadraticFunction :: Double -> Double 
quadraticFunction x = (x ** 2) - 4 * x + 4

cubicFunction :: Double -> Double 
cubicFunction x = 2 * (x ** 3) - 3 * (x ** 2) + x - 5 

stepFunction :: Double -> Double 
stepFunction x = 3 * (x ** 4)

exponentialFunction :: Double -> Double 
exponentialFunction x = 2 ** x

logarithmicFunction :: Double -> Double 
logarithmicFunction x = log x / log 2

sinFunction :: Double -> Double 
sinFunction x = sin x

cosFunction :: Double -> Double 
cosFunction x = cos x

tanFunction :: Double -> Double 
tanFunction x = tan x

rationalFunction :: Double -> Double
rationalFunction x = (2 * (x ** 2) + 3 * x + 1) / (x - 1)

modularFunction :: Double -> Double 
modularFunction x = abs $ x - 3
module Params 
    ( RectangleRule(..), 
      Strategy(..),
      Params(..)
    ) where 

data RectangleRule = 
    LeftPoint | 
    RightPoint | 
    MidPoint 
    deriving (Eq)

data Strategy = 
    Rectangle { rule :: RectangleRule } | 
    Trapezoid { estError :: Double } | 
    Simpson { estError :: Double } 
    deriving (Eq)

data Params = Params { strategy :: Strategy, lowerLimit :: Double, upperLimit :: Double, nseg :: Int } deriving (Eq)

instance Show RectangleRule where
    show LeftPoint = "Left rectangle rule"
    show RightPoint = "Right rectangle rule"
    show MidPoint = "Midpoint rectangle rule"

instance Show Strategy where
    show Rectangle { rule = r } = "Rectangle strategy with " ++ show r
    show Trapezoid { estError = e } = "Trapezoid strategy with error=" ++ show e
    show Simpson { estError = e } = "Simpson strategy with error=" ++ show e

instance Show Params where 
    show Params { strategy = st , lowerLimit = ll , upperLimit = ul , nseg = ns } = "Integral computation with params { strategy=" ++ show st ++ ", lowerLimit=" ++ show ll ++ ", upperLimit=" ++ show ul ++ ", nseg=" ++ show ns ++ " }"
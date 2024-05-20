module Error ( Error(..) ) where

data Error = 
    NsegIsNotPositive { ns :: Int} |
    ErrorIsNotPositive { err :: Double }
    deriving (Eq)

instance Show Error where
    show NsegIsNotPositive { ns = ns' } = "The number of segments should be positive, but nseg = " ++ show ns'
    show ErrorIsNotPositive { err = e } = "The error should be positive, but error = " ++ show e
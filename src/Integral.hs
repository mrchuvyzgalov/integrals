module Integral (compute'integral) where

import Error ( Error(..) )
import Params ( Params(..), RectangleRule(..), Strategy(..) )
import Data.Maybe ( fromJust )
import Data.Either ( Either(..), fromRight )
import GHC.Float ( int2Double )

rectangleRuleFrac :: [(RectangleRule, Double)]
rectangleRuleFrac = [
    (LeftPoint, 0.0),
    (RightPoint, 1.0),
    (MidPoint, 0.5)]

compute'integral'by'rectangle'strategy :: Params -> (Double -> Double) -> Either Error Double
compute'integral'by'rectangle'strategy Params { strategy = Rectangle { rule = rule' }, lowerLimit = ll , upperLimit = ul , nseg = nseg' } f 
    | nseg' <= 0 = Left NsegIsNotPositive { ns = nseg' }
    | otherwise = let 
        dx = (ul - ll) / (int2Double nseg')
        frac = fromJust $ lookup rule' rectangleRuleFrac
        xstart = ll + frac * dx
        in Right $ (* dx) $ sum $ map f $ take nseg' $ [xstart, xstart + dx ..]  

compute'integral'by'trapezoid'strategy'without'estimation :: Params -> (Double -> Double ) ->  Double
compute'integral'by'trapezoid'strategy'without'estimation Params { lowerLimit = ll , upperLimit = ul , nseg = nseg' } f = let
        dx = (ul - ll) / (int2Double nseg')
        xstart = ll + dx 
        sum0 = 0.5 * (f ll + f ul)
        in (* dx) $ (+ sum0) $ sum $ map f $ take (nseg' - 1) $ [xstart, xstart + dx..]

compute'integral'by'trapezoid'strategy :: Params -> (Double -> Double) -> Either Error Double
compute'integral'by'trapezoid'strategy ps@(Params { strategy = Trapezoid { estError = error' }, nseg = nseg' }) f 
    | nseg' <= 0 = Left NsegIsNotPositive { ns = nseg' }
    | error' <= 0 = Left ErrorIsNotPositive { err = error' }
    | otherwise = let
        ans = compute'integral'by'trapezoid'strategy'without'estimation ps f
        errEst = max 1 $ abs ans
        in Right $ estimate errEst error' ps f ans
        where 
            estimate currErr err ps@(Params { nseg = nseg' } ) f ans
                | currErr <= abs (err * ans) = ans
                | otherwise = let 
                    newPs = ps { nseg = 2 * nseg'  }
                    newAns = compute'integral'by'trapezoid'strategy'without'estimation newPs f
                    newCurrErr = abs $ ans - newAns 
                    in estimate newCurrErr err newPs f newAns

compute'integral'by'simpson'strategy :: Params -> (Double -> Double) -> Either Error Double
compute'integral'by'simpson'strategy ps@(Params { strategy = Simpson { estError = error' }, nseg = nseg' }) f 
    | nseg' <= 0 = Left NsegIsNotPositive { ns = nseg' }
    | error' <= 0 = Left ErrorIsNotPositive { err = error' }
    | otherwise = let 
        oldTrapezSum = compute'integral'by'trapezoid'strategy'without'estimation ps f 
        newPs = ps { nseg = 2 * nseg' }
        newTrapezSum = compute'integral'by'trapezoid'strategy'without'estimation newPs f 
        ans = computeAns oldTrapezSum newTrapezSum
        errEst = max 1 $ abs ans
        in Right $ estimate errEst error' newPs newTrapezSum f ans
        where 
            estimate currErr err ps@(Params { nseg = nseg' } ) oldTrapezSum f ans
                | currErr <= abs (err * ans) = ans
                | otherwise = let 
                    newPs = ps { nseg = 2 * nseg' }
                    newTrapezSum = compute'integral'by'trapezoid'strategy'without'estimation newPs f 
                    newAns = computeAns oldTrapezSum newTrapezSum
                    newCurrErr = abs $ ans - newAns 
                    in estimate newCurrErr err newPs newTrapezSum f newAns

            computeAns oldTrapezSum newTrapezSum = (4.0 * newTrapezSum - oldTrapezSum) / 3.0

compute'integral :: Params -> (Double -> Double) -> Either Error Double
compute'integral ps@(Params { strategy = Rectangle {}} ) f = compute'integral'by'rectangle'strategy ps f 
compute'integral ps@(Params { strategy = Trapezoid {}} ) f = compute'integral'by'trapezoid'strategy ps f 
compute'integral ps@(Params { strategy = Simpson {}} ) f = compute'integral'by'simpson'strategy ps f 
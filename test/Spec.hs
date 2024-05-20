import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( (@?=), testCase, assertBool, assertFailure, Assertion )
import TestFunctions
import Integral (compute'integral)
import Params ( Params(..), Strategy(..), RectangleRule(..) )
import Data.Either ( Either(..) )
import Data.Either.Extra ( fromRight' )


assertApproxEqual :: Double -> Double -> Double -> Assertion
assertApproxEqual tolerance expected actual =
  assertBool ("Values " ++ show expected ++ " is not not equal to " ++ show actual) (abs (expected - actual) <= tolerance)
  
testRectangleStrategy :: TestTree 
testRectangleStrategy = testGroup "TestRectangleStrategy" [ testLeftPointRule, testRightPointRule, testMidPointRule ]
    where 
        testLeftPointRule = testGroup "TestLeftPointRule"
            [
                testCase "x = 5" $ assertApproxEqual' 15 (fromRight' $ compute'integral'with'params constantFunction),
                testCase "2 * x + 3" $ assertApproxEqual' 30 (fromRight' $ compute'integral'with'params linearFunction),
                testCase "x ^ 2 - 4 * x + 4" $ assertApproxEqual' 9 (fromRight' $ compute'integral'with'params quadraticFunction),
                testCase "2 * x^3 - 3 * x^2 + x - 5" $ assertApproxEqual' 183 (fromRight' $ compute'integral'with'params cubicFunction),
                testCase "3 * x^4" $ assertApproxEqual' 1855.8 (fromRight' $ compute'integral'with'params stepFunction),
                testCase "2^x" $ assertApproxEqual' 40.3 (fromRight' $ compute'integral'with'params exponentialFunction), 
                testCase "log2(x)" $ assertApproxEqual' 5.28 (fromRight' $ compute'integral'with'params logarithmicFunction),
                testCase "sin(x)" $ assertApproxEqual' (-0.69) (fromRight' $ compute'integral'with'params sinFunction),
                testCase "cos(x)" $ assertApproxEqual' (-1.86) (fromRight' $ compute'integral'with'params cosFunction),
                testCase "tan(x)" $ assertApproxEqual' (-28.9) (fromRight' $ compute'integral'with'params tanFunction), 
                testCase "(2 * x^2 + 3 * x + 1) / (x - 1)" $ assertApproxEqual' 44.31 (fromRight' $ compute'integral'with'params rationalFunction),
                testCase "|x - 3|" $ assertApproxEqual' 2.5 (fromRight' $ compute'integral'with'params modularFunction)
            ]
                where 
                    params = Params { strategy = Rectangle { rule = LeftPoint }, lowerLimit = 2.0, upperLimit = 5.0, nseg = 100000 }
                    compute'integral'with'params = compute'integral params
                    assertApproxEqual' = assertApproxEqual 0.1
        testRightPointRule = testGroup "TestRightPointRule"
            [
                testCase "x = 5" $ assertApproxEqual' 15 (fromRight' $ compute'integral'with'params constantFunction),
                testCase "2 * x + 3" $ assertApproxEqual' 30 (fromRight' $ compute'integral'with'params linearFunction),
                testCase "x ^ 2 - 4 * x + 4" $ assertApproxEqual' 9 (fromRight' $ compute'integral'with'params quadraticFunction),
                testCase "2 * x^3 - 3 * x^2 + x - 5" $ assertApproxEqual' 183 (fromRight' $ compute'integral'with'params cubicFunction),
                testCase "3 * x^4" $ assertApproxEqual' 1855.8 (fromRight' $ compute'integral'with'params stepFunction),
                testCase "2^x" $ assertApproxEqual' 40.3 (fromRight' $ compute'integral'with'params exponentialFunction),
                testCase "log2(x)" $ assertApproxEqual' 5.28 (fromRight' $ compute'integral'with'params logarithmicFunction),
                testCase "sin(x)" $ assertApproxEqual' (-0.69) (fromRight' $ compute'integral'with'params sinFunction),
                testCase "cos(x)" $ assertApproxEqual' (-1.86) (fromRight' $ compute'integral'with'params cosFunction),
                testCase "tan(x)" $ assertApproxEqual' (-28.9) (fromRight' $ compute'integral'with'params tanFunction),
                testCase "(2 * x^2 + 3 * x + 1) / (x - 1)" $ assertApproxEqual' 44.31 (fromRight' $ compute'integral'with'params rationalFunction),
                testCase "|x - 3|" $ assertApproxEqual' 2.5 (fromRight' $ compute'integral'with'params modularFunction)
            ]
                where 
                    params = Params { strategy = Rectangle { rule = RightPoint }, lowerLimit = 2.0, upperLimit = 5.0, nseg = 100000 }
                    compute'integral'with'params = compute'integral params
                    assertApproxEqual' = assertApproxEqual 0.1
        testMidPointRule = testGroup "TestMidPointRule"
            [
                testCase "x = 5" $ assertApproxEqual' 15 (fromRight' $ compute'integral'with'params constantFunction),
                testCase "2 * x + 3" $ assertApproxEqual' 30 (fromRight' $ compute'integral'with'params linearFunction),
                testCase "x ^ 2 - 4 * x + 4" $ assertApproxEqual' 9 (fromRight' $ compute'integral'with'params quadraticFunction),
                testCase "2 * x^3 - 3 * x^2 + x - 5" $ assertApproxEqual' 183 (fromRight' $ compute'integral'with'params cubicFunction),
                testCase "3 * x^4" $ assertApproxEqual' 1855.8 (fromRight' $ compute'integral'with'params stepFunction),
                testCase "2^x" $ assertApproxEqual' 40.3 (fromRight' $ compute'integral'with'params exponentialFunction),
                testCase "log2(x)" $ assertApproxEqual' 5.28 (fromRight' $ compute'integral'with'params logarithmicFunction),
                testCase "sin(x)" $ assertApproxEqual' (-0.69) (fromRight' $ compute'integral'with'params sinFunction),
                testCase "cos(x)" $ assertApproxEqual' (-1.86) (fromRight' $ compute'integral'with'params cosFunction),
                testCase "tan(x)" $ assertApproxEqual' 0.71 (fromRight' $ compute'integral'with'params tanFunction),
                testCase "(2 * x^2 + 3 * x + 1) / (x - 1)" $ assertApproxEqual' 44.31 (fromRight' $ compute'integral'with'params rationalFunction),
                testCase "|x - 3|" $ assertApproxEqual' 2.5 (fromRight' $ compute'integral'with'params modularFunction)
            ]
                where 
                    params = Params { strategy = Rectangle { rule = MidPoint }, lowerLimit = 2.0, upperLimit = 5.0, nseg = 100000 }
                    compute'integral'with'params = compute'integral params
                    assertApproxEqual' = assertApproxEqual 0.1
                    
testTrapezoidStrategy :: TestTree 
testTrapezoidStrategy = testGroup "TestTrapezoidStrategy" 
    [ 
        testCase "x = 5" $ assertApproxEqual' 15 (fromRight' $ compute'integral'with'params constantFunction),
        testCase "2 * x + 3" $ assertApproxEqual' 30 (fromRight' $ compute'integral'with'params linearFunction),
        testCase "x ^ 2 - 4 * x + 4" $ assertApproxEqual' 9 (fromRight' $ compute'integral'with'params quadraticFunction),
        testCase "2 * x^3 - 3 * x^2 + x - 5" $ assertApproxEqual' 183 (fromRight' $ compute'integral'with'params cubicFunction),
        testCase "3 * x^4" $ assertApproxEqual' 1855.8 (fromRight' $ compute'integral'with'params stepFunction),
        testCase "2^x" $ assertApproxEqual' 40.3 (fromRight' $ compute'integral'with'params exponentialFunction),
        testCase "log2(x)" $ assertApproxEqual' 5.28 (fromRight' $ compute'integral'with'params logarithmicFunction),
        testCase "sin(x)" $ assertApproxEqual' (-0.69) (fromRight' $ compute'integral'with'params sinFunction),
        testCase "cos(x)" $ assertApproxEqual' (-1.86) (fromRight' $ compute'integral'with'params cosFunction),
        testCase "(2 * x^2 + 3 * x + 1) / (x - 1)" $ assertApproxEqual' 44.31 (fromRight' $ compute'integral'with'params rationalFunction),
        testCase "|x - 3|" $ assertApproxEqual' 2.5 (fromRight' $ compute'integral'with'params modularFunction)
    ]
    where 
        tolerance = 0.1
        params = Params { strategy = Trapezoid { estError = tolerance }, lowerLimit = 2.0, upperLimit = 5.0, nseg = 100 }
        compute'integral'with'params = compute'integral params
        assertApproxEqual' = assertApproxEqual tolerance

testSimpsonStrategy :: TestTree 
testSimpsonStrategy = testGroup "TestSimpsonStrategy" 
    [ 
        testCase "x = 5" $ assertApproxEqual' 15 (fromRight' $ compute'integral'with'params constantFunction),
        testCase "2 * x + 3" $ assertApproxEqual' 30 (fromRight' $ compute'integral'with'params linearFunction),
        testCase "x ^ 2 - 4 * x + 4" $ assertApproxEqual' 9 (fromRight' $ compute'integral'with'params quadraticFunction),
        testCase "2 * x^3 - 3 * x^2 + x - 5" $ assertApproxEqual' 183 (fromRight' $ compute'integral'with'params cubicFunction),
        testCase "3 * x^4" $ assertApproxEqual' 1855.8 (fromRight' $ compute'integral'with'params stepFunction),
        testCase "2^x" $ assertApproxEqual' 40.3 (fromRight' $ compute'integral'with'params exponentialFunction),
        testCase "log2(x)" $ assertApproxEqual' 5.28 (fromRight' $ compute'integral'with'params logarithmicFunction),
        testCase "sin(x)" $ assertApproxEqual' (-0.69) (fromRight' $ compute'integral'with'params sinFunction),
        testCase "cos(x)" $ assertApproxEqual' (-1.86) (fromRight' $ compute'integral'with'params cosFunction),
        testCase "(2 * x^2 + 3 * x + 1) / (x - 1)" $ assertApproxEqual' 44.31 (fromRight' $ compute'integral'with'params rationalFunction),
        testCase "|x - 3|" $ assertApproxEqual' 2.5 (fromRight' $ compute'integral'with'params modularFunction)
    ]
    where 
        tolerance = 0.1
        params = Params { strategy = Simpson { estError = tolerance }, lowerLimit = 2.0, upperLimit = 5.0, nseg = 100 }
        compute'integral'with'params = compute'integral params
        assertApproxEqual' = assertApproxEqual tolerance
                    
testIntegralGroups :: TestTree
testIntegralGroups = testGroup "TestIntegral" [ testRectangleStrategy, testTrapezoidStrategy, testSimpsonStrategy ]

main :: IO ()
main = defaultMain testIntegralGroups

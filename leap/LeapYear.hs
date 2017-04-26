module LeapYear (isLeapYear) where

evenlyDivisible :: (Integral a) => a -> a -> Bool
evenlyDivisible n d = rem n d == 0

isLeapYear :: Integer -> Bool
isLeapYear year = evenlyDivisible year 4 &&
                  not (evenlyDivisible year 100 &&
                       not (evenlyDivisible year 400))
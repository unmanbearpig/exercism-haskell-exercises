module Clock (clockHour, clockMin, fromHourMin, toString) where

import Text.Printf (printf)

data Clock = Clock Int deriving (Show, Eq)

maxHour :: Int
maxHour = 24

maxMinute :: Int
maxMinute = 60

maxClockValue :: Int
maxClockValue = maxHour * maxMinute

wrapClock :: Integral a => a -> a
wrapClock x = x `mod` (fromIntegral maxClockValue)

fromInt :: Int -> Clock
fromInt = Clock . wrapClock

instance Num Clock where
  (+) (Clock x) (Clock y)  = fromInt $ x + y
  (*) _ _                  = error "How do you expect to multiply clocks?"
  fromInteger              = fromInt . fromIntegral
  abs                      = id
  signum (Clock 0)         = 0
  signum _                 = 1
  negate (Clock x)         = fromInt $ maxClockValue - x

clockHour :: Clock -> Int
clockHour (Clock c) = c `div` maxMinute

clockMin :: Clock -> Int
clockMin (Clock c) = c `mod` maxMinute

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = fromInt $ hour * maxMinute + min

toString :: Clock -> String
toString clock = printf "%02d:%02d" (clockHour clock) (clockMin clock)

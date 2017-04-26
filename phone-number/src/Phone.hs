module Phone (number) where

import qualified Data.Set as Set
import Data.Char (isNumber)
import Data.List (find)
import Control.Monad ((>=>))

-- - If the phone number is less than 10 digits assume that it is bad
--   number
-- - If the phone number is 10 digits assume that it is good
-- - If the phone number is 11 digits and the first number is 1, trim the 1
--   and use the last 10 digits
-- - If the phone number is 11 digits and the first number is not 1, then
--   it is a bad number
-- - If the phone number is more than 11 digits assume that it is a bad
--   number

meaninglessChars :: Set.Set Char
meaninglessChars = Set.fromList "() -."

number :: String -> Maybe String
number = validatePhoneNumber . filterChars meaninglessChars

filterChars :: Set.Set Char -> String -> String
filterChars _   [] = []
filterChars set xs = filter (\char -> Set.notMember char set) xs

validatePhoneNumber :: String -> Maybe String
validatePhoneNumber = validatePhoneNumberChars >=> validatePhoneNumberLength

validatePhoneNumberChars :: String -> Maybe String
validatePhoneNumberChars phoneNumber =
  case find (not . isNumber) phoneNumber of
    Nothing   -> Just phoneNumber
    otherwise -> Nothing

validatePhoneNumberLength :: String -> Maybe String
validatePhoneNumberLength phoneNumber =
  case length phoneNumber of
    10 -> Just phoneNumber
    11 -> if head phoneNumber == '1'
      then Just $ tail phoneNumber
      else Nothing
    otherwise -> Nothing

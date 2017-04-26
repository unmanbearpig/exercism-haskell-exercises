module Beer (song) where

import Text.Printf (printf)
import Data.Char (toUpper)
import Data.List (intercalate)

data PluralizableNoun = PluralizableNoun String String

plural :: PluralizableNoun -> String
plural (PluralizableNoun _ plural) = plural

singular :: PluralizableNoun -> String
singular (PluralizableNoun singular _) = singular

pluralize :: PluralizableNoun -> Int -> String
pluralize noun 1 = singular noun
pluralize noun _ = plural noun

pluralizeWithCount :: PluralizableNoun -> Int -> String
pluralizeWithCount noun n = show n ++ " " ++ pluralize noun n

bottle = PluralizableNoun "bottle" "bottles"
pronoun = PluralizableNoun "it" "one"

remainingBottles :: Int -> String
remainingBottles 0 = "no more bottles of beer"
remainingBottles n = printf "%s of beer" $ pluralizeWithCount bottle n

clarify :: String -> String
clarify s = s ++ " on the wall"

capitalize :: String -> String
capitalize (x:xs) = (toUpper x):xs

observation :: Int -> String
observation n = printf "%s, %s." (capitalize $ clarify bs) bs
  where bs = remainingBottles n

encouragement :: Int -> String
encouragement 0 = printf "Go to the store and buy some more, %s."
                  (clarify $ remainingBottles 99)
encouragement n = printf "Take %s down and pass it around, %s."
                  (pluralize pronoun n) (clarify $ remainingBottles $ n - 1)

verse :: Int -> String
verse n = unlines [(observation n), (encouragement n)]

song :: String
song = intercalate "\n" $ map verse [99,98..0]

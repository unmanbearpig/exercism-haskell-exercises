module House (rhyme) where

import Data.List

data Character = Character { name :: String, activity :: String }

characters = [
  (Character "horse and the hound and the horn" "belonged to")
  , (Character "farmer sowing his corn" "kept")
  , (Character "rooster that crowed in the morn" "woke")
  , (Character "priest all shaven and shorn" "married")
  , (Character "man all tattered and torn" "kissed")
  , (Character "maiden all forlorn" "milked")
  , (Character "cow with the crumpled horn" "tossed")
  , (Character "dog" "worried")
  , (Character "cat" "killed")
  , (Character "rat" "ate")
  , (Character "malt" "lay in")
  , (Character "house that Jack built." "") ]

introduce :: Character -> String
introduce c = "This is the " ++ name c

actsOn :: Character -> Character -> String
actsOn subject object = "that " ++ activity subject ++ " the " ++ name object

actions :: [Character] -> [String]
actions cs = map (uncurry actsOn) $ zip cs (drop 1 cs)

chars :: Int -> [Character]
chars n = drop (length characters - n) characters

verse :: [Character] -> String
verse cs@(h:_) = intercalate "\n" $ (introduce h):(actions cs)

rhyme :: String
rhyme = (intercalate "\n\n" $ map (verse . chars) [1..12]) ++ "\n"

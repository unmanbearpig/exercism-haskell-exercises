module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Function (on)
import System.Random (randomIO)

caesarDecode :: String -> String -> String
caesarDecode key encodedText = zipWith (encodeChar (-)) encodedText (cycle key)

caesarEncode :: String -> String -> String
caesarEncode key text = zipWith (encodeChar (+)) text (cycle key)

encodeChar :: (Int -> Int -> Int) -> Char -> Char -> Char
encodeChar op key t = toLetterChar (on op letterIndex key t)

letterIndex :: Char -> Int
letterIndex c = (fromEnum c) - 97

toLetterChar :: Int -> Char
toLetterChar c = toEnum ((c `mod` 26) + 97)

randomKey :: Int -> IO String
randomKey length = sequence $ take length $ cycle [(toLetterChar <$> randomIO)]

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = (\key -> (key, caesarEncode key text)) <$> randomKey keyLength
  where keyLength = 256

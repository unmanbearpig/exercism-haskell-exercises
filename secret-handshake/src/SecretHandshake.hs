module SecretHandshake (handshake) where

import Data.Maybe (catMaybes)
import Data.Bits

handshake :: Int -> [String]
handshake x = if testBit x 4 then reverse actions else actions
  where actions = catMaybes $
          (if testBit x 0 then Just "wink" else Nothing) :
          (if testBit x 1 then Just "double blink" else Nothing) :
          (if testBit x 2 then Just "close your eyes" else Nothing) :
          (if testBit x 3 then Just "jump" else Nothing) : []

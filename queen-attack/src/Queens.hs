module Queens (boardString, canAttack) where

import Data.Array
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import Data.Function (on)

data Piece = White | Black
type Coord = (Int, Int)
data Board = Board Int (Array Coord (Maybe Piece))

renderPiece :: Piece -> String
renderPiece p = case p of
                  White -> "W"
                  Black -> "B"

renderMaybePiece :: Maybe Piece -> String
renderMaybePiece Nothing = "_"
renderMaybePiece (Just p) = renderPiece p

initBoard :: Int -> Board
initBoard size = Board size $ array ((0, 0), (size - 1, size - 1)) [((x, y), Nothing) | x <- [0..size - 1], y <- [0.. size - 1]]

addPieces :: Board -> [(Coord, Maybe Piece)] -> Board
addPieces (Board size arr) ps = Board size $ arr // ps

boardString :: Maybe Coord -> Maybe Coord -> String
boardString whiteCoord blackCoord = boardString' $ initBoard 8 `addPieces` pieces
  where pieces = catMaybes $ uncurry mkPiece <$> [(White, whiteCoord), (Black, blackCoord)]
        mkPiece p = fmap $ \c -> (c, Just p)

boardString' :: Board -> String
boardString' (Board size arr) = unlines $ map unwords $ chunksOf size $ renderMaybePiece <$> elems arr

canRookAttack :: Coord -> Coord -> Bool
canRookAttack (x1, y1) (x2, y2)
  | x1 == x2 || y1 == y2 = True
  | otherwise = False

canBishopAttack :: Coord -> Coord -> Bool
canBishopAttack a b = ((==) `on` d1) a b
                   || ((==) `on` d2) a b
  where d1 = uncurry (+)
        d2 = abs . uncurry (-)

canQueenAttack :: Coord -> Coord -> Bool
canQueenAttack a b = canRookAttack a b || canBishopAttack a b

canAttack :: Coord -> Coord -> Bool
canAttack = canQueenAttack

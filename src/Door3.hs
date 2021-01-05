module Door3 where

import Door
import Util

door3 :: Door
door3 = Door {
    no = 3,
    files = [
        "example.txt",
        "mypuzzle.txt"
    ],
    runDoor = \file ->
        let
            slopes = [
                Slope 1 1,
                Slope 3 1,
                Slope 5 1,
                Slope 7 1,
                Slope 1 2
                ]
            map = createMap file
            in
                do
                    x <- foldl1 (applyBinaryOperatorToMonad (*)) (countTreesAtSlope map <$> slopes)
                    putStrLn $ "Product of all tree occurences: " ++ show x
}

countTreesAtSlope :: Map -> Slope -> IO Int
countTreesAtSlope map slope@(Slope x y) =
    let trees = getPassedTrees map slope in
    do
        putStrLn $ "Trees passed on slope (Right " ++ show x ++ " Down " ++ show y ++ "): " ++ show trees
        return trees

data Tile = Empty | Tree
type Map = [[Tile]]
data Slope = Slope Int Int

instance Show Tile where
    show Empty = "."
    show Tree = "#"

toTile :: Char -> Tile
toTile '#' = Tree
toTile _ = Empty -- '.'

isTree :: Tile -> Int
isTree Tree = 1
isTree Empty = 0

widthOf :: Map -> Int
widthOf [[]] = 0
widthOf (x:xs) = length x

heightOf :: Map -> Int
heightOf = length

tileAt :: Map -> Int -> Int -> Tile
tileAt map x y = (map !! y) !! mod x (widthOf map)

createMap :: String -> Map
createMap file = createRow <$> lines file

createRow :: String -> [Tile]
createRow line = toTile <$> line

getPassedTrees :: Map -> Slope -> Int
getPassedTrees map slope@(Slope x y) = getPassedTreesStartingFrom x y map slope -- first tile is always empty so we move one step already

getPassedTreesStartingFrom :: Int -> Int -> Map -> Slope -> Int
getPassedTreesStartingFrom x y map slope@(Slope dx dy)
    | y < heightOf map =
        isTree (tileAt map x y)
        + getPassedTreesStartingFrom (x + dx) (y + dy) map slope
    | otherwise = 0

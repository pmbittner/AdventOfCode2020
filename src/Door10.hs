module Door10 where

import Door
import Util

import Data.List

door10 :: Door
door10 = Door {
    no = 10,
    files = [
        "example.txt",
        "example2.txt",
        "mypuzzle.txt"
    ],
    runDoor = door10Main
}

data DifferenceRecord = DR {
    lastAdapter :: Int,
    differenceCount :: Int -> Int
}

mempty :: DifferenceRecord
mempty = DR { lastAdapter = 0, differenceCount = const 0}

parseAdapters :: String -> [Int]
parseAdapters = fmap read . words

prettyPrintJoltageDifference :: Int -> Int -> [Char]
prettyPrintJoltageDifference var val =
    "There are "
    <> show val
    <> " differences of "
    <> show var
    <> " jolt."

door10Main :: String -> IO ()
door10Main text = do
    let givenAdapters = sort $ parseAdapters text
        allAdapters = givenAdapters ++ [3 + last givenAdapters] -- add internal adapter rated for 3 higher than the largest given one
        differenceCounter = \lastDifferenceRecord nextAdapter ->
            DR {
                lastAdapter = nextAdapter,
                differenceCount = \i ->
                    (if i == nextAdapter - lastAdapter lastDifferenceRecord then 1 else 0)
                    + differenceCount lastDifferenceRecord i
            }
        differences = differenceCount $ foldl differenceCounter Door10.mempty allAdapters
        amountOfDifference1 = differences 1
        amountOfDifference3 = differences 3
    putStrLn $ prettyPrintJoltageDifference 1 amountOfDifference1
    putStrLn $ prettyPrintJoltageDifference 3 amountOfDifference3
    putStrLn $
        show amountOfDifference1
        <> " * "
        <> show amountOfDifference3
        <> " == "
        <> show (amountOfDifference1 * amountOfDifference3)
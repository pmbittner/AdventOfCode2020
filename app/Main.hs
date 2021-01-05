module Main where

import Door
import Door1
import Door2
import Door3

main :: IO ()
main =
    let doors = [
            door1,
            door2,
            door3
          ] in
    mconcat $ runDoorOnAllItsFiles <$> doors

runDoorOnAllItsFiles :: Door -> IO()
runDoorOnAllItsFiles door = do
    putStrLn $ "=== Running Door " ++ show (no door) ++ " ==="
    foldl1 (\a b -> a <> putStrLn "" <> b) (runDoorOnFile door <$> files door)
    putStrLn "=======================\n"

runDoorOnFile :: Door -> String -> IO ()
runDoorOnFile door file =
    let path = "resources/Door" ++ show (no door) ++ "/" ++ file in
    do
        putStrLn $ "Input: " ++ path
        fileContents <- readFile path
        runDoor door fileContents
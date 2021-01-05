module Main where

import Door
import Door1
import Door2

main :: IO ()
main =
    let doors = [
            -- door1,
            door2
          ] in
    mconcat $ runDoor <$> doors

runDoor :: Door -> IO()
runDoor d = do
    putStrLn $ "=== Running Door " ++ show (no d) ++ " ==="
    run d
    putStrLn "=======================\n"

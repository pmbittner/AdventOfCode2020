module Main where

import Door
import Door1

main :: IO ()
main =
    let doors = [door1] in
    mconcat $ runDoor <$> doors

runDoor :: Door -> IO()
runDoor d = do
    putStrLn $ "=== Running Door " ++ (show $ no d) ++ " ==="
    run d
    putStrLn $ "=======================\n"

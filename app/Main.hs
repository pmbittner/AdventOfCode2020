module Main where

import Day
import Day1

main :: IO ()
main =
    let days = [day1] in
    mconcat $ runDay <$> days

runDay :: Day -> IO()
runDay d = do
    putStrLn $ "=== Running Day " ++ (show $ no d) ++ " ==="
    run d
    putStrLn $ "======================\n"

module Door where

data Door = Door {
    no :: Int, -- The number of the Door
    files :: [String], -- List of files that should be processed
    runDoor :: String -> IO() -- functions to run this puzzle's solution on the given text
}

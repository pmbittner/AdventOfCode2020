module Door where

type File = String
data Door = Door {
    no :: Int, -- The number of the Door
    files :: [File], -- List of files that should be processed
    runDoor :: File -> IO() -- functions to run this puzzle's solution on the given text
}

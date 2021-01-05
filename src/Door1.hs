module Door1 where

import Door

door1 :: Door
door1 = Door {
    no = 1,
    files = [
        "example.txt",
        "mypuzzle.txt"
    ],
    runDoor = runOnFile 2020
}

runOnFile :: Int -> String -> IO ()
runOnFile sum text = do
    -- putStrLn text
    let expenses = parseExpenses text
    putStrLn $ "  Product of two numbers that sum to " ++ show sum
    putStrLn $ "    " ++ show (productOfTheTwoNumbersThatSumTo sum expenses)
    putStrLn $ "  Product of three numbers that sum to " ++ show sum
    putStrLn $ "    " ++ show (productOfTheThreeNumbersThatSumTo sum expenses)

parseExpenses :: String -> [Int]
parseExpenses s = read <$> words s

productOfTheTwoNumbersThatSumTo :: Int -> [Int] -> Maybe Int
productOfTheTwoNumbersThatSumTo sum [] = Nothing
productOfTheTwoNumbersThatSumTo sum (x:xs) = 
    let y = sum - x in
    if elem y xs
    then Just (x * y)
    else productOfTheTwoNumbersThatSumTo sum xs

productOfTheThreeNumbersThatSumTo :: Int -> [Int] -> Maybe Int
productOfTheThreeNumbersThatSumTo sum [] = Nothing
productOfTheThreeNumbersThatSumTo sum (x:xs) =
    let y = sum - x in
    case productOfTheTwoNumbersThatSumTo y xs of
        Nothing -> productOfTheThreeNumbersThatSumTo sum xs
        Just z -> Just (x * z)

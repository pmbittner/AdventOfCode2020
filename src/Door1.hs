module Door1 where

import Door

door1 :: Door
door1 = Door {
    no = 1,
    run = do
        runOnFile "resources/Door1/example.txt" 2020
        runOnFile "resources/Door1/mypuzzle.txt" 2020
}

runOnFile :: String -> Int -> IO ()
runOnFile file sum = do
    example <- readFile file
    putStrLn $ "Input: " ++ file
    -- putStrLn example
    let expenses = parseExpenses example
    putStrLn $ "  Product of two numbers that sum to "++show sum
    putStrLn $ "    " ++ show (productOfTheTwoNumbersThatSumTo sum expenses)
    putStrLn $ "  Product of three numbers that sum to "++show sum
    putStrLn $ "    " ++ show (productOfTheThreeNumbersThatSumTo sum expenses)
    putStrLn ""

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

module Door2 where

import Door
import Util

import Data.List.Split
import Data.List
import Text.Read

door2 :: Door
door2 = Door {
    no = 2,
    files = [
        "example.txt",
        "mypuzzle.txt"
    ],
    runDoor = checkPasswordsIn
}

data PasswordConstraint = PasswordConstraint {
    char :: Char,
    minOccurences :: Int,
    maxOccurences :: Int
}

instance Show PasswordConstraint where
    show pwc = show (minOccurences pwc) ++ "-" ++ show (maxOccurences pwc) ++ " " ++ [char pwc]

checkPasswordsIn :: String -> IO ()
checkPasswordsIn fileContents =
    let pwlines = lines fileContents in
    do
        putStrLn $ sayHowManyPasswordsAreValid "Sled Rental" pwlines isValidSledRental
        putStrLn $ sayHowManyPasswordsAreValid "Toboggan Rental" pwlines isValidTobogganRental

sayHowManyPasswordsAreValid :: String -> [String] -> (PasswordConstraint -> String -> Bool) -> String
sayHowManyPasswordsAreValid name pwlines validator =
    "  Number of valid passwords according to " ++ name ++ ": " ++ show
    (countMatches pwlines (checkPasswordLine validator))
    -- (intercalate ", " ((\s -> s ++ " -> " ++ show (checkPasswordLine validator s)) <$> pwlines))

checkPasswordLine :: (PasswordConstraint -> String -> Bool) -> String -> Bool
checkPasswordLine f line =
    let (pwconstraint, text) = parseLine line in
        case pwconstraint of
            Just c -> f c text
            Nothing -> error $ "Could not parse password constraint in line: " ++ line

parseLine :: String -> (Maybe PasswordConstraint, String)
parseLine line = 
    let constraintAndText = trim <$> splitOn ":" line -- constraintAndText is list of length 2
        pwconstraint = parsePasswordConstraint $ head constraintAndText in
        (pwconstraint, head $ tail constraintAndText)

parsePasswordConstraint :: String -> Maybe PasswordConstraint
parsePasswordConstraint text =
    let (minStr, rest1) = splitAtFirst text (=='-')
        (maxStr, charStr) = splitAtFirst (tail rest1 {-drop the minus-}) (==' ')
        in
            case (readMaybe minStr, readMaybe maxStr) of
                (Just min, Just max) -> Just $ PasswordConstraint {
                    minOccurences = min,
                    maxOccurences = max,
                    char = head $ trim charStr
                }
                _ -> Nothing

isValidSledRental :: PasswordConstraint -> String -> Bool
isValidSledRental constraint pw =
    let charCount = countMatches pw (== char constraint) in
        minOccurences constraint <= charCount && charCount <= maxOccurences constraint

isValidTobogganRental :: PasswordConstraint -> String -> Bool
isValidTobogganRental constraint pw =
    let c = char constraint in
        xor [pw !! (minOccurences constraint - 1) == c, pw !! (maxOccurences constraint - 1) == c]
 
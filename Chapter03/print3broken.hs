module Print3Broken where

-- Uncomment this line to fix
greeting = "Yarrrrr"

printSecond :: IO ()
printSecond = putStrLn greeting

main :: IO ()
main = do
    putStrLn greeting
    printSecond
    where greeting = "Yarrrrr"

module Chapter03.Reverse where

reverse' :: String -> String
reverse' s = drop 9 s ++ take 4 (drop 5 s) ++ take 5 s

main :: IO ()
main = print $ reverse' "Curry is awesome"

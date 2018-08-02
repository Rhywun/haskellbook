module Chapter02.Test where

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

main = print [(n, 2 ^ n) | n <- [0 .. 19]]

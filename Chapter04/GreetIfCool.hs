module GreetIfCool where

greetIfCool1 :: String -> IO ()
greetIfCool1 coolness =
    if cool
        then putStrLn "eyyyyy. What's shakin'?"
        else putStrLn "pshhhh."
    where cool = coolness == "downright frosty yo"

greetIfCool2 :: String -> IO ()
greetIfCool2 coolness =
    if cool coolness
        then putStrLn "eyyyyy. What's shakin'?"
        else putStrLn "pshhhh."
    where cool v = v == "downright frosty yo"

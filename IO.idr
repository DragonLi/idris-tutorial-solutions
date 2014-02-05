module Main

main : IO ()
main = putStrLn "Hello to Idris!"

-- example of do notation

greet : IO ()
greet = do putStrLn "What is your name?"
           name <- getLine
           putStrLn ("Hello " ++ name)
  

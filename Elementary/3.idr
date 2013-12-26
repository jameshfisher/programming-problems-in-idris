-- Modify the previous program such that only the users Alice and Bob are greeted with their names.

module Main

import Helpers

main : IO ()
main = do
  name <- prompt "What's your name?: "
  putStrLn $ if name == "Alice" || name == "Bob"
    then "Hello, " ++ name
    else "Sorry, only Alice and Bob are allowed in."

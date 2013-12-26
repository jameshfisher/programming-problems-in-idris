-- Write a program that asks the user for his name and greets him with his name.

module Problems.Elementary.Main

import Problems.Helpers

main : IO ()
main = prompt "What's your name?: " >>= putStrLn . ("Hello, " ++)

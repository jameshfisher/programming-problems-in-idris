-- Write a guessing game where the user has to guess a secret number.
-- After every guess the program tells the user whether his number was too large or too small.
-- At the end the number of tries needed should be printed.
-- I counts only as one try if the user inputs the same number consecutively.

module Main

import Helpers

random_number : IO Nat
random_number = return 42 -- hmm

play : Nat -> IO ()
play secret = go 0 where
  go : Nat -> IO ()
  go tries = do
    guess <- promptNat "Guess: "
    case (compare guess secret) of
      EQ => putStrLn $ "Correct, in only " ++ (show (S tries)) ++ " tries!"
      LT => putStrLn ("Too small.") >>> go (S tries)
      GT => putStrLn ("Too large.") >>> go (S tries)

main : IO ()
main = random_number >>= play

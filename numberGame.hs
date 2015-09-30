import System.Random
import Data.Maybe

main = do
   randomNumberGenerator <- getStdGen
   let winningNumber = head (randomRs (1, 100) randomNumberGenerator :: [Int])
   playGame winningNumber

playGame :: Int -> IO ()
playGame winningNumber = do
   putStrLn "Instructions"
   userString <- getLine
   let parsedString = reads userString :: [(Int, String)]
   getOutputFromUserString winningNumber parsedString


getOutputFromUserString :: Int -> [(Int, String)] -> IO ()
getOutputFromUserString winningNumber [(userNumber, unparseable)] 
   | null unparseable = do
      let output = getOutputFromUserNumber userNumber winningNumber
      if Nothing == output
         then putStrLn "Congratulations! You guessed the correct number."
         else do
            putStrLn $ Data.Maybe.fromJust output
            main
   | otherwise = throwUnparseableError winningNumber
getOutputFromUserString winningNumber _ = do throwUnparseableError winningNumber

throwUnparseableError winningNumber = do
   putStrLn "Please enter only numbers."
   playGame winningNumber

getOutputFromUserNumber :: (Ord a) => a -> a -> Maybe String
getOutputFromUserNumber userNumber winningNumber
   | userNumber < winningNumber = Just "Too low"
   | userNumber > winningNumber = Just "Too high"
   | otherwise = Nothing

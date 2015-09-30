import System.Random

main = do
   randomNumberGenerator <- getStdGen
   let winningNumber = head (randomRs (1, 10) randomNumberGenerator :: [Int])
   playGame winningNumber

playGame winningNumber = do
   putStrLn "I've chosen a random number between 1 and 100. See if you can guess it!"
   runRound winningNumber
 
runRound winningNumber = do
   userString <- getLine
   let parsedString = reads userString :: [(Int, String)]
   let (outputString, nextAction) = processNumber parsedString winningNumber 
   putStrLn outputString
   nextAction

processNumber [(userNumber, [])] winningNumber = outputFromComparingNumbers userNumber winningNumber 
processNumber _ winningNumber = ("Please enter only numbers", runRound winningNumber)

outputFromComparingNumbers userNumber winningNumber
   | userNumber < winningNumber = ("Too low", runRound winningNumber)
   | userNumber > winningNumber = ("Too high", runRound winningNumber)
   | otherwise = ("Congratulations! You guessed it!", return ())

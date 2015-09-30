import System.Random

main = do
   randomNumberGenerator <- getStdGen
   runRound (head (randomRs (1, 10) randomNumberGenerator :: [Int])) "I've chosen a random number between 1 and 100. See if you can guess it!"

runRound winningNumber stringToPrint = do
   putStrLn stringToPrint
   userString <- getLine
   let (outputString, nextAction) = processNumber (reads userString :: [(Int, String)]) winningNumber 
   nextAction outputString

processNumber [(userNumber, [])] winningNumber = outputFromComparingNumbers userNumber winningNumber 
processNumber _ winningNumber = ("Please enter only numbers", runRound winningNumber)

outputFromComparingNumbers userNumber winningNumber
   | userNumber < winningNumber = ("Too low", runRound winningNumber)
   | userNumber > winningNumber = ("Too high", runRound winningNumber)
   | otherwise = (show winningNumber, (\winningNumberString -> putStrLn $ "Congratulations! The number was " ++ winningNumberString ++ "! Like a boss!"))

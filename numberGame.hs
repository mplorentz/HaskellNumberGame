import System.Random
import Data.Maybe
import Control.Monad   

main = do
   randomNumberGenerator <- getStdGen
   let winningNumber = head (randomRs (1, 10) randomNumberGenerator :: [Int])
   playGame winningNumber

playGame :: Int -> IO ()
playGame winningNumber = do
   putStrLn "I've chosen a random number between 1 and 100. See if you can guess it!"
   runRound winningNumber
 
runRound winningNumber = do
   userString <- getLine
   let parsedString = reads userString :: [(Int, String)]
   let (outputString, nextAction) = processNumber parsedString winningNumber 
   case outputString of 
      Just output -> do
         putStrLn output
         nextAction
      _ -> nextAction

processNumber :: [(Int, String)] -> Int -> (Maybe String, IO ())
processNumber [(userNumber, unparseable)] winningNumber
   | null unparseable   = outputFromComparingNumbers userNumber winningNumber
   | otherwise          = (Just "Please enter only numbers", runRound winningNumber)

outputFromComparingNumbers :: Int -> Int -> (Maybe String, IO ())
outputFromComparingNumbers userNumber winningNumber
   | userNumber < winningNumber = (Just "Too low", runRound winningNumber)
   | userNumber > winningNumber = (Just "Too high", runRound winningNumber)
   | otherwise = (Nothing, putStrLn "Congratulations! You guessed it!")

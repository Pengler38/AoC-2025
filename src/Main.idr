module Main

import System
import System.File
import Data.String
import Data.Vect

import Day1

%default total

solutions : Vect ? (String -> String)
solutions = [
  day1
]

defaultFiles : Vect (length Main.solutions) String
defaultFiles = [
  "data/day1.txt"
]

data Error = InvalidDay String | InvalidCommand String | InvalidFile String FileError

implementation Show Error where
  show (InvalidDay n) = "Day " ++ show n ++  " does not exist"
  show (InvalidCommand s) = "Command is invalid: " ++ s
  show (InvalidFile path e) = show e ++ ": " ++ path

||| Utility function, for use with a showable Left in an IO context
unwrapInto : Show e => Either e a -> Lazy (a -> IO ()) -> IO ()
e `unwrapInto` f = either (putStrLn . show) f e

||| Attempts to parse s into a Fin which can safely index solutions
parseDay : String -> Either Error (Fin ?)
parseDay s = 
  let fin : Maybe (Fin ?)
      fin = do
        i <- parseInteger s
        integerToFin (i - 1) (length solutions)
  in maybe (Left $ InvalidDay s) Right fin

covering
getFile : String -> IO (Either Error String)
getFile s = mapFst (InvalidFile s) <$> readFile s

||| Runs the solution of day n on the input string
solve : (Fin ?) -> String -> String
solve n = index n solutions


covering
runCommand : String -> IO ()
runCommand command = 
  let runSolution : String -> Maybe String -> IO ()
      runSolution sDay mFile =
        (parseDay sDay) `unwrapInto` \day => do
          let file = maybe (index day defaultFiles) id mFile
          eInput <- getFile file
          eInput `unwrapInto` \input => do
            putStrLn $ "Running day " ++ sDay ++ " on " ++ file ++ "..."
            putStrLn $ "\nResult:\n" ++ solve day input
  in case words command of
    day::rest => runSolution day (head' rest)
    _ => putStrLn $ show $ InvalidCommand command



intro : String
intro = """
  Usage:
    <day>
    <day> <input file>
  Enter command: 
  """

covering
main : IO ()
main = do
  args <- getArgs
  case args of
    _::a::b => runCommand (unwords (a::b))
    _ => do
      putStr intro
      input <- getLine
      runCommand input


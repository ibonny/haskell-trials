import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import qualified Data.Maybe
import System.Console.ANSI (getTerminalSize)
import System.Environment (getArgs)
import Text.Printf (printf)

data AppState = AppState
  { num :: Int,
    str :: String,
    flag :: Bool
  }
  deriving (Show)

type App = StateT AppState IO

compute :: App String
compute = do
  state <- get
  return $
    "Num: "
      ++ show (num state)
      ++ ", String: "
      ++ show (str state)
      ++ ", Flag: "
      ++ show (flag state)

updateString :: String -> App ()
updateString newStr = do
  modify (\s -> s {str = newStr})

toBool :: String -> Bool
toBool str = str `elem` ["true", "1", "yes", "y"]

someFunction :: Bool -> String
someFunction True = "Some first value"
someFunction False = "Some default value"

terminalSize :: IO (Int, Int)
terminalSize = do
  termSize <- getTerminalSize

  let (height, width) = case termSize of
        Just (x, y) -> (x, y)
        Nothing -> (0, 0)

  return (height, width)

dimensions :: Int -> Int -> Int
dimensions height width = height * width

countWithState :: App ()
countWithState = do
  state <- get
  let currentStr = str state
  let newStr = case currentStr of
        "one" -> "two"
        "two" -> "three"
        "three" -> "four"
        "four" -> "five"
        "five" -> "six"
        "six" -> "seven"
        "seven" -> "eight"
        "eight" -> "nine"
        "nine" -> "ten"
  modify (\s -> s {str = newStr})

loopState :: App ()
loopState = do
  state <- get
  liftIO $ print state
  let currentStr = str state
  if currentStr == "ten"
    then return ()
    else do
      countWithState
      loopState

main :: IO ()
main = do
  (height, width) <- terminalSize

  printf "Terminal size is: %dx%d\n" height width

  printf "Dimensions of the terminal are: %d\n" (dimensions height width)

  let initialState = AppState 10 "hello" True

  -- let (_, middleState) = runState (updateString "Some new string") initialState

  (_, middleState) <- runStateT (modify (\s -> s {str = "Some new string"})) initialState

  (result, _) <- runStateT compute middleState

  ignored <- execStateT loopState (AppState 10 "one" True)

  putStr ""

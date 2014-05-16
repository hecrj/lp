import System.IO


-- Useful functions
each :: (Int -> a -> b) -> [a] -> [b]
each f = zipWith f [0..]

-- intercalates the first argument with the elements of the second argument
intercalate :: Show s => String -> [s] -> String
intercalate delim [] = ""
intercalate delim [x] = show x
intercalate delim (x:xs) = show x ++ delim ++ intercalate delim xs

formatOption :: Int -> String -> String
formatOption index str = show (index+1) ++ ". " ++ str ++ "\n"

prompt :: String -> IO String
prompt message = do
  putStr $ message ++ ": "
  hFlush stdout
  getLine


promptInt :: String -> Int -> Int -> IO Int
promptInt msg start end = do
  line <- prompt ("=> " ++ msg ++ " [" ++ show start ++ ".." ++ show end ++ "]")
  let num = read line
  if num `elem` [start..end]
    then return num
    else promptInt msg start end

selectOption :: [String] -> IO Int
selectOption options = do
  let optionStr = foldr1 (++) $ each formatOption options
  putStr optionStr
  promptInt "Select option" 1 $ length options


-- Game types
class Game g where
  name :: g -> String
  play :: g -> IO ()

data GameType = UserVsCPU | CPUvsCPU

instance Game GameType where
  name UserVsCPU = "User vs CPU"
  name CPUvsCPU = "CPU vs CPU"
  play g = do
    board <- readBoard
    putStrLn $ "Playing " ++ name g ++ "..."
    draw board


-- Board
data Cell = Empty | S | O

instance Show Cell where
  show Empty = " "
  show S = "S"
  show O = "O"

data Board = Board { rows :: Int, cols :: Int, content :: [[Cell]]}

readBoard :: IO Board
readBoard = do
  rows <- promptInt "Enter number of rows" 1 50
  cols <- promptInt "Enter number of columns" 1 50
  return $ newBoard rows cols

newBoard :: Int -> Int -> Board
newBoard rows cols = Board rows cols [take cols $ repeat Empty | _ <- [1..rows]]

draw :: Board -> IO ()
draw (Board _ _ content) = drawContent content

drawContent :: [[Cell]] -> IO ()
drawContent content = do
  putStrLn $ separate $ map (\x -> " " ++ x ++ " ") $ map (intercalate " | ") content

separate :: [String] -> String
separate [] = ""
separate [x] = x
separate (x:xs) = x ++ "\n" ++ (replicate (length x) '-') ++ "\n" ++ separate xs


-- Main
main = do
  putStrLn " ___  ___  ___ _       _ _ "
  putStrLn "/ __|/ _ \\/ __| |_____| | |"
  putStrLn "\\__ \\ (_) \\__ \\ / / -_) | |"
  putStrLn "|___/\\___/|___/_\\_\\___|_|_|"
  putStrLn ""
  putStrLn "Welcome! Select the type of game:"

  let gameTypes = [UserVsCPU, CPUvsCPU]
  selected <- selectOption $ map name gameTypes
  play $ gameTypes!!(selected-1)

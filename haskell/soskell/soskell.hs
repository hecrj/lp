import System.IO
import System.Random


-- Useful functions
each :: (Int -> a -> b) -> [a] -> [b]
each f = zipWith f [0..]

mapSelect :: (a -> a) -> Int -> [a] -> [a]
mapSelect f1 selected = each (ifSelected f1 selected)
  where
    ifSelected f1 selected current
      | selected == current = f1
      | otherwise = id

maximumBy :: Ord b => (a -> b) -> [a] -> a
maximumBy f (x:xs) = maximumBy' f xs x $ f x
  where
    maximumBy' f [] max _ = max
    maximumBy' f (x:xs) max currVal
      | newVal > currVal = maximumBy' f xs x newVal
      | otherwise = maximumBy' f xs max currVal
      where
        newVal = f x

intercalate :: Show s => String -> [s] -> String
intercalate delim [] = ""
intercalate delim [x] = show x
intercalate delim (x:xs) = show x ++ delim ++ intercalate delim xs

join :: String -> [String] -> String
join glue [x,y] = x ++ glue ++ y

separate :: [String] -> String
separate [] = ""
separate [x] = x
separate (x:xs) = x ++ "\n" ++ (replicate (length x) '-') ++ "\n" ++ separate xs

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

putOptions :: Show s => [s] -> IO ()
putOptions options = do
  let optionStr = foldr (++) "" $ each formatOption $ map show options
  putStr optionStr

selectOption :: Show s => [s] -> IO s
selectOption options = do
  putOptions options
  selected <- promptInt "Select option" 1 $ length options
  return $ options!!(selected-1)


-- Game types
data GameType = GameType { description :: String, factory :: GameFactory }
type GameFactory = Board -> IO Game

instance Show GameType where
  show = description

gameTypes :: [GameType]
gameTypes = [
    GameType "User vs CPU" createUserVsCPU,
    GameType "CPU vs CPU" createCPUVsCPU
  ]

createUserVsCPU :: GameFactory
createUserVsCPU board = do
  putStrLn "Select the strategy of the CPU player:"
  cpu <- selectOption cpuPlayers
  putStrLn "Who will make the first move?"
  let player = Player "User" user 0
  putOptions [player, cpu]
  firstMove <- promptInt "Select option" 1 2
  return (Game player cpu board (firstMove-1) 1)

createCPUVsCPU :: GameFactory
createCPUVsCPU board = do
  putStrLn "Select the strategy of the first player:"
  player1 <- selectOption cpuPlayers
  putStrLn "Select the strategy of the second player:"
  player2 <- selectOption cpuPlayers
  return (Game player1 player2 board 0 1)


-- Game
data Game = Game { player1 :: Player, player2 :: Player, board :: Board,
  nowPlays :: Int, turn :: Int }

instance Show Game where
  show (Game p1 p2 board _ turn) = scoreStr ++ "\n" ++ boardStr
    where
      scoreStr = join "  -  " $ each showWithScore [p1, p2]
      showWithScore = (\i p -> "(P" ++ show (i+1) ++ ") " ++ (show p) ++ " " ++ (show $ score p))
      boardStr = show board

play :: Game -> IO ()
play game@(Game player1 player2 board nowPlays turn)
  | turn > size board = finished game
  | otherwise = do
    let player = [player1, player2]!!nowPlays
    action <- (strategy player) game
    play $ update game action

update :: Game -> Action -> Game
update game@(Game p1 p2 board plays turn) action
  | isValid board action = nextGame
  | otherwise = game
  where
    nextGame = (Game newPlayer1 newPlayer2 newBoard newPlays (turn+1))
    (newBoard, scored) = apply board action
    [newPlayer1, newPlayer2] = mapSelect
      (\(Player name strat score) -> Player name strat (score+scored)) plays [p1, p2]
    newPlays
      | scored > 0 = plays
      | otherwise = (plays+1) `mod` 2

finished :: Game -> IO ()
finished game = do
  putStrLn $ show game
  putStrLn $ status game

status :: Game -> String
status (Game p1 p2 _ _ _)
  | score1 > score2 = wins p1 1
  | score1 < score2 = wins p2 2
  | otherwise = "Game drawn!"
  where
    score1 = score p1
    score2 = score p2
    wins p i = "(P" ++ show i ++ ") " ++ show p ++ " wins!"


-- Board
data Board = Board { rows :: Int, cols :: Int, cells :: [[Cell]]}
data Cell = Empty | S | O deriving Eq

instance Show Board where
  show (Board _ _ cells) = separate rows
    where
      rows = map ((\x -> " " ++ x ++ " ") . (intercalate " | ")) cells

instance Show Cell where
  show Empty = " "
  show S = "S"
  show O = "O"

readBoard :: IO Board
readBoard = do
  rows <- promptInt "Enter number of rows" 1 50
  cols <- promptInt "Enter number of columns" 1 50
  return $ newBoard rows cols

newBoard :: Int -> Int -> Board
newBoard rows cols = Board rows cols [take cols $ repeat Empty | _ <- [1..rows]]

size :: Board -> Int
size (Board rows cols _) = rows * cols

getCell :: Board -> Int -> Int -> Maybe Cell
getCell (Board rows cols cells) row col
  | row >= 0 && col >= 0 && row < rows && col < cols = Just $ (cells!!row)!!col
  | otherwise = Nothing

getRelativeCells :: Board -> Int -> Int -> [(Int, Int)] -> [Maybe Cell]
getRelativeCells _ _ _ [] = []
getRelativeCells board row col ((i,j):xs) =
  getCell board (row+i) (col+j) : getRelativeCells board row col xs

isValid :: Board -> Action -> Bool
isValid board (Action row col _) = getCell board row col == Just Empty

apply :: Board -> Action -> (Board, Int)
apply b@(Board rows cols cells) a@(Action row col letter) = (newBoard, scored)
  where
    newBoard = Board rows cols updatedCells
    scored = actionScore b a
    updatedCells = mapSelect (mapSelect (\_ -> letter) col) row cells

actionScore :: Board -> Action -> Int
actionScore board (Action row col S) =
  length $ filter (\x -> x == [Just O, Just S]) $ map (getRelativeCells board row col) dirs
  where
    dirs = [[(i, j), (i*2, j*2)] | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0]

actionScore board (Action row col O) =
  length $ filter (\x -> x == [Just S, Just S]) $ map (getRelativeCells board row col) dirs
  where
    dirs = [[(i, j), (-i,-j)] | i <- [-1..0], j <- [-1..1], i /= 0 || (j /= 0 && j /= 1)]


-- Strategies
type Strategy = Game -> IO Action
data Player = Player { name :: String, strategy :: Strategy, score :: Int }
data Action = Action { row :: Int, col :: Int, letter :: Cell }

instance Show Player where
  show = name

cpuPlayers :: [Player]
cpuPlayers = [
    Player "Random" rand 0,
    Player "Brute force" brute 0
  ]

user :: Strategy
user game@(Game player1 player2 board _ turn) = do
  putStrLn $ "Turn " ++ show turn
  putStrLn $ show game
  row <- promptInt "Enter the row you want to modify" 1 $ rows board
  col <- promptInt "Enter the column yo want to modify" 1 $ cols board
  putStrLn "Select the letter you want to add:"
  letter <- selectOption [S, O]
  return $ Action (row-1) (col-1) letter

rand :: Strategy
rand (Game _ _ board _ _) = do
  row <- randomRIO (0, (rows board)-1)
  col <- randomRIO (0, (cols board)-1)
  letter <- randomRIO (0, 1)
  return $ Action row col $ [S, O]!!letter

brute :: Strategy
brute game@(Game _ _ board _ _)
  | actionScore board action == 0 = rand game
  | otherwise = return action
  where
    action = bestAction board

bestAction :: Board -> Action
bestAction board@(Board rows cols _) = maximumBy (actionScore board) availableActions
  where
    availableActions = filter (isValid board) allActions
    allActions = [ Action i j letter | i <- [0..rows-1], j <- [0..cols-1], letter <- [S, O]]


-- Main
main = do
  putStrLn " ___  ___  ___ _       _ _ "
  putStrLn "/ __|/ _ \\/ __| |_____| | |"
  putStrLn "\\__ \\ (_) \\__ \\ / / -_) | |"
  putStrLn "|___/\\___/|___/_\\_\\___|_|_|"
  putStrLn ""
  putStrLn "Welcome! Select the type of game:"

  gameType <- selectOption gameTypes
  board <- readBoard
  game <- (factory gameType) board
  play game

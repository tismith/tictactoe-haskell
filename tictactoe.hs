--Haskell solution to Tony Morris's TicTacToe challenge
--http://blog.tmorris.net/scala-exercise-with-types-and-abstraction/

data Position = TL | TM | TR | ML | MM | MR | BL | BM | BR 
    deriving (Eq, Show)
data Player = One | Two 
    deriving (Eq, Show)
type Move = (Player, Position)
data Unfinished = Unfinished [Move] deriving Show
data Finished = Finished [Move] deriving Show

-- this Either is so that 'move' can
-- return a potentially finished game
type Game = Either Unfinished Finished

class TicTacToe a where
    playerAt :: a -> Position -> Maybe Player
instance TicTacToe Unfinished where
    playerAt (Unfinished moves) = findPlayer moves 
instance TicTacToe Finished where
    playerAt (Finished moves) = findPlayer moves

winningPatterns :: [[Position]]
winningPatterns = [[TL,TM, TR], [ML, MM, MR], [BL, BM, BR], 
    [TL, ML, BL], [TM, MM, BM], [TR, MR, BR], [TL, MM, BR],
    [TR, MM, BL]]

newGame :: Unfinished
newGame = (Unfinished [])

--this was failing before I introduced the Either to Game
--test = whoWon (Unfinished [])

-- applies a move to a game
-- must only accept unfinished games
move :: Unfinished -> Position -> Game
move (Unfinished []) position = Left (Unfinished [(One, position)])
move game@(Unfinished moves) position
    | isFinished (Unfinished moveList) = Right (Finished moveList)
    | otherwise = Left (Unfinished moveList) 
    where
        player = whoseTurn game
        moveList = (player, position):moves

isFinished :: Unfinished -> Bool
isFinished (Unfinished []) = False
isFinished (Unfinished moves@((player,_):_)) = 
    any (null) (foldl (removeMove) winningPatterns lastPlayersMoves)
    where 
        lastPlayersMoves = map (snd) $ filter ((== player) . fst) moves
        removeMove winningMoves move = map (filter (/= move)) winningMoves

findPlayer :: [Move] -> Position -> Maybe Player
findPlayer moves position 
    | null move = Nothing
    | otherwise = Just $ (fst . head) move 
    where 
        move = filter ((== position) . snd) moves

-- this should just work on unfinished games
whoseTurn :: Unfinished -> Player
whoseTurn (Unfinished []) = One
whoseTurn (Unfinished ((player,_):_)) = otherPlayer player

otherPlayer :: Player -> Player
otherPlayer One = Two
otherPlayer Two = One

-- this should only work on finished games
whoWon :: Finished -> Player
whoWon (Finished ((winner,_):_)) = winner


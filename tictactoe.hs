data Position = TL | TM | TR | ML | MM | MR | BL | BM | BR 
    deriving (Eq, Show)
data Player = One | Two 
    deriving (Eq, Show)
type Move = (Player, Position)
data Unfinished = Unfinished [Move] deriving Show
data Finished = Finished [Move] deriving Show
-- this Either is so that move can
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
move (Unfinished moves@((prevPlayer,_):_)) position =
    case (isFinished (Unfinished moveList)) of
        True -> Right (Finished moveList)
        False -> Left (Unfinished moveList)
    where
        moveList = ((otherPlayer prevPlayer, position):moves)

isFinished :: Unfinished -> Bool
isFinished (Unfinished []) = False
isFinished (Unfinished moves@((player,_):_)) = 
    any (null) (foldl (removeMove) winningPatterns lastPlayersMoves)
    where 
        lastPlayersMoves = map (snd) $ filter ((== player) . fst) moves
        removeMove winningMoves move = map (filter (/= move)) winningMoves

otherPlayer :: Player -> Player
otherPlayer One = Two
otherPlayer Two = One

findPlayer :: [Move] -> Position -> Maybe Player
findPlayer moves position = 
    case (null move) of
        True -> Nothing
        False -> Just $ (fst . head) move 
    where 
        move = filter ((== position) . snd) moves

-- this should just work on unfinished games
whoseTurn :: Unfinished -> Player
whoseTurn (Unfinished []) = One
whoseTurn (Unfinished ((player,_):_)) = otherPlayer player

-- this should only work on finished games
whoWon :: Finished -> Player
whoWon (Finished ((winner,_):_)) = winner

--should work on finished and unfinished games?
--does that mean 
--playerAt::Finished -> Position -> Player
--and 
--playerAt::Unfinished -> Position -> Player
--or
--playerAt :: Game -> Position -> Player


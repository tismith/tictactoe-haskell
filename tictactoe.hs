data Position = TL | TM | TR | ML | MM | MR | BL | BM | BR 
    deriving (Eq, Show)
data Player = One | Two 
    deriving (Eq, Show)
type Move = (Player, Position)
data Game = Unfinished [Move] | Finished [Move] 
    deriving Show

winningPatterns = [[TL,TM, TR], [ML, MM, MR], [BL, BM, BR], 
    [TL, ML, BL], [TM, MM, BM], [TR, MR, BR], [TL, MM, BR],
    [TR, MM, BL]]

-- applies a move to a game
-- must only accept unfinished games
move :: Game -> Position -> Game
move (Unfinished []) position = Unfinished [(One, position)]
move (Unfinished moves@((prevPlayer,_):_)) position =
    case (isFinished (Unfinished moveList)) of
        True -> Finished moveList
        False -> Unfinished moveList
    where
        moveList = ((otherPlayer prevPlayer, position):moves)

isFinished :: Game -> Bool
isFinished (Unfinished []) = False
isFinished (Unfinished moves@((player,_):_)) = 
    any (null) (foldl (removeMove) winningPatterns lastPlayersMoves)
    where 
        lastPlayersMoves = map (snd) $ filter ((== player) . fst) moves
        removeMove winningMoves move = map (filter (/= move)) winningMoves

otherPlayer :: Player -> Player
otherPlayer One = Two
otherPlayer Two = One

-- this should just work on unfinished games
--whoseTurn :: Game -> Player

-- this should only work on finished games
whoWon :: Game -> Player
whoWon (Finished ((winner,_):_)) = winner

--should work on finished and unfinished games
--playerAt :: Game -> Position -> Player


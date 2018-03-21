module Lib where

import Data.Vector
import System.Random

boardSize :: Int
boardSize = 9
winLength :: Int
winLength = 5

data Square 
    = SQEmpty 
    | SQBlack 
    | SQWhite 
    deriving (Eq,Show)

type Board = Vector Square
type Loc = (Int,Int)

emptyBoard :: Board 
emptyBoard = Data.Vector.replicate (boardSize*boardSize) SQEmpty

boardIndex :: Loc -> Int
boardIndex (x,y) = x*boardSize + y

boardValue :: Board -> Loc -> Square
boardValue b l = b ! (boardIndex l)

boardSet :: Board -> Loc -> Square -> Board
boardSet b (x,y) s = b // [(i,s)]
    where 
        i = x*boardSize + y

countDir :: Board -> Int -> Loc -> Loc -> Int
countDir b cnt (x,y) (x',y') =
    if valid (x+x',y+y') && boardValue b (x,y) == boardValue b (x+x',y+y') 
    then countDir b (cnt+1) (x+x',y+y') (x',y')
    else cnt
        where 
            valid (-1,_) = False                                     
            valid (_,-1) = False
            valid (a,b) = not ( a == boardSize || b == boardSize)
-- | does player win after moving to location 
isWinner :: Board -> Loc -> Bool
isWinner b l = Prelude.any (>=winLength) ls
    where
        directions :: [Loc]
        directions = [(1,0),(0,1),(1,1),(1,-1)] 
        ls :: [Int]                        
        ls = [1 + countDir b 0 l (x,y) + countDir b 0 l (-x,-y) | (x,y) <- directions]

showBoard :: Board -> String
showBoard b = unlines ls 
    where 
        squareToChar SQEmpty = '.'
        squareToChar SQBlack = 'x'
        squareToChar SQWhite = 'o'
        ls = do 
            let bs = [0..(boardSize-1)]
            i <- bs
            return [squareToChar (b ! (i*boardSize+j)) | j <- bs]

getValidMoves :: Board -> [Loc]
getValidMoves b = [(i,j) | i <- [0..(boardSize-1)]
                        ,j <- [0..(boardSize-1)]
                        ,b ! (i*boardSize+j)  == SQEmpty
                        ]
    

-- Needs to fix invalid move probabilities            
getMoveProbabilities :: Board -> Square -> [Loc] -> [(Float,Loc)]
getMoveProbabilities b s ls= [(p,l) | l <- ls]
    where 
        len = Prelude.length ls
        p :: Float
        p = (fromIntegral 1) / (fromIntegral len)
            

--selectMoveRandomly [] _ = Left "error: no move selected. rand > 1 or move probs don't add to 1"
selectMoveRandomly :: [(Float,Loc)] -> Float -> Loc
selectMoveRandomly ((_,l):[]) _ = l
selectMoveRandomly ((f,l):mps) rand = 
    if rand <= f
    then l 
    else selectMoveRandomly mps (rand-f)
                                        

--if no moves available then winner is player 
autoPlayRollout :: Board -> Square -> [Float] -> (Square,Board,Loc)
autoPlayRollout b s (r:rs) = if winVal == SQEmpty then autoPlayRollout b' s' rs else (winVal,b',(x,y))
    where
        ls = getValidMoves b
        s' = if s == SQBlack then SQWhite else SQBlack
        mps = getMoveProbabilities b s ls
        (x,y) = selectMoveRandomly mps r
        b' = b // [(x*boardSize+y,s)] 
        winVal = if ls == [] || isWinner b' (x,y)  then s else SQEmpty


data Mcts = MkMcts 
    {board :: Board
    ,player :: Square
    ,moves :: [Loc]
    ,wins :: Int
    ,plays :: Int
    ,won :: Square
    ,children :: [Mcts]
    } 

showMcts :: Mcts -> String
showMcts mcts = (showBoard (board mcts)) Prelude.++ (show (player mcts)) Prelude.++ (show (moves mcts)) Prelude.++ (show (won mcts))

initialMcts :: Board -> Square -> Square -> Mcts
initialMcts b player won = MkMcts b player vmoves 0 0 won [initialMcts b' nextPlayer w' | (b',w') <- boardsWins]
    where 
        vmoves = (getValidMoves b)
        nextPlayer = if player == SQWhite then SQBlack else SQWhite
        boardsLocs = [(b // [((x*boardSize+y),nextPlayer)],(x,y)) | (x,y) <- vmoves]
        thisWins SQEmpty b l = if isWinner b l then nextPlayer else SQEmpty
        thisWins sq _ _ = sq
        boardsWins = [(b1,thisWins won b1 l1) | (b1,l1) <- boardsLocs]

ucb1 :: Mcts -> Int -> Float
ucb1 mcts totalPlays = if (plays mcts) == 0 then inf else mean + explore
    where
        mean :: Float
        mean = (fromIntegral (wins mcts)) / (fromIntegral (plays mcts))
        explore :: Float
        explore = (2*(log (fromIntegral totalPlays)) / (fromIntegral(plays mcts))) ** 0.5
        inf :: Float
        inf = 1.0/0.0

--returned first list is reversed
selectTopRandomly :: [Mcts] -> Int -> Float -> ([Mcts],Mcts,[Mcts])
selectTopRandomly mctss totalPlays r = selectIt [] mctss r
    where
        scores = [ucb1 m totalPlays | m <- mctss]
        maxcnt xs = (max,len)
            where 
                len = Prelude.length [i | i <- xs, max == i]
                max = Prelude.maximum xs
        (maxScore,cnt) = maxcnt scores

        selectIt :: [Mcts] ->  [Mcts] -> Float -> ([Mcts],Mcts,[Mcts])
        selectIt l [] _ = selectIt [] (Prelude.reverse l) (-1*1.0) -- pick first one (shouldn't happen)
        selectIt l (m:rs) rand = 
            if isTop && rand' <=0 
            then (l,m,rs)
            else selectIt (m:l) rs rand'            
            where 
                isTop = (ucb1 m totalPlays) == maxScore
                rand' = if isTop then rand-(1.0/(fromIntegral cnt)) else rand

            

            
        
oneMctsUpdate :: Mcts -> Int -> [Float]-> (Mcts,Square)
oneMctsUpdate mcts totalPlays (r:rs) =
    --select
    if (plays mcts) /= 0 
    then
        let  
            --select/expand
            (leftRev,randTop,right) = selectTopRandomly (children mcts) totalPlays r
            left = Prelude.reverse leftRev
            (mctsNew,winnerSq) = oneMctsUpdate randTop totalPlays rs
            --backprop
            newWins = (wins mcts) + if winnerSq == (player mcts) then 1 else 0
            newChildren = left Prelude.++ (mctsNew:right)
            newPlays = (plays mcts) + 1
            --ret = MkMcts (board mcts) (player mcts) (moves mcts) newWins newPlays newChildren
            ret = mcts {wins=newWins, plays=newPlays, children=newChildren}
        in
        (ret,winnerSq)
    else
        --rollout (playout)
        let 
            gameWon = won mcts
            (winner,_,_) = if gameWon == SQEmpty then autoPlayRollout (board mcts) (player mcts) rs else (gameWon,board mcts,(0,0))
            numWins = if winner == (player mcts) then 1 else 0
        in 
            (mcts {plays = 1,wins = numWins},winner)


mctsUpdates :: Mcts -> Int -> IO Mcts
mctsUpdates mcts cnt = do 
    g <- newStdGen
    let rs = randomRs (0.0,1.0) g    
    let totPlays = plays mcts
    let (mcts',_) = oneMctsUpdate mcts totPlays rs
    if cnt <= 0 then pure mcts else mctsUpdates mcts' (cnt-1)

selectTopMoveDet :: Mcts -> Mcts
selectTopMoveDet mcts = initialMcts b p w
    where 
          p = if (player mcts) == SQBlack then SQWhite else SQBlack
          mostPlays = Prelude.maximum [plays m | m <- children mcts]
          mcts' = Prelude.head [m | m <- children mcts, plays m == mostPlays]
          b = board mcts'
          w = won mcts'

autoPlay :: Mcts -> Int -> IO ()
autoPlay mcts cnt = do    
    putStrLn  (showBoard (board mcts))
    mctsUpdate <- mctsUpdates mcts cnt
    let mcts' = selectTopMoveDet mctsUpdate
    if won mcts /= SQEmpty
    then putStrLn (show (won mcts))
    else autoPlay mcts' cnt


testWinner :: IO ()
testWinner = do
    let b = Data.Vector.replicate (boardSize*boardSize) SQBlack
    putStrLn (showBoard b)
    putStrLn (show (countDir b 0 (3,3) (1,1)))  
    putStrLn (show (isWinner b (0,0)))
    let e = emptyBoard
    let h5 = e // [(i,SQBlack) | i <- [0..4]]
    putStrLn (showBoard h5)
    putStrLn (show [isWinner h5 (0,i) | i <- [0..4]])
    let v5 = e // [(i*boardSize,SQBlack) | i <- [0..4]]
    putStrLn (showBoard v5)
    putStrLn (show [isWinner v5 (i,0) | i <- [0..4]])
    let s5 = e // [(i*boardSize+i,SQBlack) | i <- [0..4]]
    putStrLn (showBoard s5)
    putStrLn (show [isWinner s5 (i,i) | i <- [0..4]])
    let b5 = e // [(i*boardSize+boardSize-i-1,SQBlack) | i <- [0..4]]
    putStrLn (showBoard b5)
    putStrLn (show [isWinner b5 (i,boardSize-i-1) | i <- [0..4]])

maintest :: IO ()
maintest = do 
        g <- getStdGen
        --let zero = 0.0
        --let one = 1.0
        let rs = randomRs (0.0,1.0) g
        let (_,b,l) = autoPlayRollout emptyBoard SQBlack rs
        putStrLn (showBoard b)
        putStrLn (show l)
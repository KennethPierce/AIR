-- Necessary:
--{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE DeriveAnyClass #-}


module Lib where

import Data.Vector.Instances() -- hashing
import Data.Vector
import System.Random

import qualified Data.Traversable 
--import qualified Data.MessagePack
import qualified Control.Monad
import qualified Data.List
import qualified Data.Hashable -- for haxl
import qualified Data.Typeable -- for haxl
import qualified Haxl.Core --for batching call to TF inferences
--import qualified Data.ByteString as BS

boardSize :: Int
boardSize = 7
winLength :: Int
winLength = 5

data Square 
    = SQEmpty 
    | SQBlack 
    | SQWhite 
    deriving (Eq,Show,Enum)

instance Data.Hashable.Hashable Square where
    hashWithSalt s sq = Data.Hashable.hashWithSalt s (0::Int, fromEnum sq)
    


type Board = Vector Square
type Loc1 = (Int,Int)
type Loc = Int
type RandInts = [Int]
type RandFloats = [Float]
type History = [Int]


type HaxlId = Haxl.Core.GenHaxl ()
runHaxlId :: HaxlId a -> IO (a)
runHaxlId haxlid = do
  let stateStore = Haxl.Core.stateSet TensorFlowState{} Haxl.Core.stateEmpty
  env0 <- Haxl.Core.initEnv stateStore ()
  let flag0 = Haxl.Core.flags env0
  let flag1 = flag0{Haxl.Core.caching=0}
  let env1 = env0{Haxl.Core.flags=flag1} -- turn off caching between calls to runHaxlId (memleak)
  Haxl.Core.runHaxl env1 haxlid

getInference :: Board -> HaxlId (Vector Float,Float)
getInference b = Haxl.Core.dataFetch (GetInference b)

data TensorFlowReq a where
    GetInference :: Board -> TensorFlowReq (Vector Float,Float) 
    deriving (Data.Typeable.Typeable)


deriving instance Eq (TensorFlowReq a)
deriving instance Show (TensorFlowReq a)
instance Haxl.Core.ShowP TensorFlowReq where showp = show

instance Data.Hashable.Hashable (TensorFlowReq a) where
    hashWithSalt s (GetInference a) = Data.Hashable.hashWithSalt s (0::Int, a)
 
instance Haxl.Core.DataSourceName TensorFlowReq where
    dataSourceName _ = "TensorFlowData"

instance Haxl.Core.StateKey TensorFlowReq where
    data State TensorFlowReq = TensorFlowState {}

vectOne :: Data.Vector.Vector Float
vectOne = Data.Vector.replicate (boardSize*boardSize) 1.0


instance Haxl.Core.DataSource u TensorFlowReq where
--  fetch :: State req -> Flags -> u -> [BlockedFetch req] -> PerformFetch
    fetch _state _flags _userEnv blockedFetches = Haxl.Core.SyncFetch $  do
        let 
            _b :: [Board]
            resultVarInferences :: [Haxl.Core.ResultVar (Vector Float,Float)]
            (_b,resultVarInferences) = Data.List.unzip [(b1, r) | Haxl.Core.BlockedFetch (GetInference b1) r <- blockedFetches]
            batchGetInference :: IO ()
            batchGetInference = do
                --putStrLn (showBoard (Data.List.head boards))
                --putStrLn ("Batched call " Data.List.++ (show (Data.List.length resultVarInferences)))
                let results = [(vectOne,0) | _ <- ([0..] :: [Int])  ] 
                Control.Monad.mapM_ (uncurry Haxl.Core.putSuccess) (Data.List.zip resultVarInferences results)
        Control.Monad.unless (Data.List.null resultVarInferences) batchGetInference

emptyBoard :: Board 
emptyBoard = Data.Vector.replicate (boardSize*boardSize) SQEmpty

boardIndex :: Loc1 -> Int
boardIndex (x,y) = x*boardSize + y

boardValue :: Board -> Loc1 -> Square
boardValue b l = b ! (boardIndex l)

boardSet :: Board -> Loc1 -> Square -> Board
boardSet b (x,y) s = b // [(i,s)]
    where 
        i = x*boardSize + y

countDir :: Board -> Int -> Loc1 -> Loc1 -> Int
countDir b cnt (x,y) (x',y') =
    if valid (x+x',y+y') && boardValue b (x,y) == boardValue b (x+x',y+y') 
    then countDir b (cnt+1) (x+x',y+y') (x',y')
    else cnt
        where 
            valid (-1,_) = False                                     
            valid (_,-1) = False
            valid (a1,b1) = not ( a1 == boardSize || b1 == boardSize)
-- | does player win after moving to location 
isWinner :: Board -> Loc -> Bool
isWinner b l = Prelude.any (>=winLength) ls
    where
        l' = quotRem l boardSize
        directions :: [Loc1]
        directions = [(1,0),(0,1),(1,1),(1,-1)] 
        ls :: [Int]                        
        ls = [1 + countDir b 0 l' (x,y) + countDir b 0 l' (-x,-y) | (x,y) <- directions]

thisWins :: Square -> Board -> Loc -> [Loc] -> Square
thisWins SQEmpty b l (_:[]) =  b!l 
thisWins SQEmpty b l _  = if  isWinner b l then b!l else SQEmpty
thisWins sq _ _ _ = sq



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


-- Needs to fix invalid move probabilities            
getMoveProbabilities :: Board -> Square -> [Loc] -> HaxlId [(Float,Loc)]
getMoveProbabilities b sq ls = do 
    (probs,_) <- getInference b'
    let validMoves = [probs ! i | i <- ls]
    let sumVal = Prelude.sum validMoves
    let probs' = [i / sumVal | i <- validMoves]
    pure (Data.List.zip probs' ls)
    where 
        --Alway send board as if player Black moves next.
        --Minimize learning time.
        b' = flipBoardIfWhite sq b
        flipBoardIfWhite SQBlack b1 = b1
        flipBoardIfWhite _ b1 = fmap flipSq b1
        flipSq SQEmpty = SQEmpty
        flipSq SQBlack = SQWhite
        flipSq SQWhite = SQBlack

--selectMoveRandomly [] _ = Left "error: no move selected. rand > 1 or move probs don't add to 1"
selectMoveRandomly :: [(Float,Loc)] -> Float -> Int -> (Loc,Int)
selectMoveRandomly ((_,l):[]) _ idx = (l,idx)
selectMoveRandomly ((f,l):mps) rand idx = 
    if rand <= f
    then (l,idx) 
    else selectMoveRandomly mps (rand-f) (idx+1)
                                        



moves :: Mcts -> [Loc]
moves mcts = [i | i <- [0..(boardSize*boardSize-1)], b ! i == SQEmpty ] 
    where 
        b = board mcts

data Mcts = MkMcts 
    {board :: Board
    ,player :: Square
    ,moves_:: ()
    ,wins :: Int
    ,plays :: Int
    ,won :: Square
    ,children :: [Mcts]
    } 

showMcts :: Mcts -> String
showMcts mcts = (showBoard (board mcts)) Prelude.++ (show (player mcts)) Prelude.++ (show (moves mcts)) Prelude.++ (show (won mcts))

initialMcts :: Board -> Square -> Square -> Mcts
initialMcts b p w = MkMcts b p () 0 0 w [initialMcts b' nextPlayer w' | (b',w') <- boardsWins]
    where 
        vmoves = moves (MkMcts b p () 0 0 w [])
        nextPlayer = if p == SQWhite then SQBlack else SQWhite
        boardsLocs :: [(Board,Loc)]
        boardsLocs = [(b // [(i,nextPlayer)],i) | i  <- vmoves]
        boardsWins :: [(Board,Square)]
        boardsWins = [(b1,thisWins w b1 l1 vmoves) | (b1,l1) <- boardsLocs]

--if no moves available then winner is player 
autoPlayRollout :: Mcts -> RandFloats -> HaxlId Square
autoPlayRollout (MkMcts _ _ _ _ _ SQBlack _) _ = pure SQBlack
autoPlayRollout (MkMcts _ _ _ _ _ SQWhite _) _ = pure SQWhite
autoPlayRollout mcts@(MkMcts b p _ _ _ w _) (r:rs) = do 
    mps <- getMoveProbabilities b p ls
    let (_,idx) = selectMoveRandomly mps r 0 
    let b' = b // [(idx,nextPlayer)]
    let newWin = if w /= SQEmpty then w else thisWins w b' idx ls
    let mcts' = MkMcts b' nextPlayer () 0 0 newWin []
    autoPlayRollout mcts' rs 
    where
        ls = moves mcts
        nextPlayer = if p == SQBlack then SQWhite else SQBlack
        

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
        maxcnt xs = (max1,len)
            where 
                len = Prelude.length [i | i <- xs, max1 == i]
                max1 = Prelude.maximum xs
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

            

oneMctsUpdate :: Mcts -> Int -> RandFloats -> HaxlId (Mcts,Square)
oneMctsUpdate mcts@(MkMcts _ _ _ _ 0 gameWon _) _ (_:rs) = do 
    winner <- hwinner
    let numWins = if winner == (player mcts) then 1 else 0  
    pure (mcts {plays = 1,wins = numWins},winner)
    where
        --rollout (playout)
        hwinner = if gameWon == SQEmpty then autoPlayRollout mcts rs else pure gameWon
oneMctsUpdate mcts@(MkMcts _ _ _ _ _ SQEmpty _) totalPlays (r:rs) = do 
    (mctsNew,winnerSq) <- oneMctsUpdate randTop totalPlays rs
    let     
        newChildren = left Prelude.++ (mctsNew:right)
        newPlays = (plays mcts) + 1
        --backprop
        newWins = (wins mcts) + if winnerSq == (player mcts) then 1 else 0
        ret = mcts {wins=newWins, plays=newPlays, children=newChildren}        
    pure (ret,winnerSq)
    where 
        --select/expand
        (leftRev,randTop,right) = selectTopRandomly (children mcts) totalPlays r
        left = Prelude.reverse leftRev
oneMctsUpdate mcts@(MkMcts _ _ _ _ _ winner _) _ _ = do
    pure (ret,winner)
    where  
        newPlays = (plays mcts) + 1
        newWins = (wins mcts) + if winner == (player mcts) then 1 else 0
        ret = mcts {wins=newWins, plays=newPlays}

            


mctsUpdates :: Mcts -> Int -> RandInts ->  HaxlId Mcts
mctsUpdates mcts cnt (i:is) = do
    (mcts',_) <- oneMctsUpdate mcts totPlays rs
    let res = if cnt <= 0 then pure mcts else mctsUpdates mcts' (cnt-1) is
    res
    where 
        g = mkStdGen i
        rs = randomRs (0.0,1.0) g    
        totPlays = plays mcts

--reusing stats from previous plays
selectTopMoveDet :: Mcts -> Mcts
selectTopMoveDet mcts = mcts'
    where 
          mostPlays = Prelude.maximum [plays m | m <- children mcts]
          mcts' = Prelude.head [m | m <- children mcts, plays m == mostPlays]

    

selfPlay :: Mcts -> Int -> RandInts -> HaxlId Mcts
selfPlay mcts@(MkMcts _ _ _ _ _ SQEmpty _) cnt (r:rs) = do 
    let g = mkStdGen r
    let rs' = randoms g
    mctsUpdate <- mctsUpdates mcts cnt rs'
    let mcts'  = selectTopMoveDet mctsUpdate
    let mcts'' = initialMcts (board mcts') (player mcts') (won mcts') -- memory savings
    selfPlay mcts'' cnt rs
selfPlay mcts _ _ =  pure mcts

selfPlays :: [Mcts] -> Int -> RandInts -> HaxlId [Mcts]
selfPlays mctss cnt rs = Control.Monad.mapM play (Data.List.zip mctss rs)
    where
        play (mcts,r) = selfPlay mcts cnt rs'
            where 
                g = mkStdGen r
                rs' = randoms g

selfPlaysIO :: Int -> Int -> IO ()
selfPlaysIO num cnt = do 
    g <- newStdGen
    let rs = randoms g
    let mctss = Data.List.replicate num mctsInitBoard
    mctss' <- runHaxlId (selfPlays mctss cnt rs)
    _ <- Data.Traversable.for mctss' (\mcts -> putStrLn (showBoard (board mcts)))
    pure ()

mctsInitBoard :: Mcts
mctsInitBoard = initialMcts (emptyBoard) SQBlack SQEmpty

testWinner :: IO ()
testWinner = do
    let b = Data.Vector.replicate (boardSize*boardSize) SQBlack
    putStrLn (showBoard b)
    putStrLn (show (countDir b 0 (3,3) (1,1)))  
    putStrLn (show (isWinner_ b (0,0)))
    let e = emptyBoard
    let h5 = e // [(i,SQBlack) | i <- [0..4]]
    putStrLn (showBoard h5)
    putStrLn (show [isWinner_ h5 (0,i) | i <- [0..4]])
    let v5 = e // [(i*boardSize,SQBlack) | i <- [0..4]]
    putStrLn (showBoard v5)
    putStrLn (show [isWinner_ v5 (i,0) | i <- [0..4]])
    let s5 = e // [(i*boardSize+i,SQBlack) | i <- [0..4]]
    putStrLn (showBoard s5)
    putStrLn (show [isWinner_ s5 (i,i) | i <- [0..4]])
    let b5 = e // [(i*boardSize+boardSize-i-1,SQBlack) | i <- [0..4]]
    putStrLn (showBoard b5)
    putStrLn (show [isWinner_ b5 (i,boardSize-i-1) | i <- [0..4]])
    where
        isWinner_ b (x,y) = isWinner b (x*boardSize+y)

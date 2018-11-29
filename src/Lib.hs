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
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

--{-# LANGUAGE DeriveAnyClass #-}


module Lib where

import Data.Vector.Instances() -- hashing
import qualified Data.Vector.Unboxed as V
import System.Random

import qualified Control.Exception 
import qualified Debug.Trace
import qualified Data.MessagePack as MP
import qualified Data.ByteString.Lazy as BL
import qualified Control.Monad
import qualified Data.List
import qualified Data.Hashable -- for haxl
import qualified Data.Typeable -- for haxl
import qualified Haxl.Core --for batching call to TF inferences
import qualified Haxl.Prelude
import qualified Data.Set as Set
import Network.Wreq
import qualified Control.Lens
--import Data.Aeson.Lens
--import qualified Data.Text as T
import Data.Aeson
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
    


type Board = (V.Vector Bool,V.Vector Bool)
type Loc1 = (Int,Int)
type Loc = Int
type RandInts = [Int]
type RandFloats = [Float]
type History = [Int]
type Moves = Set.Set Int



type HaxlId = Haxl.Core.GenHaxl ()
runHaxlId :: HaxlId a -> IO (a)
runHaxlId haxlid = do
  let stateStore = Haxl.Core.stateSet TensorFlowState{} Haxl.Core.stateEmpty
  env0 <- Haxl.Core.initEnv stateStore ()
  let flag0 = Haxl.Core.flags env0
  let flag1 = flag0{Haxl.Core.caching=0}
  let env1 = env0{Haxl.Core.flags=flag1} -- turn off caching between calls to runHaxlId (memleak)
  Haxl.Core.runHaxl env1 haxlid

getInference :: Board -> HaxlId (V.Vector Float,Float)
getInference b = Haxl.Core.uncachedRequest  (GetInference b)

data TensorFlowReq a where
    GetInference :: Board -> TensorFlowReq (V.Vector Float,Float) 
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

vectOne :: V.Vector Float
vectOne = V.replicate (boardSize*boardSize) 1.0

type RespInt = Response [[Float]]
postMsg :: [Board] ->  IO [[Float]]
postMsg []  = pure []
postMsg boards = do 
    l <- postMsgChunk lhs 
    r <- postMsg rhs
    pure (l++r)
        where 
        (lhs,rhs) = splitAt 4000 boards
        postMsgChunk :: [Board] ->  IO [[Float]]
        postMsgChunk bs = do
            let msg_json = toJSON bs
            p <- post "http://127.0.0.1" msg_json
            r <- asJSON p :: IO RespInt
            pure (r Control.Lens.^. responseBody)


instance Haxl.Core.DataSource u TensorFlowReq where
--  fetch :: State req -> Flags -> u -> [BlockedFetch req] -> PerformFetch
    fetch _ _flags _userEnv blockedFetches = Haxl.Core.SyncFetch $  do
        let 
            boards :: [Board]
            resultVarInferences :: [Haxl.Core.ResultVar (V.Vector Float,Float)]
            (boards,resultVarInferences) = Data.List.unzip [(b1, r) | Haxl.Core.BlockedFetch (GetInference b1) r <- blockedFetches]
            batchGetInference :: IO ()
            batchGetInference = do
                msg <- postMsg boards
                let results = [(V.fromList probs,score) | (score:probs) <- msg]
                Haxl.Prelude.mapM_ (uncurry Haxl.Core.putSuccess) (Data.List.zip resultVarInferences results)
        Control.Monad.unless (Data.List.null resultVarInferences) batchGetInference





emptyBoard :: Board 
emptyBoard = (V.replicate (boardSize*boardSize) False,V.replicate (boardSize*boardSize) False)

boardIndexL :: Loc1 -> Int
boardIndexL (x,y) = x*boardSize + y

boardValueL :: Board -> Loc1 -> Square
boardValueL b l = b `boardIndex` (boardIndexL l)

boardSetL :: Board -> Loc1 -> Square -> Board
boardSetL b (x,y) s = b `boardSet` [(i,s)]
    where 
        i = x*boardSize + y

boardIndex :: Board -> Int -> Square
boardIndex (bb,bw) i = toSquare (bb V.! i) (bw V.! i)
    where 
        toSquare True _ = SQBlack
        toSquare _ True = SQWhite
        toSquare True True = Control.Exception.assert False  SQEmpty
        toSquare _ _ = SQEmpty

boardSet :: Board -> [(Int,Square)] -> Board
boardSet b [] = b
boardSet (bb,bw) ((i,SQBlack):us) = boardSet (bb V.// [(i,True)],bw) us
boardSet (bb,bw) ((i,SQWhite):us) = boardSet (bb,bw V.// [(i,True)]) us
boardSet (bb,bw) ((i,SQEmpty):us) = boardSet (bb V.// [(i,False)],bw V.// [(i,False)]) us

countDir :: Board -> Int -> Loc1 -> Loc1 -> Int
countDir b@(bb,bw) cnt (x,y) (xd,yd) =
    if valid (x',y') && sameValue 
    then countDir b (cnt+1) (x',y') (xd,yd)
    else cnt
        where 
            (x',y') = (x+xd,y+yd)
            valid (-1,_) = False                                     
            valid (_,-1) = False
            valid (a1,b1) = not ( a1    == boardSize || b1 == boardSize)

            idx1 = x* boardSize+y 
            idx2 = x'*boardSize+y'
            bSq = if bb V.! idx1 then bb else bw
            sameValue = bSq V.! idx2

             
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
thisWins SQEmpty b l (_:[]) =  b `boardIndex` l 
thisWins SQEmpty b l _  = if  isWinner b l then b `boardIndex` l else SQEmpty
thisWins sq _ _ _ = sq



showBoard :: Board -> String
showBoard b = unlines ls 
    where 
        squareToChar SQEmpty = '.'
        squareToChar SQBlack = 'b'
        squareToChar SQWhite = 'w'
        ls = do 
            let bs = [0..(boardSize-1)]
            i <- bs
            return [squareToChar (b `boardIndex` (i*boardSize+j)) | j <- bs]



-- Needs to fix invalid move probabilities            
getMoveProbabilities :: Board -> Square -> Moves -> HaxlId (V.Vector (Float,Loc))
getMoveProbabilities b sq m = do 
    (probs,_score) <- getInference b'
    --let probs = vectOne
    let !validMoves = V.backpermute  probs vm
    let sumVal = V.sum validMoves
    let !z = V.zipWith (\a1 b1 -> (a1/sumVal,b1)) validMoves vm
    pure z 
    where 
        --Alway send board as if player Black moves next.
        --Minimize learning time.
        mcnt= Set.size m
        vm = V.fromListN mcnt (Set.toList m)
        b' = flipBoardIfWhite sq b
        flipBoardIfWhite SQBlack b1 = b1
        flipBoardIfWhite _ (bb,bw) = (bw,bb)

--selectMoveRandomly [] _ = Left "error: no move selected. rand > 1 or move probs don't add to 1"
selectMoveRandomly :: [(Float,Loc)] -> Float -> Int -> (Loc,Int)
selectMoveRandomly ((_,l):[]) _ idx = (l,idx)
selectMoveRandomly ((f,l):mps) rand idx = 
    if rand <= f
    then (l,idx) 
    else selectMoveRandomly mps (rand-f) (idx+1)
                                        



movesOnBoard :: Board -> [Loc]
movesOnBoard b = [i | i <- [0..(boardSize*boardSize-1)], b `boardIndex` i == SQEmpty ] 

moves :: Mcts -> [Int]
moves mcts = Control.Exception.assert b m
    where
        b = if m == mob then True else Debug.Trace.trace tracemsg False
        tracemsg = show m ++ show mob
        m = Set.toList (moves_ mcts)
        mob = movesOnBoard (board mcts)


data Mcts = MkMcts 
    {board :: !Board
    ,player :: !Square
    ,moves_:: !Moves
    ,wins :: !Int
    ,plays :: !Int
    ,won :: !Square
    ,children :: [Mcts]
    ,history :: History
    } 

showMcts :: Mcts -> String
showMcts mcts = (showBoard (board mcts)) Prelude.++ (show (player mcts)) Prelude.++ (show (moves mcts)) Prelude.++ (show (won mcts))

initialMcts :: Board -> Square -> Square -> History -> Mcts
initialMcts b p w h = initialMctsMoves b p m w h
    where
        m = Set.fromList (movesOnBoard b)

initialMctsMoves :: Board -> Square -> Moves -> Square -> History -> Mcts
initialMctsMoves b p m w h = MkMcts b p m 0 0 w [initialMctsMoves b' p' m' w' (h':h) | (b',m',w',h') <- boardsWins] h
    where 
        vmoves = Set.toList m
        p' = if p == SQWhite then SQBlack else SQWhite
        boardsLocs :: [(Board,Loc)]
        boardsLocs = [(b `boardSet` [(i,p')],i) | i  <- vmoves]
        boardsWins :: [(Board,Moves,Square,Int)]
        boardsWins = [(b1,Set.delete l1 m,thisWins w b1 l1 vmoves,l1) | (b1,l1) <- boardsLocs]

--if no moves available then winner is player 
autoPlayRollout :: Mcts -> RandFloats -> HaxlId Square
autoPlayRollout (MkMcts _ _ _ _ _ SQBlack _ _) _ = pure SQBlack
autoPlayRollout (MkMcts _ _ _ _ _ SQWhite _ _) _ = pure SQWhite
autoPlayRollout mcts@(MkMcts b p m _ _ w _ _) (r:rs) = do 
    mps <- getMoveProbabilities b p m
    let (l,_idx) = selectMoveRandomly (V.toList mps) r 0 
    let b' = Control.Exception.assert (b `boardIndex` l == SQEmpty) (b `boardSet` [(l,nextPlayer)])
    let newWin = thisWins w b' l ls
    let mcts' = MkMcts b' nextPlayer (Set.delete l m) 0 0 newWin [] []
    autoPlayRollout mcts' rs 
    where
        ls = moves mcts
        nextPlayer = if p == SQBlack then SQWhite else SQBlack
        
-- Give highest or lowest score to won games; otherwise extra rollouts might happen
ucb1 :: Mcts -> Int -> (Float,Float)
ucb1 (MkMcts _ _ _ _ 0 SQEmpty _ _) _ = (0,1/0)
ucb1 (MkMcts _ _ _ mwins mplays SQEmpty _ _) totalPlays = (0,mean+explore)
    where
        mean :: Float
        mean = (fromIntegral mwins) / (fromIntegral mplays)
        explore :: Float
        explore = (2*(log (fromIntegral totalPlays)) / (fromIntegral mplays )) ** 0.5
ucb1 (MkMcts _ mplayer _ _ _ mwon _ _) _ = if (mplayer == mwon) then (1/0,0) else (-1/0,0)

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
oneMctsUpdate mcts@(MkMcts _ _ _ _ 0 gameWon _ _) _ (_:rs) = do 
    winner <- hwinner
    let numWins = if winner == (player mcts) then 1 else 0  
    pure (mcts {plays = 1,wins = numWins},winner)
    where
        --rollout (playout)
        hwinner = if gameWon == SQEmpty then autoPlayRollout mcts rs else pure gameWon
oneMctsUpdate mcts@(MkMcts _ _ _ _ _ SQEmpty _ _) totalPlays (r:rs) = do 
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
oneMctsUpdate mcts@(MkMcts _ _ _ _ _ winner _ _) _ _ = do
    pure (ret,winner)
    where  
        newPlays = (plays mcts) + 1
        newWins = (wins mcts) + if winner == (player mcts) then 1 else 0
        ret = mcts {wins=newWins, plays=newPlays}

  
manyMctsUpdate :: Mcts -> Int -> RandFloats ->  HaxlId (Mcts,Int)
manyMctsUpdate mcts@(MkMcts _ _ _ _ p _ cs _) rollouts rs = do
    x <- Haxl.Prelude.mapM update zs   
    let wins' = sum [wins i | i <- x]
    let plays' = sum [plays i | i <- x]
    let rollouts' = length [b | (_,b) <- zs , b]
    pure (mcts{children=x,wins=wins',plays=plays'},min rollouts rollouts')
    where
        us = [ucb1 m p | m <- cs]
        mx = maximum us
        zs = zip cs [mx == i | i <- us]
        update :: (Mcts,Bool) -> HaxlId Mcts
        update (mcts1,True) = do
            (mcts',_) <- oneMctsUpdate mcts1 p rs
            pure mcts'
        update (mcts1,False) = do
            pure mcts1

mctsUpdates :: Mcts -> Int -> RandInts ->  HaxlId Mcts
mctsUpdates mcts 0 _ =  Debug.Trace.trace tracemsg (pure mcts)
    where 
        tracemsg = " moveCnt="++show (length (moves mcts))
mctsUpdates mcts cnt (i:is) = do
    (mcts',rcnt) <-  manyMctsUpdate mcts cnt rs
    mctsUpdates mcts' (cnt-rcnt) is
    where 
        g = mkStdGen i
        rs = randomRs (0.0,1.0) g    

--reusing stats from previous plays
selectTopMove :: Mcts -> Int -> Mcts
selectTopMove mcts r = mcts'
    where 
          mostPlays = Prelude.maximum [plays m | m <- children mcts]
          mp =  [m | m <- children mcts, plays m == mostPlays]
          rm = r `mod` (length mp)
          mcts' = mp !! rm

    

selfPlay :: Mcts -> Int -> RandInts -> HaxlId Mcts
selfPlay mcts@(MkMcts _ _ _ _ _ SQEmpty _ _) cnt (r:rs) = do 
    let g = mkStdGen r
    let rs' = randoms g
    mctsUpdate <- mctsUpdates mcts cnt rs'
    let mcts'  = selectTopMove mctsUpdate r
    selfPlay mcts' cnt rs
selfPlay mcts _ _ =  pure mcts

selfPlays :: [Mcts] -> Int -> RandInts -> HaxlId [Mcts]
selfPlays mctss cnt rs = Haxl.Prelude.mapM play (Data.List.zip mctss rs)
    where
        play (mcts,r) = selfPlay mcts cnt rs'
            where 
                g = mkStdGen r
                rs' = randoms g

selfPlaysIO :: Int -> Int -> Int -> IO ()
selfPlaysIO numGames numRollouts bMoves= do 
    g <- newStdGen
    let (r:rs) = randoms g
    let mctss = mctsRandomBoards numGames bMoves (randoms (mkStdGen r))
    let splay = selfPlays mctss numRollouts rs
    mctss' <- runHaxlId splay
    _ <- Haxl.Prelude.forM_ mctss' (\mcts -> putStrLn (showBoard (board mcts)))
    let trainData@(_bs,_,_) = msgPackTrainData mctss'
    --_ <- Haxl.Prelude.forM_ _bs (\b -> putStrLn (showBoard b))

    BL.writeFile "mctsTrainBII.mp" (MP.pack trainData)
    pure ()

type TrainData = (Board,Int,Int)
type TrainDatas = [TrainData]
    

rot90V :: V.Vector Int 
rot90V =  V.fromList [ro90Idx i | i <- [0..boardSize*boardSize-1]]
    where
        ro90Idx idx = (boardSize - 1 - r) * boardSize + q 
            where (q,r) = quotRem idx boardSize

rot90Board ::  TrainData -> TrainData
rot90Board ((bb,bw),ws,move) = permBoard rot90V (bb,bw) ws move

mirrorV :: V.Vector Int
mirrorV = V.fromList [mirrorIdx i | i <- [0..boardSize*boardSize-1]]
    where 
        mirrorIdx :: Int -> Int
        mirrorIdx idx = boardSize*q + boardSize - r -1
            where (q,r) = quotRem idx boardSize

mirrorBoard :: TrainData -> TrainData
mirrorBoard ((bb,bw),ws,move) = permBoard mirrorV (bb,bw) ws move

permBoard :: V.Vector Int -> Board -> Int -> Int -> TrainData
permBoard v (bb,bw) ws move = ((perm bb,perm bw),ws,mirrorV V.! move)
    where perm b = V.backpermute b v


addBoards :: TrainData -> TrainDatas -> TrainDatas
addBoards ((bb,bw),ws,m) tds = tds''
    where 
        b = if ws == -1 then (bb,bw) else (bw,bb) 
        td = (b,ws,m)
        rotBoards td0 tds0 = td0:td90:td180:td270:tds0
            where 
                td90  = rot90Board td0
                td180 = rot90Board td90
                td270 = rot90Board td180

        mtd  = mirrorBoard td
        tds' = rotBoards mtd tds
        tds'' = rotBoards td tds'


msgPackTrainData :: [Mcts] -> ([Board],[Int],[Int])
msgPackTrainData mctss = unzip3 (foldr trainData [] mctss)
    where
        trainData :: Mcts -> TrainDatas -> TrainDatas
        trainData mcts tds = recreate wb hist
            where
                (hi:his) = reverse (history mcts)
                w = won mcts
                (wb,np,hist,ws) = if w == SQWhite then (emptyBoard,SQBlack,hi:his,1) else (boardSet emptyBoard [(hi,SQWhite)],SQWhite,his,-1)

                recreate :: Board ->  History -> TrainDatas
                recreate _ [] = tds
                recreate b (h1:[]) = addBoards (b,ws,h1) tds
                --skip loser board, add the mirror board
                recreate b (h1:h2:hs) = addBoards (b,ws,h1) (recreate newBoard hs)
                    where 
                        newBoard = boardSet b [(h1,w),(h2,np)]

mctsInitBoard :: Mcts
mctsInitBoard = initialMcts emptyBoard SQBlack SQEmpty [] 

mctsRandomBoard :: Mcts -> RandInts -> Mcts
mctsRandomBoard m [] = m
mctsRandomBoard m (r:rs) = mctsRandomBoard m' rs
    where
        cs = children m
        mr = mod r (length cs)
        m' = cs !! mr

mctsRandomBoards :: Int -> Int -> RandInts -> [Mcts]
mctsRandomBoards numGames numMoves rs = take numGames [mctsRandomBoard mctsInitBoard (take numMoves rs') | rs' <- rands]
    where
        rands = [randoms (mkStdGen r) | r <- rs]

inferPost :: IO ()
inferPost = do
    postMsg  [emptyBoard,emptyBoard `boardSet` [(i,SQBlack) | i <- [0..4]]]
    pure ()

testWinner :: IO ()
testWinner = do
    let b = (V.replicate (boardSize*boardSize) True,V.replicate (boardSize*boardSize) False)
    putStrLn (showBoard b)
    putStrLn (show (countDir b 0 (3,3) (1,1)))  
    putStrLn (show (isWinner_ b (0,0)))
    let e = emptyBoard
    let h5 = e `boardSet` [(i,SQBlack) | i <- [0..4]]
    putStrLn (showBoard h5)
    putStrLn (show [isWinner_ h5 (0,i) | i <- [0..4]])
    let v5 = e `boardSet` [(i*boardSize,SQBlack) | i <- [0..4]]
    putStrLn (showBoard v5)
    putStrLn (show [isWinner_ v5 (i,0) | i <- [0..4]])
    let s5 = e `boardSet` [(i*boardSize+i,SQBlack) | i <- [0..4]]
    putStrLn (showBoard s5)
    putStrLn (show [isWinner_ s5 (i,i) | i <- [0..4]])
    let b5 = e `boardSet` [(i*boardSize+boardSize-i-1,SQBlack) | i <- [0..4]]
    putStrLn (showBoard b5)
    putStrLn (show [isWinner_ b5 (i,boardSize-i-1) | i <- [0..4]])
    where
        isWinner_ b (x,y) = isWinner b (x*boardSize+y)

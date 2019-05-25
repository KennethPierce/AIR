{-# LANGUAGE NoImplicitPrelude  #-}
module Hex where
import RIO
import qualified Data.Vector as V
import qualified RIO.List as L
--import qualified Data.IntMap as IntMap
--import qualified Data.ByteString.Lazy as BL
--import qualified Data.List as List
import Prelude (putStr,putStrLn)
import qualified Types as AirTypes
import qualified Mcts
import System.Random

type VIntSet = V.Vector Bool
type HexMoves =  V.Vector Int


data Config = MkConfig
    {   boardSize :: Int
    }
emptyConfig :: Config
emptyConfig = MkConfig undefined 

data Hex = MkHex 
    {   hexconfig :: Config
    ,   hexmoves :: HexMoves
    ,   hexcnt :: Int
    }

createHex :: Int -> Hex
createHex bs = MkHex (MkConfig bs) (V.replicate (bs*bs) 0) 0

emptyHex :: Hex
emptyHex = MkHex emptyConfig V.empty 0

data StartOrPos = P1 | P2 | P Int

showHex :: Int -> [Int] ->  IO () 
showHex bs ms = putStr $ toString (MkHex config board  0)
    where 
        config = emptyConfig {boardSize=bs}
        board = empty V.// [(idx,mid) | (idx,mid) <- zip ms [1..]]
        empty = V.replicate (bs*bs) 0

toString :: Hex -> String 
toString (MkHex config hmoves _) =   unlines $ topBoarder:[replicate i ' ' ++ "\\" ++ row i | i <- [0..bs-1]]
    where 
        bs = boardSize config
        topBoarder :: String
        topBoarder = replicate (2*bs) '-'
        row i = foldr (\c str -> c:c:str ) "" [cell i j | j <- [0..bs-1] ]
        cell i j = if v == 0 then ' ' else if even v then 'e' else  'o'
            where v = hmoves V.! (i*bs + j)

connected :: Int -> StartOrPos -> [Int]
connected bs P1 = [i    | i<-[0..bs-1]]
connected bs P2 = [i*bs | i<-[0..bs-1]]
connected bs (P i) = inbounds
    where 
        moveDirs :: [(Int,Int)]
        moveDirs = [(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0)]
        (r,c) = quotRem i bs
        inboundsCoords = filter (\(r',c') -> not $ r' < 0 || c' < 0 || r' >= bs || c' >= bs) [(r+r',c+c') | (r',c') <- moveDirs]
        inbounds = fmap (\(r',c') -> r'*bs+c') inboundsCoords

isPath :: StartOrPos -> StartOrPos -> HexMoves -> Int -> Bool
isPath start end  moves bs = fst $ dfs_ start empty
    where
        empty = V.replicate (bs*bs) False
        same = sameF end
        sameF :: StartOrPos -> Int
        sameF P1 = 1
        sameF P2 = 2
        sameF _ = undefined
        e = endPos end
        endPos P1 = empty V.// [(bs*bs-bs+i,True) | i <- [0..bs-1]]
        endPos P2 = empty V.// [(i*bs+(bs-1),True) | i <- [0..bs-1]]
        endPos _ = empty
        foldDfs :: (Bool,VIntSet) -> Int -> (Bool,VIntSet)
        foldDfs (True,v) _ = (True,v)
        foldDfs (False,v) i = dfs_ (P i) v
        dfs_ :: StartOrPos -> VIntSet -> (Bool,VIntSet)
        dfs_ P1 v = L.foldl foldDfs (False,v) (connected bs P1)
        dfs_ P2 v = L.foldl foldDfs (False,v) (connected bs P2)
        dfs_ (P i) v =  if notVisited && isSame 
                        then if isEnd then (True,v) else L.foldl foldDfs (False,v') ms
                        else (False,v)
            where 
                notVisited = False ==  v V.! i 
                isSame =  0 == mod ((moves V.! i)+same) 2 && (moves V.! i) /= 0
                isEnd = e V.! i 
                ms = connected bs (P i)
                v' = v V.// [(i,True)]
                

instance AirTypes.Game Hex where
    isGameOver (MkHex c m _) = dfsp1 || dfsp2
        where 
            dfsp1 = isPath P1 P1 m bs
            dfsp2 = isPath P2 P2 m bs
            bs = boardSize c

    validMoves (MkHex _ m _) = V.fromList [idx | (idx,val)<-zip [0..] (V.toList m),val == 0]
    makeMove (MkHex c m cnt) move = MkHex c (m V.//[(move,cnt+1)]) (cnt+1)

moveWeightsEqual :: Monad m => Hex -> m (Vector Double)
moveWeightsEqual hex  = pure (V.replicate (bs*bs) 1.0)
    where bs = boardSize (hexconfig hex)

chooseMoveMcts :: Monad m => Int -> Hex -> Int -> m Int
chooseMoveMcts rolloutCnt hex rand = do 
    mcts <- Mcts.runMcts moveWeightsEqual (Mcts.initMcts hex) rolloutCnt rand
    pure $ Mcts.getTopMctsByPlays mcts

instance AirTypes.AI Hex where
    scoreGame (MkHex _ _ hcWin) (MkHex _ _ hcPlayer) = if 0 == mod (hcWin+hcPlayer) 2 then 1 else -1
    moveWeights = moveWeightsEqual
    inferGame = undefined
    trainGame = undefined

autoPlay :: IO ()
autoPlay = autoPlay' players hex rands
    where
        rands = randoms $ mkStdGen 1
        hex = createHex 1
        players = L.cycle [chooseMoveMcts 10 ::  Hex -> Int -> Maybe Int,chooseMoveMcts 20 ::  Hex -> Int -> Maybe Int]
        autoPlay' (p:ps) hex (r:rs) = do
            putStr $ toString hex
            if AirTypes.isGameOver hex 
            then putStrLn "end" 
            else do
                let Just m  = p hex r
                let hex' = AirTypes.makeMove hex m 
                autoPlay' ps hex' rs
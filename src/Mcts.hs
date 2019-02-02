module Mcts where
import qualified Data.IntMap.Lazy as IntMap
import qualified Types as AirTypes
import qualified Debug.Trace
import System.Random
type RandInt = Int

data Mcts a = MkMcts 
    { plays :: Double
    , score :: Double
    , game :: a
    , tree :: IntMap.IntMap (Mcts a)
    }

-- Give highest or lowest score to won games; otherwise extra rollouts might happen
ucb1 :: Int -> Mcts a -> Double
ucb1 totalPlays (MkMcts p s _ _)  = mean + explore
    where
        ftp = if totalPlays < 1 then 1 else  (fromIntegral totalPlays) -- prevent neg in log function below for zero totalplays
        mean = s/p
        explore =  (2*((log ftp)/p)) ** 0.5

-- start each tree with small random score to break ties and simplify algorithm
initMcts :: AirTypes.Game a => a -> RandInt -> Mcts a
initMcts g rand = MkMcts epsilon1 (100*epsilon2) g im 
    where 
        g1 = mkStdGen rand
        (r1:rs) = randoms g1
        g2 = mkStdGen r1
        (epsilon1:epsilon2:_) = randomRs (0.0000001,0.000001) g2     :: [Double]
        im = IntMap.fromAscList [(m,initMcts (AirTypes.makeMove g m) r )| (m,r) <- zip (AirTypes.validMoves g) rs]

rollout :: (AirTypes.Game a,AirTypes.AI a) => a -> a 
rollout g = if AirTypes.isGameOver g then  g else rollout (AirTypes.makeMove g (AirTypes.chooseMove g))

getTopMcts :: (AirTypes.Game a,AirTypes.AI a) => Mcts a -> Int -> Int
getTopMcts mcts totPlays = idx
    where
        ucb1s = IntMap.map (ucb1 totPlays) (tree mcts)
        (idx,_) = IntMap.foldrWithKey maxScore (-1,0/1) ucb1s
        maxScore ::  Int -> Double  -> (Int,Double) -> (Int,Double)
        maxScore idx1 score1 (topIdx,topScore) = if score1 > topScore then (idx1,score1) else (topIdx,topScore)

selectRollout :: (AirTypes.Game a,AirTypes.AI a) => Mcts a -> Int -> (a,Mcts a)
selectRollout mcts1@(MkMcts p1 s1 g1 t1) totPlays =  (endingGame,topMcts''')
    where
        idx = getTopMcts mcts1 totPlays
        topMcts1@(MkMcts topPlays _ topGame _) = t1 IntMap.! idx
        (endingGame,topMcts'@(MkMcts p s g t))= if topPlays < 1 then (rollout topGame,topMcts1) else selectRollout topMcts1 totPlays
        s' = AirTypes.scoreGame endingGame topGame
        topMcts'' = MkMcts  (p+1) (s+s') g t
        topMcts''' = MkMcts p1 s1 endingGame (IntMap.insert idx topMcts'' (tree mcts1) )

mctsUpdate :: (AirTypes.Game a,AirTypes.AI a) => a  -> Int -> RandInt -> Mcts a
mctsUpdate game1 rollouts rand = mctsUpdate' mcts1 rollouts 
    where
        mcts1 = initMcts game1 rand
        mctsUpdate' :: (AirTypes.Game a,AirTypes.AI a) => Mcts a -> Int ->  Mcts a
        mctsUpdate' mcts 0  = mcts {plays= (plays mcts )+ (fromIntegral rollouts)} -- root node doesn't update score. shouldn't matter though; not used.
        mctsUpdate' mcts ro = mctsUpdate' mctsU (ro-1)
            where
                (_,mctsU) = selectRollout mcts (rollouts - ro)

showMcts :: Mcts a -> String
showMcts mcts@(MkMcts plays1 _ _ t)  = toStringA mcts ++ "- -" ++ IntMap.showTree (IntMap.map toStringA t)
    where toStringA mcts1@(MkMcts p s _ _ ) = (show p) ++ " score:" ++ (show s) ++ " mean" ++ show (s/p) ++ " ucb:" ++ (show (ucb1  (round plays1) mcts1))
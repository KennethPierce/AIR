module Mcts where
import qualified Data.IntMap as IntMap
import qualified Types as AirTypes
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
        ftp = 1 +  (fromIntegral totalPlays) -- adding 1 to prevent neg in log function below for zero totalplays
        mean = s/p
        explore =  (2*((log ftp)/p)) ** 0.5


-- start each tree with small random score to break ties and simplify algorithm
initMcts :: AirTypes.Game a => a -> RandInt -> Mcts a
initMcts g rand = MkMcts (100*epsilon1) epsilon2 g im 
    where 
        g1 = mkStdGen rand
        (r1:rs) = randoms g1
        g2 = mkStdGen r1
        (epsilon1:epsilon2:_) = randomRs (0.000001,0.00001) g2     :: [Double]
        im = IntMap.fromList [(m,initMcts (AirTypes.makeMove g m) r )| (m,r) <- zip (AirTypes.validMoves g) rs]

rollout :: (AirTypes.Game a,AirTypes.AI a) => a -> a 
rollout g = if AirTypes.isGameOver g then  g else rollout (AirTypes.makeMove g (AirTypes.chooseMove g))

getTopMcts :: (AirTypes.Game a,AirTypes.AI a) => Mcts a -> Int -> Int
getTopMcts mcts totPlays = idx
    where
        ucb1s = IntMap.map (ucb1 totPlays) (tree mcts)
        (idx,_) = IntMap.foldrWithKey maxScore (-1,0/1) ucb1s
        maxScore ::  Int -> Double  -> (Int,Double) -> (Int,Double)
        maxScore idx1 score1 (topIdx,topScore) = if score1 > topScore then (idx1,score1) else (topIdx,topScore)

mctsUpdate :: (AirTypes.Game a,AirTypes.AI a) => a  -> Int -> RandInt -> Mcts a
mctsUpdate game1 rollouts rand = mctsUpdate' mcts1 rollouts 
    where
        mcts1 = initMcts game1 rand
        mctsUpdate' :: (AirTypes.Game a,AirTypes.AI a) => Mcts a -> Int ->  Mcts a
        mctsUpdate' mcts 0  = mcts
        mctsUpdate' mcts ro = mctsUpdate' mctsU (ro-1)
            where
                (_,mctsU) = selectRollout mcts
                selectRollout :: (AirTypes.Game a,AirTypes.AI a) => Mcts a -> (a,Mcts a)
                selectRollout mcts'@(MkMcts p s g c) =  (endingGame,topMcts'')
                    where
                        idx = getTopMcts mcts' (rollouts-ro)
                        topMcts@(MkMcts _ _ topGame _) = c IntMap.! idx
                        (endingGame,topMcts')= if p < 1 then (rollout topGame,topMcts) else selectRollout topMcts
                        s' = AirTypes.scoreGame endingGame g
                        topMcts'' = MkMcts (p+1) (s+s') g (IntMap.insert idx topMcts' (tree mcts') )




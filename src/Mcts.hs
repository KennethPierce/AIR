module Mcts where
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.IntMap.Internal.Debug as IntMapDebug
import qualified Types as AirTypes
--import qualified Debug.Trace
import System.Random
type RandInt = Int

data Mcts a = MkMcts 
    { plays :: Double
    , score :: Double
    , game :: a
    , randValue :: RandInt
    , tree :: IntMap.IntMap (Mcts a)
    }

-- Give highest or lowest score to won games; otherwise extra rollouts might happen
ucb1 :: Int -> Mcts a -> Double
ucb1 totalPlays (MkMcts p s _ _ _)  = mean + explore
    where
        ftp = if totalPlays < 1 then 1 else  (fromIntegral totalPlays) -- prevent neg in log function below for zero totalplays
        mean = s/p -- the initial values of these will give a high mean score but negligable later
        explore =  (2*((log ftp)/p)) ** 0.5

-- start each tree with small random score to break ties and simplify algorithm
initMcts :: AirTypes.Game a => a -> RandInt -> Mcts a
initMcts g rand = MkMcts epsilon1 (100*epsilon2) g rand im 
    where 
        g1 = mkStdGen rand
        (r1:rs) = randoms g1
        g2 = mkStdGen r1
        (epsilon1:epsilon2:_) = randomRs (0.0000001,0.000001) g2     :: [Double]
        im = IntMap.fromAscList [(m,initMcts (AirTypes.makeMove g m) r )| (m,r) <- zip (AirTypes.validMoves g) rs]

rollout :: (AirTypes.Game a,AirTypes.AI a,Monad m) => m a -> [Double] -> m a 
rollout mg (r:rs) =  do 
    g <- mg
    if AirTypes.isGameOver g 
    then pure g 
    else  do 
             mi <- AirTypes.chooseMove mg r
             rollout (pure (AirTypes.makeMove g mi)) rs


getTopMctsByUcb1 :: (AirTypes.Game a,AirTypes.AI a) => Mcts a -> Int -> Int
getTopMctsByUcb1 mcts totPlays = getTopMcts mcts (ucb1 totPlays)

getTopMctsByPlays ::  Mcts a -> Int
getTopMctsByPlays mcts  = getTopMcts mcts plays

getTopMcts ::  Mcts a -> (Mcts a -> Double) -> Int
getTopMcts mcts f = idx
    where
        (idx,_) = IntMap.foldrWithKey maxScore (-1,0/1) (IntMap.map f (tree mcts))
        maxScore ::  Int -> Double  -> (Int,Double) -> (Int,Double)
        maxScore idx1 score1 (topIdx,topScore) = if score1 > topScore then (idx1,score1) else (topIdx,topScore)
    

selectRollout :: (AirTypes.Game a,AirTypes.AI a,Monad m)  => m (Mcts a) -> Int -> m (a,Mcts a)
selectRollout mMcts totPlays = do 
    mcts1@(MkMcts p1 s1 _ r1 t1) <- mMcts
    let 
        idx = getTopMctsByUcb1 mcts1 totPlays
        topMcts1@(MkMcts topPlays _ topGame r2 _) = t1 IntMap.! idx
        rs = randomRs (0,1) (mkStdGen r2)
    (endingGame,_topMcts'@(MkMcts p s g r t)) <- if topPlays < 1 then fmap (\x -> (x,topMcts1)) (rollout (pure topGame) rs) else selectRollout (pure topMcts1) totPlays
    let 
        s' = AirTypes.scoreGame endingGame topGame
        topMcts'' = MkMcts  (p+1) (s+s') g r t
        topMcts''' = MkMcts p1 s1 endingGame r1 (IntMap.insert idx topMcts'' (tree mcts1) )
    pure (endingGame,topMcts''')

mctsUpdate :: (AirTypes.Game a,AirTypes.AI a,Monad m) => a  -> Int -> RandInt -> m (Mcts a)
mctsUpdate game1 rollouts rand = do         
    mctsUpdate' mcts1 rollouts
    where
        mcts1 = pure (initMcts game1 rand)
        mctsUpdate' :: (AirTypes.Game a,AirTypes.AI a,Monad m) => m (Mcts a) -> Int -> m (Mcts a)
        mctsUpdate' mMcts 0  = do 
            mcts <- mMcts
            pure (mcts {plays= (plays mcts) + (fromIntegral rollouts)}) -- root node doesn't update score. shouldn't matter though; not used.
        mctsUpdate' mMcts ro = do 
                (_,mctsU) <- selectRollout mMcts (rollouts - ro)        
                mctsUpdate' (pure mctsU) (ro-1)

showMcts :: Mcts a -> String
showMcts mcts@(MkMcts plays1 _ _ _ t)  = toStringA mcts ++ "- -" ++ IntMapDebug.showTree (IntMap.map toStringA t)
    where toStringA mcts1@(MkMcts p s _ _ _ ) = (show p) ++ " score:" ++ (show s) ++ " mean" ++ show (s/p) ++ " ucb:" ++ (show (ucb1  (round plays1) mcts1))

{-
autoPlayRec :: (AirTypes.Game a,AirTypes.AI a) => a -> [Int] -> [RandInt] -> a 
autoPlayRec g (ro:ros) (ri:ris) = if AirTypes.isGameOver g then g else autoPlayRec g' ros ris
    where g' = AirTypes.makeMove g move
          move = getTopMctsByPlays mcts
          mcts = mctsUpdate g ro ri
-}



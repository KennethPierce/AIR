{-# LANGUAGE RankNTypes              #-}
module Mcts where
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.IntMap.Internal.Debug as IntMapDebug
import qualified Types as AirTypes
import qualified Data.Vector as V

--import qualified Debug.Trace
import System.Random
type RandInt = Int

data Mcts a = MkMcts 
    { plays :: Double
    , score :: Double
    , game :: a
    , tree :: IntMap.IntMap (Mcts a)
    }

type ChooseMove m a =  Monad m
    => m a      -- | GameState
    -> Double   -- | Random 
    -> m Int      -- | Move Chosen

emptyMcts :: a ->  Mcts a 
emptyMcts g = MkMcts 0 0 g IntMap.empty

-- Give highest or lowest score to won games; otherwise extra rollouts might happen
ucb1 :: Int -> Mcts a  -> Double
ucb1 _totalPlays (MkMcts 0 _ _ _) = 1/0
ucb1  totalPlays (MkMcts p s _ _)  = mean + explore
    where
        ftp = if totalPlays < 1 then 1 else  (fromIntegral totalPlays) 
        mean = s/p 
        explore =  (2*((log ftp)/p)) ** 0.5

initMcts :: AirTypes.Game a => a  -> Mcts a
initMcts g  = MkMcts 0 0 g im 
    where 
        im = IntMap.fromAscList [(m,initMcts (AirTypes.makeMove g m)  )| m <-  V.toList (AirTypes.validMoves g)]

-- | use selectMove to break ties (weight ties by moveWeights)
getTopMctsByUcb1 :: Mcts a -> V.Vector Double -> Int -> RandInt -> Int 
getTopMctsByUcb1 mcts mws totPlays rand = selectMove (V.fromList topMoves) mws rand
    where
        topMoves = getTopMcts mcts (ucb1 totPlays ) 

-- | select by largest number of plays
getTopMctsByPlays ::  Mcts a -> Int 
getTopMctsByPlays mcts  = idx
    where 
        (idx:_) = getTopMcts mcts plays 

-- | return top indexes
getTopMcts ::  Mcts a -> (Mcts a -> Double) -> [Int]
getTopMcts mcts f  = [i | (i,_) <- iscores]
    where
        iscores = IntMap.foldrWithKey maxScores [(-1,-1/0)] (IntMap.map f (tree mcts))
        maxScores ::  Int -> Double  -> [(Int,Double)] -> [(Int,Double)]
        maxScores i s l@((_,ts):_) = maxScores' (compare s ts) (i,s) l
            where 
                maxScores' GT is iss = [is]
                maxScores' EQ is iss = is:iss
                maxScores' LT is iss = iss
            
-- | 
selectMove :: V.Vector Int -> V.Vector Double -> RandInt -> Int
selectMove validMoves weightedMoves rand = selectMove' ivwms rd
    where 
        vwms = V.backpermute  weightedMoves validMoves
        s = V.sum vwms
        ivwms = V.toList (V.zip validMoves vwms)
        (rd:_) = randomRs (0,1) (mkStdGen rand)
        selectMove' []  _r = -1
        selectMove' ((idx,_w):[]) _ = idx
        selectMove' ((idx,w):wms) r = if r-(w/s) <= 0 then idx else selectMove' wms (r-(w/s))


rollout :: (AirTypes.Game a,AirTypes.AI a,Monad m) => AirTypes.MoveWeights m a -> a -> RandInt -> m a 
rollout fcm g rand =  do 
    let (r1:r2:_) = makeRands rand
    if AirTypes.isGameOver g 
    then pure g 
    else do
             mws <-  fcm g
             let i = selectMove (AirTypes.validMoves g) mws r1
             rollout fcm (AirTypes.makeMove g i) r2
    


updateMcts :: Mcts a -> Double -> Maybe (Int,Mcts a) -> Mcts a
updateMcts mcts scoreVal Nothing = mcts{plays=1 + (plays mcts),score=scoreVal+(score mcts)}
updateMcts mcts scoreVal (Just (idx,val)) = updateMcts mcts{tree=IntMap.insert idx val (tree mcts)} scoreVal Nothing

selectRollout :: (AirTypes.Game a,AirTypes.AI a,Monad m)  => AirTypes.MoveWeights m a -> Mcts a -> Int -> RandInt -> m (a,Mcts a)
selectRollout fcm mcts@(MkMcts 0 _ _ _) totPlays rand = do
    let (r1:_) = makeRands rand
    g <- rollout fcm (game mcts) r1
    let mcts' = updateMcts mcts (AirTypes.scoreGame g (game mcts)) Nothing
    pure (g,mcts')
selectRollout fcm mcts                        totPlays rand = do     
    mws <- fcm (game mcts)
    let (r1:r2:_) = makeRands rand
    let idx = getTopMctsByUcb1 mcts mws totPlays r2
    let ucbMcts = (tree mcts) IntMap.! idx 
    (g,ucbMcts') <- selectRollout fcm ucbMcts totPlays r1
    let mcts' = updateMcts mcts (AirTypes.scoreGame g (game mcts)) (Just (idx,ucbMcts') )
    pure (g,mcts')
        

runMcts :: (AirTypes.Game a,AirTypes.AI a,Monad m) => AirTypes.MoveWeights m a -> Mcts a -> Int -> RandInt -> m (Mcts a)
runMcts _fcm mcts 0 _rand = pure mcts
runMcts fcm mcts rollouts rand = do 
    let (r1:r2:_) = makeRands rand
    (_,mcts') <- selectRollout fcm mcts (truncate $ plays mcts) r1  
    runMcts fcm mcts' (rollouts-1) r2

makeRands :: Int -> [Int]    
makeRands rand = randoms (mkStdGen rand)

showMcts :: Mcts a -> String
showMcts mcts@(MkMcts plays1 _  _ t)  = toStringA mcts ++ "- -" ++ IntMapDebug.showTree (IntMap.map toStringA t)
    where toStringA mcts1@(MkMcts p s _  _ ) = (show p) ++ " score:" ++ (show s) ++ " mean" ++ show (s/p) ++ " ucb:" ++ (show (ucb1  (round plays1) mcts1))
{-

autoPlayRec :: (AirTypes.Game a,AirTypes.AI a) => a -> [Int] -> [RandInt] -> a 
autoPlayRec g (ro:ros) (ri:ris) = if AirTypes.isGameOver g then g else autoPlayRec g' ros ris
    where g' = AirTypes.makeMove g move
          move = getTopMctsByPlays mcts
          mcts = mctsUpdate g ro ri
-}



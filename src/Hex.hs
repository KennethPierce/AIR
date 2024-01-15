module Hex where
import qualified Data.Vector as V
import qualified Types as AirTypes
import qualified Mcts
import System.Random
import System.Random.Shuffle




data HexGame = MkHexGame
    { bs2    :: Int
    , marker :: V.Vector (V.Vector Bool)
    , group  :: V.Vector Int -- connected markers will all have same group number equal to lowest index
    } deriving (Show)



makeHexGame :: Int -> HexGame
makeHexGame bs = hg
  where
    g = V.generate (bs*bs) (\i -> i)
    m = V.replicate (bs*bs) False
    ms = V.replicate (2) m
    hg = MkHexGame bs ms g

{- | initial moves to create a boarder around the 'real' hex board. This make the win condition simple to calculate and better visualizations.
>>> initMoves 5
[1,0,2,5,3,10,4,15,20,9,21,14,22,19,23,24]
-} 

initMoves :: Int -> [Int]
initMoves bs = m
  where 
        p0 = [1..(bs-1)] ++ [(bs*bs-bs)..(bs*bs-2)]
        p1 = [0,bs..(bs*bs-bs-1)] ++ [bs+bs-1,bs+bs+bs-1..bs*bs-1]
        merge (l1:l1s) (l2:l2s) = l1:l2:merge l1s l2s        
        merge _ _ = []
        m = merge p0 p1


hgPlayGame :: Int -> [Int] -> [HexGame]
hgPlayGame bs ms = foldr makeMoves [makeHexGame bs] zs
  where
        zs = reverse $ zip ms (cycle [0,1])  -- reverse since we use foldr not foldl
        makeMoves :: (Int,Int) -> [HexGame] -> [HexGame]
        makeMoves (m,p) (hg:hgs) = (hgMakeMove p m hg):hg:hgs


initHexGame :: Int -> HexGame
initHexGame bs = head g
  where 
    m = initMoves bs
    g = hgPlayGame bs m

hgMakeMove :: Int -> Int -> HexGame -> HexGame
hgMakeMove pl move (MkHexGame bs ms g) = MkHexGame bs ms' g'
    where
        m = ms V.! pl
        m' = m V.// [(move,True)]
        ms' = ms V.// [(pl,m')]
        g' = V.map (\x -> if elem x connG then minG else x) g
          where
            dir = [(0,0),(-1,-1),(-1,0),(0,-1),(0,1),(1,0),(1,1)]
            (quot,rem) = quotRem move bs
            ddir = fmap (\(dr,dc) -> (quot + dr,rem+dc) ) dir
            validDir = [ r*bs + c | (r,c) <- ddir ,r >=0 , r <bs, c>=0, c<bs ]
            connI = filter (\i -> m' V.! i) validDir
            connG = fmap (\i -> g V.! i ) connI
            minG = minimum connG

hgIsGameOver :: HexGame -> Bool
hgIsGameOver (MkHexGame bs _ g) = p1Win || p2Win
    where 
        p1Win = (g V.! (bs*bs -2) ) == 1
        p2Win = (g V.! (bs*bs -1) ) == 0

hgValidMoves :: HexGame -> [Int]
hgValidMoves (MkHexGame bs ms _) = [i | i <- [0..bs-1], emptySq i]
  where
    emptySq i =not $ (ms V.! 0) V.! i || (ms V.! 1) V.! i



------------Utility functions


hgRandGame :: Int -> Int -> [Int]
hgRandGame bs seed = m ++ sh
    where
        m = initMoves bs
        sh = shuffle' ms   (length ms) (mkStdGen seed)
        ms = filter isEdge [0..bs*bs-1]
        isEdge i = not (bZ || bBS)
          where
            (r,c) = quotRem i bs
            bZ = r == 0 || c == 0
            bBS = r == (bs-1) || c == (bs-1)



hgPrintGroup :: Int -> [Int] -> IO ()
hgPrintGroup bs ms = putStrLn (unlines ls)
    where
        hgs = hgPlayGame bs ms
        ls :: [String]
        ls = tail $ reverse $ fmap (',':) (fmap (show.group) hgs)



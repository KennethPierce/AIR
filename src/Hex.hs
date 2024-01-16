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
    } deriving (Show,Eq)



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

{- | creates a new game with the list of moves. Players alternate moves.
-} 

hgPlayGame :: Int -> [Int] -> [HexGame]
hgPlayGame bs ms = foldr makeMoves [makeHexGame bs] zs
  where
        zs = reverse $ zip ms (cycle [0,1])  -- reverse since we use foldr not foldl
        makeMoves :: (Int,Int) -> [HexGame] -> [HexGame]
        makeMoves (m,p) (hg:hgs) = (hgMakeMove p m hg):hg:hgs

{- | creates a new game board with the initial moves already performed
-} 

initHexGame :: Int -> HexGame
initHexGame bs = head g
  where 
    m = initMoves bs
    g = hgPlayGame bs m

{- | makes a game board move and updates the groups field for easy win condition testing
groups algorithm
each hex is a member of a group which is represented by a unique number. 
When two or more groups are connected the hexes are all given the lowest group number.
-} 
hgMakeMove :: Int -> Int -> HexGame -> HexGame
hgMakeMove pl move (MkHexGame bs ms g) = MkHexGame bs ms' g'
    where
        m = ms V.! pl
        m' = m V.// [(move,True)]
        ms' = ms V.// [(pl,m')]
        g' = V.map (\x -> if elem x connG then minG else x) g
          where
            dir = [(0,0),(-1,-1),(-1,0),(0,-1),(0,1),(1,0),(1,1)]
            (qu,re) = quotRem move bs
            ddir = fmap (\(dr,dc) -> (qu + dr,re+dc) ) dir
            validDir = [ r*bs + c | (r,c) <- ddir ,r >=0 , r <bs, c>=0, c<bs ]
            connI = filter (\i -> m' V.! i) validDir
            connG = fmap (\i -> g V.! i ) connI
            minG = minimum connG

{- | a player wins when they connect a path of hex squares from their initial moves 
     for player 0 it is top - bottom
     for player 1 is it left - right
     in hex there are no ties possible. There is always a winner once all the moves are made.
-} 

hgIsGameOver :: HexGame -> Bool
hgIsGameOver (MkHexGame bs _ g) = p1Win || p2Win
    where 
        p1Win = (g V.! (bs*bs -2) ) == 1
        p2Win = (g V.! (bs*bs -1) ) == 0

{- | Any unoccupied hex can be selected
-} 
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

{- | Rotate a game board quarter turn and flip players.
This is simplify game AI and heuristics so everything will be with p0 as the focus
>>> let  rhg7 = head $hgPlayGame 7 (hgRandGame 7 7)
>>> let r4 = hgRotateGame.hgRotateGame.hgRotateGame.hgRotateGame
>>> rhg7 == r4 rhg7
True
-}

hgRotateGame :: HexGame -> HexGame
hgRotateGame (MkHexGame bs mss g) = MkHexGame bs mss' g'
  where
    mss' = V.fromList [doRot (mss V.! 1),doRot (mss V.! 0)]
    g' = fmap updateG doRotG
      where
        doRotG = doRot g
        ug = (V.generate (bs*bs) id) V.// (reverse [(v,i) | (i,v) <- V.toList (V.indexed doRotG)])
        updateG i = ug V.! i
    doRot v = V.fromList [v V.! (rot i) | i <- [0..bs*bs-1]]
    rot i = bs*(bs-re-1)+qu
      where 
        (qu,re) = quotRem i bs

module InRow where
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List
import qualified Types as AirTypes

boardSize :: Int
boardSize = 7
winLength :: Int
winLength = 5
boardSizeSq :: Int
boardSizeSq = boardSize*boardSize


data Square 
    = SQEmpty 
    | SQBlack 
    | SQWhite 
    deriving (Eq,Show,Enum)






instance AirTypes.AI InRow where
    scoreGame (MkInRow _ _ _ SQEmpty _ _ _) _ = 0.0
    scoreGame (MkInRow _ _ _ winw _ _ _) (MkInRow _ _ playerp _ _ _ _) = if winw == playerp then 1.0 else -1.0


instance AirTypes.Game InRow where
    isGameOver (MkInRow _ _ _ w _ _ c) = w /= SQEmpty || null c
    validMoves (MkInRow _ _ _ _ _ m _) = Set.toList m
    makeMove inrow move = (children inrow) !! (Set.findIndex move (moves inrow))





type Board = (V.Vector Bool,V.Vector Bool)
type WinState = (V.Vector Int,V.Vector Int,V.Vector Int,V.Vector Int) -- | horizontal virtical left-diagonal right-diagonal
type WinStates = (WinState,WinState) 
type History = [Int]
type Moves = Set.Set Int

data InRow = MkInRow 
    { board :: Board
    , winState :: WinStates
    , player :: Square
    , won :: Square
    , history :: History
    , moves :: Moves
    , children :: [InRow]
    }

scoreGameSimple  :: InRow -> Double
scoreGameSimple (MkInRow _ (wsb,wsw) pg _ _ _ _) = if pg == SQBlack then scoreWinState wsb wsw else scoreWinState wsw wsb
    where
        scoreWinState pl opp = score pl opp pos - score opp pl neg
        pos = [0,1,4,9,16,125]
        neg = [0,1,4,9,75.150]
        score (p1,p2,p3,p4) (o1,o2,o3,o4) points = sc p1 o1 + sc p2 o2 + sc p3 o3 + sc p4 o4
            where 
                sc p o = V.foldl calc 0.0 (V.zip p o)
                calc s (pl,opp) = s + (if opp == 0 then (points !! pl) else 0.0) 


autoPlay :: InRow -> IO ()
autoPlay inrow@(MkInRow _ _ _ _ _ _ []) = putStr $ inRowToString inrow 
autoPlay inrow@(MkInRow _ _ _ SQEmpty _ _ _) = do 
    putStr $ inRowToString inrow    
    autoPlay (snd (List.maximumBy (\(a,_) (b,_) -> compare a b  ) zw))
        where 
            c = children inrow
            zw = fmap (\ir -> (scoreGameSimple ir,ir)) c
autoPlay inrow = putStr $ inRowToString inrow             

boardIndex :: Board -> Int -> Square
boardIndex (bb,bw) i = toSquare (bb V.! i) (bw V.! i)
    where 
        toSquare True True = SQEmpty
        toSquare True _ = SQBlack
        toSquare _ True = SQWhite
        toSquare _ _ = SQEmpty

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

showMatrixInt :: Int -> Int -> V.Vector Int -> String
showMatrixInt r c v = unlines $ do 
    i <- [0..r-1]
    return $ show $ [v V.! (i*c+j) | j <- [0..c-1]]

inRowToString :: InRow -> String 
inRowToString inrow@(MkInRow b ws p w _ _ _) = unlines [pPlayer,pBoard,pWinState]
    where
        pPlayer = "player: " ++ show p ++ " winner:" ++ show w ++ " score: " ++ (show (scoreGameSimple inrow))
        pBoard = showBoard b
        pWinState = unlines 
            [ showMatrixInt 7 3 h
            , showMatrixInt 3 7 v
            , showMatrixInt 3 3 ld
            , showMatrixInt 3 3 rd
            ]
            where
                (wsb,wsw)  = ws
                (h,v,ld,rd) = if p == SQBlack then wsb else wsw

printGame :: [Int] -> InRow -> IO ()
printGame [] ir = putStr $ inRowToString ir
printGame (x:xs) ir = do
    putStr $ inRowToString ir
    printGame xs ((children ir) !! x)


winStateUpdate :: [ ([Int],[Int],[Int],[Int])]
winStateUpdate =  wss
    where 
        wsh :: (Int,Int)-> [Int]
        wsh (row,col) = fmap (rowAdd+) [b..e]
            where 
                rowAdd = row*(boardSize - winLength + 1)
                b = if col - winLength < 0 then 0 else col - winLength + 1
                e = if col + winLength > (boardSize -1) then boardSize - winLength  else col 
        wsd :: (Int,Int) -> [Int]
        wsd (r,c) = wsdd !! (r*boardSize + c)
        wsdd :: [[Int]]
        wsdd = 
            [ [0],[3]  ,[5]    ,[]     ,[]     ,[]   ,[]
            , [6],[0,1],[3,4]  ,[5]    ,[]     ,[]   ,[]
            , [8],[6,7],[0,1,2],[3,4]  ,[5]    ,[]   ,[]
            , [] ,[8]  ,[6,7]  ,[0,1,2],[3,4]  ,[5]  ,[]
            , [] ,[]   ,[8]    ,[6,7]  ,[0,1,2],[3,4],[5]
            , [] ,[]   ,[]     ,[8]    ,[6,7]  ,[1,2],[4]
            , [] ,[]   ,[]     ,[]     ,[8]    ,[7]  ,[2]
            ]
                    
        wss :: [([Int],[Int],[Int],[Int])]
        wss = do 
            idx <- [0..(boardSizeSq-1)]
            let (r,c) = quotRem idx boardSize
            pure (wsh (r,c),wsh (c,r),wsd(r,c),wsd(r,boardSize-c-1)) 

makeMoveInRow :: Int -> InRow -> InRow
makeMoveInRow move (MkInRow b (wsb,wsw) p w h m _) = inRow'
    where
        p' = if p == SQWhite then SQBlack else SQWhite
        b' = updateB b
            where updateB (bb,bw) = if p' == SQBlack then (bb V.// [(move,True)],bw) else (bb,bw V.// [(move,True)])
        (aa',bb',cc',dd') = winStateUpdate !! move
        (wsb',wsw') = if p' == SQBlack then (updateWS wsb ,wsw) else (wsb,updateWS wsw )
            where 
                updateWS :: WinState -> WinState
                updateWS (aa,bb,cc,dd) =  (upd aa aa',upd bb bb',upd cc cc',upd dd dd')
                    where
                        upd as as' = V.accum (+) as [(i,1) | i <- as']
        w' = if isWinner then p' else w
            where 
                isWinner = if p' == SQBlack then checkWinner wsb' else checkWinner wsw'
                checkWinner (aa,bb,cc,dd) = or [check aa aa',check bb bb',check cc cc', check dd dd']
                    where check vect list = or [winLength == (vect V.! i) | i <- list]
        h' = move:h
        m' = Set.delete move m 
        inRow' = MkInRow b' (wsb',wsw') p' w' h' m' []

initInRowRec :: InRow -> InRow
initInRowRec inRow@(MkInRow b ws p w h m _) = MkInRow b ws p w h m cs 
    where cs = do 
                move <- Set.toList m 
                let inRow' = makeMoveInRow move inRow
                pure (initInRowRec inRow')


initializeInRow :: InRow 
initializeInRow = initInRowRec inRow
    where    
        inRow = MkInRow b ws p w h m c
        b = (falseV,falseV) 
            where falseV = V.replicate boardSizeSq False
        --
        ws = (wsp,wsp)
            where
                wsp = (zeroVhv,zeroVhv,zeroVd,zeroVd)
                zeroVhv = V.replicate (boardSize*numWins) 0
                zeroVd = V.replicate (numWins*numWins) 0
                numWins = boardSize-winLength+1

        --
        p = SQBlack
        w = SQEmpty
        h = []
        m = Set.fromList [0..(boardSizeSq-1)]
        c = []



{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where


import Lib
import qualified System.Console.CmdArgs as Args
--import qualified Mcts 
--import qualified InRow
import qualified Diagrams.Prelude as Diag
import qualified Diagrams.Backend.SVG.CmdLine as Diag
--import qualified Diagrams.Coordinates as Diag
import qualified Hex
import qualified Data.Vector as V
import System.Random


data MctsGames =  MctsGames 
    { gamesToPlay :: Int
    , rollouts :: Int
    , beginMoves :: Int
    , testPost :: Bool
    }  deriving (Show, Args.Data, Args.Typeable)

mctsGames :: MctsGames
mctsGames = MctsGames 
    { gamesToPlay = 10 Args.&= Args.help "number of concurrent games to play."
    , rollouts    = 10 Args.&= Args.help "number of random game rollouts per move"
    , beginMoves  = 8  Args.&= Args.help "number of random starting moves for each game"
    , testPost    = False Args.&= Args.help "http post request with simple board state"
    }


makeHexBoard :: Int -> [Int] -> Diag.Diagram Diag.B
makeHexBoard bs ms =  blueLine Diag.# Diag.alignL Diag.=== (diagHex  `Diag.atop` diagPath) 
    where
        blueLine = Diag.hrule bs'  Diag.# Diag.lc Diag.blue 
        bs' = 2 * (fromIntegral  bs-1)
        points = [Diag.p2 (j,-i) | i <- [0,2..bs'], j <- [i/2,(i/2)+2..bs'+(i/2)]]
        diagPath = Diag.fromVertices points
        diagHex = Diag.atPoints points [selectHex i | i <- ms]

        selectHex i = if i == 0 then hex1 else if mod i 2 == 0 then hexRed else hexBlue
        
        hex1 = Diag.hexagon 1 Diag.# Diag.rotateBy (1/12)
        hexRed =  Diag.hexagon 1 Diag.# Diag.rotateBy (1/12) Diag.# Diag.fc Diag.red
        hexBlue = Diag.hexagon 1 Diag.# Diag.rotateBy (1/12) Diag.# Diag.fc Diag.blue



main :: IO()
main = do
    rand <- getStdGen
    let range = (1,1000) :: (Int,Int)
    let rs = randomRs range rand
    let hb = take (13*13) rs
    let vhb = V.fromList hb
    let p1 = Hex.isPath Hex.P1 Hex.P1 vhb 13
    let p2 = Hex.isPath Hex.P2 Hex.P2 vhb 13
    let color = if p1 then if p2 then Diag.black else Diag.blue else if p2 then Diag.red else Diag.yellow
    Diag.mainWith $ Diag.circle 1 Diag.# Diag.fc color Diag.=== makeHexBoard 13 hb
    

main2 :: IO () 
main2 = Diag.mainWith $ (makeHexBoard 13 [1..13*13]) Diag.||| (makeHexBoard 13 (showMove 91)) Diag.||| (makeHexBoard 13 (showMove (13*13-13)))
    where 
        showMove m = V.toList (e V.// l)
            where 
                e = V.replicate (13*13) 0
                l = (m,1):[(i,2)|i<-Hex.connected 13 (Hex.P m)]



main1 :: IO () 
main1 = do 
    ca  <- Args.cmdArgs mctsGames
    doMain ca
    where
        doMain (MctsGames _gcnt _rcnt _bMoves True) = inferPost
        doMain (MctsGames gcnt rcnt bMoves _) = selfPlaysIO gcnt rcnt bMoves

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


main :: IO ()
main = undefined
    

main2 :: IO () 
main2 = undefined



main1 :: IO () 
main1 = do 
    ca  <- Args.cmdArgs mctsGames
    doMain ca
    where
        doMain (MctsGames _gcnt _rcnt _bMoves True) = inferPost
        doMain (MctsGames gcnt rcnt bMoves _) = selfPlaysIO gcnt rcnt bMoves

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where


import Lib
import qualified System.Console.CmdArgs as Args

data MctsGames = MctsGames 
    { gamesToPlay :: Int
    , rollouts :: Int
    , testPost :: Bool
    }  deriving (Show, Args.Data, Args.Typeable)

mctsGames :: MctsGames
mctsGames = MctsGames 
    { gamesToPlay = 10 Args.&= Args.help "number of concurrent games to play."
    , rollouts    = 10 Args.&= Args.help "number of random game rollouts per move"
    , testPost    = False Args.&= Args.help "http post request with simple board state"
    }

main :: IO () 
main = do 
    (MctsGames gcnt rcnt tp) <- Args.cmdArgs mctsGames
    if tp 
    then
        inferPost
    else
        selfPlaysIO gcnt rcnt


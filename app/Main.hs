{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where


import Lib
import qualified System.Console.CmdArgs as Args

data MctsGames = MctsGames 
    { gamesToPlay :: Int
    , rollouts :: Int
    }  deriving (Show, Args.Data, Args.Typeable)

mctsGames :: MctsGames
mctsGames = MctsGames 
    { gamesToPlay = 10 Args.&= Args.help "number of concurrent games to play."
    , rollouts    = 10 Args.&= Args.help "number of random game rollouts per move"
    }

main :: IO ()
main = do 
    (MctsGames gcnt rcnt )<- Args.cmdArgs mctsGames
    selfPlaysIO gcnt rcnt

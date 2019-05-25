--multi-armed bandit slot machine game

module Bandit where
import qualified Types as AirTypes    



data Bandit = MkBandit 
    { plays   :: Int
    , score :: Double
    , bandits :: [[Double]] 
    }

initBandit :: Bandit
initBandit = MkBandit 0 0 [cycle [1],cycle [1,0],cycle [0]]

instance  AirTypes.Game Bandit where
    isGameOver (MkBandit _ _ _) = True

    --validMoves _ = [0..2]

    makeMove (MkBandit p _ b) idx = MkBandit (p+1) ((b !! idx) !! p) (repeat (b!!idx)) 

instance AirTypes.AI Bandit where
    scoreGame  (MkBandit _ s _) _ = s


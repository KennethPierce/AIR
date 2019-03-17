module Types where
import qualified Data.ByteString.Lazy as BL


class AI a where
    inferGame :: a -> BL.ByteString

    trainGame :: a -> BL.ByteString

    chooseMove  :: Monad m
                => m a      -- | GameState
                -> Double   -- | Random 
                -> m Int      -- | Move Chosen

    scoreGame :: a      -- | winning game (endgame)
              -> a      -- | player game (used in multiplayer games)
              -> Double -- | game scored for that player

class Game a where
    isGameOver :: a -> Bool

    validMoves :: a -> [Int]

    makeMove :: a -> Int -> a
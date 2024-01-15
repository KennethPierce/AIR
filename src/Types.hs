{-# LANGUAGE RankNTypes              #-}


module Types where
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

type MoveWeights m a =  Monad m
    => a              -- ^ GameState
    -> m (V.Vector Double)     -- ^ Move weights


class AI a where
    inferGame :: a -> BL.ByteString

    trainGame :: a -> BL.ByteString

    moveWeights  :: MoveWeights m a

    scoreGame :: a      -- ^ winning game (endgame)
              -> a      -- ^ player game (used in multiplayer games)
              -> Double -- ^ game scored for that player

class Game a where
    isGameOver :: a -> Bool

    validMoves :: a -> V.Vector Int

    makeMove :: a -> Int -> a
module Types where
import qualified Data.ByteString.Lazy as BL


class AI a where
    inferGame :: a -> BL.ByteString

    trainGame :: a -> BL.ByteString

    chooseMove :: a -> Int

    scoreGame :: a -> a -> Double

class Game a where
    isGameOver :: a -> Bool

    validMoves :: a -> [Int]

    makeMove :: a -> Int -> a
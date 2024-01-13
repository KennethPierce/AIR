

module HexSpec where
import Test.Hspec
import Test.QuickCheck
import qualified Hex
import qualified Types as AirTypes
import qualified Data.Vector as V

spec :: Spec 
spec =  do
    describe "Hex isPath" $ do
        it "has a winner on a non zero board" $ do 
            let 
                bs = 7
                makeFilledHex [] = makeFilledHex [1..bs]
                makeFilledHex xs = Hex.MkHex (Hex.MkConfig bs) (V.fromList (take (bs*bs) [if i == 0 then 1 else i | i <- cycle xs])) bs                
            property $ \xs -> AirTypes.isGameOver (makeFilledHex xs) == True
        it "empty board has no winner" $ do 
            AirTypes.isGameOver (Hex.createHex 5) `shouldBe` False
            AirTypes.isGameOver (Hex.createHex 1) `shouldBe`  False

    describe "scoreGame should be 1 for winner and -1 for loser" $ do
        it "Make 1x1 hex board and play one move to win"  $ do
            let 
                startHex = Hex.MkHex (Hex.MkConfig 1) (V.fromList [0]) 0
                winHex   = AirTypes.makeMove startHex 0
            AirTypes.scoreGame winHex startHex `shouldBe` -1
            AirTypes.scoreGame winHex winHex   `shouldBe` 1

    describe "Hex connected" $ do
        it "P1 connects to top side" $
            Hex.connected 3 Hex.P1 `shouldBe` [0,1,2]
  
        it "P2 connects to left side" $
            Hex.connected 3 Hex.P2 `shouldBe` [0,3,6]
  
        it "Top left connects to right and down" $
            Hex.connected 3 (Hex.P 0) `shouldBe` [1,3]
  
        it "Top right connects to left, left-down,down" $
            Hex.connected 3 (Hex.P 2) `shouldBe` [1,4,5]
  
        it "Bottom left connects to _" $
            Hex.connected 3 (Hex.P 6) `shouldBe` [3,4,7]                        

        it "Bottom right connects to _" $
            Hex.connected 3 (Hex.P 8) `shouldBe` [5,7]                                    

        it "Middle connects to _" $
            Hex.connected 3 (Hex.P 4) `shouldBe` [1,2,3,5,6,7]                                                
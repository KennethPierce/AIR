module MctsSpec where
import Test.Hspec
import Test.QuickCheck
import qualified Mcts


spec :: Spec 
spec =  do 
    describe "ucb1" $ do
        it "explore value should only value with 1 total plays"  $ do 
            let m = Mcts.emptyMcts () 
            let m1Win = m{Mcts.plays=1,Mcts.score=1}
            let m1Lose = m{Mcts.plays=1,Mcts.score=0}
            Mcts.ucb1 1 m1Win `shouldBe` 1
            Mcts.ucb1 1 m1Lose `shouldBe` 0
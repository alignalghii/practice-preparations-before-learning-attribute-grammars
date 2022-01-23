module LazyLetRec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Combinators (Transform, fixpointCombinator)


-- Seemingly meaningless let-rec constructs can be usually implemented correctly with fixpoint combinator calls on an appropriate precursor function
spec :: Spec
spec = describe "The lazy let-rec construct" $ do
    it "is equivalent to a fixpoint combinator call on a function with irrefutable patterns" $ do
        letRec_construct `shouldBe` fixpoint_call

letRec_construct, fixpoint_call :: (Int, Int)
letRec_construct = let (m, n) = (0, m) in (m, n) -- seems to be paradoxical, meaningless, or infinitely running
fixpoint_call = fixpointCombinator letRec_precursor -- has a terminating lambda-calculus definition under lazy evaluation

-- Precursor function of the sample lazyletrec construct
letRec_precursor :: Transform (Int, Int)
letRec_precursor ~(m, _) = (0, m) -- won't work without irrefutable pattern: if lacking it, it will not terminate




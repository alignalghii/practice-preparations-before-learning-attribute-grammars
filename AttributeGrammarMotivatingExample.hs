module AttributeGrammarMotivatingExample where

import Test.Hspec (Spec, describe, it, shouldBe)
import Combinators (Transform, combinatorS)
import Control.Arrow ((***))
import Control.Monad (mapM_)


-- Wouter Swiestra: [Why attribute grammars matter](https://wiki.haskell.org/The_Monad.Reader/Issue4/Why_Attribute_Grammars_Matter)
spec :: Spec
spec = describe "`Why attribute grammars matter' article's roadmap demonstration for variants of equalize function increasingly involved in AG theory" $ do
    mapM_ demonstrateVersion [("naive unoptimized (3-traversal)"              , equalize   ),
                              ("partially optimized (2-traversal)"            , equalize'  ),
                              ("optimized (single-traversal)"                 , equalize'' ),
                              ("attribute grammar inspired (single-traversal)", equalize''')]

type Label = String

demonstrateVersion :: (Label, Transform [Float]) -> Spec
demonstrateVersion (label, equalizeVersion) = describe (label ++ " version") $ do
    it "works for empty list" $ do
        equalizeVersion [] `shouldBe` []
    it "works for singleton list" $ do
        equalizeVersion [7] `shouldBe` [7]
    it "works for pairs" $ do
        equalizeVersion [1, 5] `shouldBe` [3, 3]


-------------------------------------------
-- Naive unoptimized (3-traversal) version:
-------------------------------------------

equalize :: Transform [Float]
equalize lst = let sum_   = sum lst -- No scope shadowing: let-rec is not a simple lambda bind, rather it's an implicit fixpoint combinator
                   count  = length lst
               in distribute count sum_

distribute :: Int -> Float -> [Float]
distribute n s = replicate n (s / fromIntegral n)


---------------------------------------------
-- Partially optimized (2-traversal) version:
---------------------------------------------

equalize' :: Transform [Float]
equalize' = uncurry distribute . countAndTaxate

countAndTaxate :: Num a => [a] -> (Int, a)
countAndTaxate = foldr ((***) succ . (+)) (0, 0)
-- lengthSum [] = (0, 0)
-- lengthSum (a : as) = (succ *** (+ a)) $ lengthSum as


----------------------------------------
-- Optimized (single-traversal) version:
----------------------------------------

equalize'' :: Transform [Float]
equalize'' lst = let cons x (count, sum, plugAvg) = (succ count, x + sum, \avg -> avg : plugAvg avg) -- combinatorS (:) plugAvg
                     nil                          = (         0,       0, \avg ->       []         ) -- const []
                     (count, sum, plugAvg)        = foldr cons nil lst
                 in plugAvg (sum / fromIntegral count)


-------------------------------------------------
-- Attribute grammar inspired (single-traversal):
-------------------------------------------------

equalize''' :: Transform [Float]
equalize''' lst = let cons x withAvg   = \avg -> let (     count,     sum,       xs) = withAvg avg
                                                 in  (succ count, x + sum, avg : xs)
                      nil              = \_   ->     (         0,       0,       [])
                      (count, sum, xs) = foldr cons nil lst (sum / fromIntegral count)
                  in xs

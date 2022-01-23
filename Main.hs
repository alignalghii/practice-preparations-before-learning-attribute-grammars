module Main where

import Test.Hspec (hspec, describe)
import LazyLetRec (spec)
import AttributeGrammarMotivatingExample (spec) -- the main content is here


-- Wouter Swiestra: [Why attribute grammars matter](https://wiki.haskell.org/The_Monad.Reader/Issue4/Why_Attribute_Grammars_Matter)
main :: IO ()
main = hspec $ do
    describe "A prerequisite to understand Wouter Swierstra's article is to understand the lazy let-rec construct" $ do
        LazyLetRec.spec
        AttributeGrammarMotivatingExample.spec

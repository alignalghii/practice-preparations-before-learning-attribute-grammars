Wouter Swierstra: [Why attribute grammars matter](https://wiki.haskell.org/The_Monad.Reader/Issue4/Why_Attribute_Grammars_Matter)

The simple writing down of the examples (with a little simplification) can be seen here in this repository.

The small simplicifcation: the motivating example is simplified very slightly:

- Write an `equalize` function, which takes a list of real (Float) numbers, and replaces each item with the overall average.
In short, this is a „communist” function: takes the populace of a country (a list of their incomes), and produces a list of the same length, with each item replaced to the average income.
- Implement this with as few traversals as You can.

In the source code, (see module [AttributeGrammarMotivatingExample](https://github.com/alignalghii/practice-preparations-before-learning-attribute-grammars/blob/main/AttributeGrammarMotivatingExample.hs)), I am using auxiliar functions named with the talking narrative names:

```haskell
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
```

These are the unoptimalized solution (3-traversal), and after that, a partially optimized solution (2-traversal).
the names of the auxiliary functions are talking names:
- Unoptimized version: a communist income policiy consists of counting the people (1st traversal), gathering their incomes into a result sum (2nd traversal, and distributing back, each person becoming the average).
- Also the partially optimized version uses talking names: we count and taxate the people simultaneously (1st traversal), then distribute back (2nd traversal). The main pint is to introduce a temporary data type to store the current count and sum continuously during the traversal. We will see that this idea will have an analogy on the Writer monad and monad transformer: representing important surplus information with tuples (or augmenting an existing tuple with one more slot). 

of course the main point is to show the single-traversal version. The author presents two variants.

The first variant extends the idea seem before:

```haskell
----------------------------------------
-- Optimized (single-traversal) version:
----------------------------------------

equalize'' :: Transform [Float]
equalize'' lst = let cons x (count, sum, plugAvg) = (succ count, x + sum, \avg -> avg : plugAvg avg) -- combinatorS (:) plugAvg
                     nil                          = (         0,       0, \avg ->       []         ) -- const []
                     (count, sum, plugAvg)        = foldr cons nil lst
                 in plugAvg (sum / fromIntegral count)
```

There are two novelties with this solution: the tuple is longer with one slot, and this slot is not a mere value but a parametizable value. This is something the combination of the basic idea behind a Reader and a Writer monad: the mere existence of a surplus slot is a writing, while the parametrizablilty of the value is a reading.

The second version of the single traversal implementation is done by factoring out the parametrizability from the third slot to the tuple itself:

```haskell
-------------------------------------------------
-- Attribute grammar inspired (single-traversal):
-------------------------------------------------

equalize''' :: Transform [Float]
equalize''' lst = let cons x withAvg   = \avg -> let (     count,     sum,       xs) = withAvg avg
                                                 in  (succ count, x + sum, avg : xs)
                      nil              = \_   ->     (         0,       0,       [])
                      (count, sum, xs) = foldr cons nil lst (sum / fromIntegral count)
                  in xs
```

Although the outfactorng itself is straightforward, there is a new and steep learning obstacle at another place:
the main `let`... `in`... constructs becomes seemingly strange, self-referent:

```haskell
let (count, sum, xs) = ... sum ... count
in xs 
```

It works, but why? We know, — and the author mentions too, — that lazy evaluation can handle this, and the main reason is that some of the variables do not depend from the others (are constant to the parametrization), and the dependencies are so that lazy evaluation can untangle these hidden dependecies and indepenedencies. But this is just a feeling, we may want a formal understanding too. So it seems worth looking more deeply behind Haskell's lazy ``let`` construct, capable of encoding whole recursion (the *let-rec* topics).

let us start the Haskell compliler interactively — e.g. by starting  `ghci` — and type in:

```
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Prelude> omega = omega
Prelude> omega
^CInterrupted.
Prelude> let omega = omega in omega
^CInterrupted.
Prelude> let omega = omega in 12
12
Prelude> y f = f (y f)
Prelude> y id
^CInterrupted.
Prelude> omega = y id
Prelude> omega
^CInterrupted.
Prelude> let (a, b) = (12, a) in (a, b)
(12,12)
Prelude> cross (a, b) = (12, a)
Prelude> cross (231,6784)
(12,231)
Prelude> y cross
*** Exception: stack overflow
Prelude> cross undefined 
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:13:7 in interactive:Ghci8
Prelude> let (a, b) = cross (a, b) in (a, b)
(12,12)
Prelude> cross $ cross $ cross $ cross (a, b)

<interactive>:15:32: error: Variable not in scope: a

<interactive>:15:35: error: Variable not in scope: b
Prelude> cross $ cross $ cross $ cross undefined 
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:16:31 in interactive:Ghci9
Prelude> cross ~(a, b) = (12, a)
Prelude> cross $ cross $ cross $ cross undefined 
(12,12)
Prelude> y cross
(12,12)
Prelude> 
```

In summary. Haskell's lazy `let` construct implements a potential recursion construct in a hidden, implicit way (it is a *let-rec*). In pure lambda-calculus, it could be translated into pure lambda calculus terms with the use of the **Y** fixpoint-combinator. Its semantics can be seen from the above `ghci` session, and also from the [LazyLetRec](https://github.com/alignalghii/practice-preparations-before-learning-attribute-grammars/blob/main/LazyLetRec.hs) module of this little miniproject.

This miniproject of mine simply tries out the code what Wouter Swierstra showed in his article „[Why attribute grammars matter](https://wiki.haskell.org/The_Monad.Reader/Issue4/Why_Attribute_Grammars_Matter)”.

The simple writing down of the examples (with a little simplification) can be seen here in this repository.

The small simplification: the motivating example is simplified very slightly:

- Write an `equalize` function, which takes a list of real (`Float`) numbers, and replaces each item with the overall average.
In short, this is a „communist” function: takes the populace of a country (a list of their incomes), and produces a list of the same length, with each item replaced to the average income.
- Implement this with as few traversals as You can.

In the source code, (see module [AttributeGrammarMotivatingExample](https://github.com/alignalghii/practice-preparations-before-learning-attribute-grammars/blob/main/AttributeGrammarMotivatingExample.hs)), I am using auxiliary functions named with the talking narrative names:

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

These are the unoptimized solution (3-traversal), and after that, a partially optimized solution (2-traversal).
The names of the auxiliary functions are talking names:

- Unoptimized version: a communist income policy consists of counting the people (1st traversal), gathering their incomes into a result sum (2nd traversal), and distributing back (3rd traversal, although this is a building traversal hiding behind `iterate`), each person becoming the average.
- Also the partially optimized version uses talking names: we count and taxate the people simultaneously (1st traversal), then distribute back (2nd traversal). The main point is to introduce a temporary data type to store the current count and sum continuously during the first traversal. We will see that this idea will have an analogy in the shape of the `Writer` monad and `WriterT` monad transformer: representing important surplus information with tuples (or augmenting an existing tuple with one more slot).

Of course the main gloal is to demonstate a single-traversal version. Yes, there exists such an optimization, moreover, the author presents two variants.

The first variant extends the idea just seen above:

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

There are two novelties with this solution: the tuple is longer with one slot, and this slot is not a mere value but a parametrizable value. This is something the combination of the basic idea behind a `Reader` and a `Writer` monad: the mere existence of a surplus slot is a writing-idea, while the parametrizablilty of the value is a reading-idea.

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

Although the outfactoring refactory itself is straightforward, but there is a new and steep learning obstacle at another place:
the main `let`... `in`... constructs becomes seemingly strange, paradoxical, self-referent:

```haskell
let (count, sum, xs) = ... sum ... count
in xs
```

It works: it terinates with the correct result (see the unit tests in all three modules, start [from here](https://github.com/alignalghii/practice-preparations-before-learning-attribute-grammars/blob/main/Main.hs)), it does not run into an infinite runaway, but how can it work at all? Why is it not a syntax error, and if it evades syntax check, how can it terminate at all? We know, — and the author mentions too, — that lazy evaluation can handle this, so it is not a menaingless thing: the main reason is that some of the variables do not depend contentually from the others (because are constant to the parametrization), thus the dependencies are so that lazy evaluation can untangle these hidden dependecies and indepenedencies. But this is just a feeling, we may want a formal understanding too. So it seems worth looking more deeply behind Haskell's lazy ``let`` construct, capable of encoding whole recursion (the *let-rec* topics).

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

In summary: Haskell's lazy `let` construct implements a potential recursion construct in a hidden, implicit way (it is a *let-rec*). In pure lambda-calculus, it could be translated into pure lambda calculus terms with the use of the **Y** fixpoint-combinator. Its semantics can be seen from the above `ghci` session, and also from the [LazyLetRec](https://github.com/alignalghii/practice-preparations-before-learning-attribute-grammars/blob/main/LazyLetRec.hs) module of this little miniproject.

Let us see a minimal example for a seemingly paradoxical `let`... `in` expression:

```haskell
let (a, b) = (1, a) in (a, b)
```

here the variable *a* is present both on the left-hand-side and the right-hand side, still, the expression terminates with `(1, 1)`. How does it do it?

Let us simulate the let-rec construct with the fixpoint combinator:

```haskell
letRec_construct, fixpoint_call :: (Int, Int)
letRec_construct = let (m, n) = (0, m) in (m, n) -- seems to be paradoxical, meaningless, or infinitely running
fixpoint_call = fixpointCombinator letRec_precursor -- has a terminating lambda-calculus definition under lazy evaluation

-- Precursor function of the sample lazyletrec construct
letRec_precursor :: Transform (Int, Int)
letRec_precursor ~(m, _) = (0, m)
```

where, of course, the famous fixpoint combinator is:

```haskell
-- The fixpoint combinator Y, see lambda calculus or combinatory logic
fixpointCombinator :: (a -> a) -> a
fixpointCombinator f = f $ fixpointCombinator f
```

It works (but note the irrefutable pattern at the `letRec_precursor`, it does not work without it!)
Still, we do not really see the underlying deduction precisely and formally. Let us translate everything to pure combinatory logic!

Let us begin with the fixpoint combinator:

- **X** ≡ λ*x f*.*f* (*x x f*)
- **Y** ≡ **X** **X**

The desired deduction is that **Y** _f_ must reduce to _f_(**Y** _f_), and that is achieved, because  **X X** _f_ indeed reduces to _f_(**X X** _f_), and we remember that **Y** is defined as and is identical to **X X**.

Now let us define the ordered pair:

- **pair** ≡ λ*x y f*._f x y_
- **fst** ≡ λ*p*._p_ **K**
- **snd** ≡ λ*p*._p_ (**K I**)

The desired dueductions here are: for anything _α_ and _β_, we expect both
- **fst** (**pair** _α β_) reducing to _α_
- **snd** (**pair** _α β_) reducing to _β_
and both hold true.

now let us encode the seemingly paradoxical let-rec construct `let (m, n) = (0, m) in (m, n)` into pure lambda calculus:

**let-rec-example** ≡ **Y** **let-rec-example-precursor**

where

**let-rec-example-precursor** ≡ λ⟨*a*, *b*⟩.⟨1, *a*⟩

which latter is of course a syntactic sugar, it must be translated to pure lambda calculus so:

**let-rec-example-precursor** ≡ λ*p*.**pair** 1 (**fst** _p_)


What we expect is **let-rec-example** terminating under lazy evaluation (normal-order reduction stategy, leftmost-outermost reduction strategy), and that holds true. let us use the syntactic sugar of function composition:

**let-rec-example-precursor** ≡ λ*p*.**pair 1** (**fst** _p_) ≡ **pair 1** ⋅ **fst**

The syntactic sugar of using the composition ⋅ operator can be justified because it can be encoded inside pure lambda terms with λ*f g x*._f_(_gx_).

Now **let-rec-example** being identic to **Y** **let-rec-example-precursor** reduces to  (**pair** 1 ⋅ **fst**) (**pair** 1 ⋅ **fst**) (**pair** 1 ⋅ **fst**) (**pair** 1 ⋅ **fst**) ..., in short (**pair** 1 ⋅ **fst**) (**pair** 1 ⋅ **fst**) ..., but this potentialy infinite term must reduce only till the first two subterms (lazy / normal order evaluation manages this well, see the leftmost-outermost reduction strategy).

And it is now straightforward to see the reduction steps, and prove that the expectation holds true. For clarity, regexps are underlined:

- <ins>(**pair 1** ⋅ **fst**)</ins> (**pair 1** ⋅ **fst**) ... reduces to **pair 1** (**fst** ((**pair 1** ⋅ **fst**) ...))
- **pair 1** (**fst** (<ins>(**pair 1** ⋅ **fst**) ...</ins>)) reduces further to **pair 1** (**fst** (**pair 1** (**fst** ...)))
- **pair 1** (<ins>**fst** (**pair 1** (**fst** ...))</ins>) reduces further to **pair 1** (**pair 1** (**fst** ...) **K**)
- **pair 1** (<ins>**pair 1** (**fst** ...) **K**</ins>) reduces further to **pair 1** (**K 1** (**fst** ...))
- **pair 1** (<ins>**K 1** (**fst** ...)</ins>) reduces further to **pair 1 1**

Quod erat demonstrandum. So we have just seen why Haskell's

```haskell
let (a, b) = (1, a) in (a, a)
```

lazy let-rec construct terminates and evalues to `(1, 1)`.

An this let-rec is not more difficult than

```haskell
let (count, sum, xs) = ... sum ... count
in xs
```

is! so the mysterious „credit-card borrow to the future” part of Wouter Swiestra's article, presented here with

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

looks no more so msterious: because we have just seen above on the example of a simplified construct, `let (m, n) = (0, m) in (m, n)`, that this has an entirely valid solution, moreover, it is completely translatable to pure combinatory logic, and reducable with pencil-and-paper to termination (normal form), having valid result.

For working with pure lambda calculus expressions under lazy evaluation strategy, You can see some of my implementation projects on pure combinatory ogic. Although these implement combinatory logic, not lambda calculus, but the questions in impplementation details can be similar.
- [This combinatory logic evaluator / interpreter](https://github.com/alignalghii/CL-zipper-RWS) has a clean implementation, but has only very few features for practical use, it is extemely minimal, although its code can accept well further extensions.
- [An even more extremest minimalistic project](https://hub.darcs.net/physis/CL-LeftAssocApplTreeZipper/browse/Comb.hs). In experimental stage. This is similar to the above one, but it encodes more information into typing and algebraic data types: it has thus a more orthogonal and type-safe architecture.
- [A practically usable project](https://hub.darcs.net/physis/CL-quine): it is much more user-friendy with many features, but has a rather old and tangled sourcecode.
    - It has also a sample of the most familiar and practical combinators of combinatory logics —, like the most familiar combinators above the familair **S**-and-**K**-base : **C**, **B**, **Y** and many more — but in a HaskellWiki article of mine I wrote a human-readable sample of combinators and a small [sample of how to write „programs” directly in combinatory logic](https://wiki.haskell.org/Combinatory_logic#Programming_in_CL) as mini a programming language (e.g. tuples, `Maybe` and `Either` datatypes, recursive datatypes like lists).
    - The project also implements a [quine / selfrep](https://hub.darcs.net/physis/CL-quine/browse/CL/Quine.lhs) in combinatory logic itself — more precisely said, the corresponding concept of what we call a „*quine*” (selfrep) in traditional programming languages (see David Madore's [article](http://www.madore.org/~david/computers/quine.html), or that of [Wikipedia](https://en.wikipedia.org/wiki/Quine_(computing)). In my combinatory logic project, analogously I call a quine a combintory logic term which reduces to a normal form which is at the same time its own „quotation”, i.e. a tree datatype encoded explicitly in (combinatory logic) having exactly the same structure as the original term.

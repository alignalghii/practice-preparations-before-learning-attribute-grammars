module Combinators where


type Transform a = a -> a

-- One of combinatory logic's base combinators: S
combinatorS :: (a -> b -> c) -> (a -> b) -> a -> c
combinatorS p f x = p x (f x)

-- The fixpoint combinator Y, see lambda calculus or combinatory logic
fixpointCombinator :: (a -> a) -> a
fixpointCombinator f = f $ fixpointCombinator f

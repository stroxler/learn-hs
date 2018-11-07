# tautology

Simple tautology checker, based on Chapter 8 of Graham Hutton's
"Programming Haskell", Second Edition.

My code is using a lot more libraries that Hutton's, because I'm
tryin to use small exercises to get the hang of "real" haskell.

## Problem Statement

Given this definition of a boolean proposition:
```
data Prop = Const Bool
          | Var String
          | And Prop Prop
          | Or Prop Prop
          | Not Prop
          | Imply Prop Prop
```
write a tautology checker: determine whether a proposition is true
for all possible assignments of variables.

## The implementation

First, we define a `VarMap` to be a `Map String Bool` - basically a map
(like a dictionary for pythonistas) to hold variable assignments. Then,
we define a `findVar` helper to look up a variable in the map, and an
`evalProp` evaluator that walks the proposition AST and returns `True`
or `False` for a given `VarMap`.

Finally, we write three more functions:
 - `getVariables` that can scan a `Prop` and find all of the variables in it
   (as a `Set` of `String` in our case)
 - `makeVariableValues` that can turn a set of variable names into a list
   of maps, one for every possible combination of boolean values
 - `isTautology`, which calls `all (evaluate prop)` (from `Foldable`) on
    the result of `(makeVariableValues . getVariables) prop`

All functions have unit tests.

## What's interesting in this implementation

### Map and Set

Most intro Haskell books don't cover `Map` and `Set`, so this is a bit
interesting. The main thing is that they aren't part of the standard library,
you need to pull in `collections` (see `package.yaml`).

The interfaces are reasonably intuitive if you are familiar with immutable
data structures; my suggestion for exploring the api is to use `ghci` and
run
```
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
```
so that you can tab-complete for exploring the api.

We also use `Set.fold`, which is really just `foldr` but I find calling the
set-specific one pretty intuitive, as part of `makeVariableValues`.

### Writer monad

The `getVariables` function could easily have been done via a tail-recursive
helper function, but I realized this was a great opportunity to get the
hang of the `Writer` pattern. There's a comment above the function that
discusses this a bit.

Note that I didn't use a built-in `Writer` (not sure if there is one), instead
I got the one from `transformers` (see `package.yaml`) since this is frequently
what you'd want to use for real apps anyway.

### DSL in tests

In the tests, writing out explicit data structures can be a pain. One
interesting thing I did to cut down on boilerplate was write a dsl of infix
operators for `And`, `Or`, and `Impl`. I could have done this in the library,
but doing it in the tests is interesting because you can define the DSL very
locally, which prevents readers from needing to memorize a bunch of unfamiliar
operators. I'm pretty happy with how some of the `evalProp` and `isTautology`
tests came out.

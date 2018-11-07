module Lib where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Writer.Lazy (Writer, execWriter, tell)

data Prop = Const Bool
          | Var String
          | And Prop Prop
          | Or Prop Prop
          | Not Prop
          | Imply Prop Prop


type Varname = String
type VarMap = Map.Map Varname Bool


findVar :: Varname -> VarMap -> Bool
findVar v m = case Map.lookup v m of
  Nothing -> error $ "Could not find var `" ++ v ++ "` in var map"
  Just b -> b



evalProp :: Prop -> VarMap -> Bool
evalProp (Const b) _ = b
evalProp (Var v) m = findVar v m
evalProp (Not p) m = not $ evalProp p m
evalProp (And p0 p1) m = evalProp p0 m && evalProp p1 m
evalProp (Or p0 p1) m = evalProp p0 m || evalProp p1 m
evalProp (Imply p0 p1) m = evalProp p1 m || (not $ evalProp p0 m)


isTautology :: Prop -> Bool
isTautology prop = all (evalProp prop) $ (makeVariableValues . getVariables) prop

-- getVariables is an example of using the Writer monad for
-- problem solving.
--
-- The Writer monad wires up a lot of code automatically
-- for us, because the `w` parameter inside it represents a monoid, which
-- means it knows how to do all of the combining for us (unlike, say, a
-- state monad where we'd need to use `get`/`put` or `modify` and set
-- everything up ourselves). Also unlike state, `Writer` doesn't require
-- us to set an initial state.
--
-- In our case, we're using a (Set.Set String) as the monoid inside
-- our writer, and the only thing we ever need to do is `tell` the writer
-- about a variable we see, by handing it a `Set.singleton` with it.
getVariables :: Prop -> Set.Set Varname
getVariables prop = execWriter $ getVariables_ prop


getVariables_ :: Prop -> Writer (Set.Set Varname) ()
getVariables_ (Const _) = pure ()
getVariables_ (Var v)   = tell (Set.singleton v)
getVariables_ (Not p)   = getVariables_ p
getVariables_ p =
  let getSubPropVariables p0 p1 = do
        getVariables_ p0
        getVariables_ p1
  in case p of
    And   p0 p1 -> getSubPropVariables p0 p1
    Or    p0 p1 -> getSubPropVariables p0 p1
    Imply p0 p1 -> getSubPropVariables p0 p1


makeVariableValues :: Set.Set Varname -> [VarMap]
makeVariableValues vars = Set.fold f [Map.empty] vars
  where f var accum =
          do
            otherVars <- accum
            [ Map.insert var True otherVars,
              Map.insert var False otherVars ]


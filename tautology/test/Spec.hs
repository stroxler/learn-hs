import Lib

import Test.Hspec
import Control.Exception (evaluate)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map


quickTest descr contents =
  describe descr $ it "should behave as expected" $ contents

run :: IO ()
run = hspec $ do
  describe "findVar" $ do
    it "should throw error if var not found" $ do
      evaluate (findVar "x" Map.empty) `shouldThrow`
        errorCall "Could not find var `x` in var map"
    it "should return value if var found" $ do
      findVar "x" (Map.fromList [("x", True), ("y", False)]) `shouldBe` True
      findVar "y" (Map.fromList [("x", True), ("y", False)]) `shouldBe` False
  describe "getVariables" $ do
    it "should work on simple cases" $ do
      getVariables (Const True) `shouldBe` Set.fromAscList []
      getVariables (Const False) `shouldBe` Set.fromAscList []
      getVariables (Var "x") `shouldBe` Set.fromAscList ["x"]
    it "should work on nested cases" $ do
      getVariables (And (Const False) (Const True)) `shouldBe` Set.fromAscList []
      getVariables (And (Var "x") (Const True)) `shouldBe` Set.fromAscList ["x"]
      getVariables (And (Var "x") (Var "y")) `shouldBe` Set.fromAscList ["x", "y"]
      getVariables (Or (Var "x") (Var "y")) `shouldBe` Set.fromAscList ["x", "y"]
      getVariables (Imply (Var "x") (Var "y")) `shouldBe` Set.fromAscList ["x", "y"]
      getVariables (Not $ (Var "x")) `shouldBe` Set.fromAscList ["x"]
    it "should work on complex cases, with duplicated values" $ do
      getVariables (
        Imply
          (And (Or (Const True) (Var "x")) (Not $ Var "x"))
          (Or (Var "y") (And (Var "y") (Var "z")))
        ) `shouldBe` Set.fromAscList ["x", "y", "z"]
  describe "makeVariableValues" $ do
    it "should work on empty vars and singletons" $ do
      makeVariableValues (Set.empty) `shouldBe` [Map.empty]
      makeVariableValues (Set.singleton "x") `shouldBe`
        [Map.singleton "x" True, Map.singleton "x" False]
    it "should work with more variables" $ do
      makeVariableValues (Set.fromList ["x", "y"]) `shouldBe`
        [ Map.fromList [("x", True),  ("y", True) ]   -- Note that we depend on
        , Map.fromList [("x", False), ("y", True) ]   -- the set fold ordering here
        , Map.fromList [("x", True),  ("y", False)]
        , Map.fromList [("x", False), ("y", False)]
        ]
  describe "evalProp" $ do
    it "should work on atomic propositions" $ do
      evalProp (Const True) Map.empty `shouldBe` True
      evalProp (Const False) Map.empty `shouldBe` False
      evalProp (Var "x") (Map.fromList [("x", True), ("y", False)]) `shouldBe` True
      evalProp (Var "y") (Map.fromList [("x", True), ("y", False)]) `shouldBe` False
    it "should work on nested propositions" $
      let
          varmap = Map.fromList [("x", True), ("y", False)]
          (t, f, vx, vy) = (Const True, Const False, Var "x", Var "y")
          ((&&&), (|||), (>>>)) = (And, Or, Imply)
      in do
          evalProp (t &&& f) varmap `shouldBe` False
          evalProp (t &&& t) varmap `shouldBe` True
          evalProp (vx &&& t) varmap `shouldBe` True
          evalProp (vx ||| vy) varmap `shouldBe` True
          evalProp (vx >>> vy) varmap `shouldBe` False
          evalProp (vx >>> (Not vy)) varmap `shouldBe` True
  describe "isTautology" $ do
    it "should work on some test cases" $
      let
          (t, f, vx, vy) = (Const True, Const False, Var "x", Var "y")
          ((&&&), (|||), (>>>)) = (And, Or, Imply)
      in do
        isTautology t `shouldBe` True
        isTautology (vx >>> t) `shouldBe` True
        isTautology ((vx ||| vy) >>> t) `shouldBe` True
        isTautology ((vx &&& vy) >>> (vx ||| vy)) `shouldBe` True
        isTautology ((vx &&& vy) >>> (vx ||| (Not vy))) `shouldBe` True
        isTautology ((Not (vx &&& vy)) >>> (vx ||| (Not vy))) `shouldBe` False


main :: IO ()
main = do
  run

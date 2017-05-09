{-# LANGUAGE QuasiQuotes #-}

module Language.HigherRank.TypecheckSpec (spec) where

import Test.Hspec

import Language.HigherRank.Typecheck
import Language.HigherRank.Util.TH

spec :: Spec
spec = describe "Typecheck" $ do
  describe "()" $
    it "has type ()" $
      runInfer [exprQ|()|] `shouldBe` Right [typeQ|()|]

  describe "function application" $ do
    it "has the type of its result" $
      runInfer [exprQ|((\x -> x) ())|] `shouldBe` Right [typeQ|()|]

    it "supports higher-order functions" $
      runInfer [exprQ|((\f -> (f ())) (\x -> x))|]
        `shouldBe` Right [typeQ|()|]

    it "produces a type error when applied with the wrong type" $
      runInfer [exprQ|((\f -> (f ())) ())|]
        `shouldBe` Left "type mismatch: expected (() -> a4'), given ()"

    it "supports higher-rank polymorphism" $
      runInfer [exprQ|(((\f -> ((f (\y -> y)) (f ())))
                        : ((forall a. (a -> a)) -> ()))
                       (\x -> x))|]
        `shouldBe` Right [typeQ|()|]

    it "produces a type error when given a function of insufficient generality" $
      runInfer [exprQ|(((\f -> ((f (\y -> y)) (f ())))
                        : ((forall a. (a -> a)) -> ()))
                       ((\x -> x) : (() -> ())))|]
        `shouldBe` Left "type mismatch: expected (), given a"

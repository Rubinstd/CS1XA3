{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType
import           Test.QuickCheck
import           Generic.Random
import           GHC.Generics


import qualified Data.Map.Strict as Map
import           Test.QuickCheck


instance Arbitrary (Expr Double) where
  arbitrary = genericArbitraryRec uniform `withBaseCase` return (Const 1)

sampleExpr1 :: Expr Double
sampleExpr1 = (var "x") !+ (var "y")


listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"


test1 :: Double -> Bool
test1 x = eval (Map.fromList [("x",x),("y",-x)]) sampleExpr1 == 0

evalVectProp1 :: [Expr Double] -> [Expr Double]-> Bool -- Make sure that my vectors in addition are of the right length.
evalVectProp1 (e1) (e2) = case length e1 > length e2 of
                        True -> length (evalVect vrs (Add (Vect e1) (Vect e2))) == length e1
                        False -> length (evalVect vrs (Add (Vect e1) (Vect e2))) == length e2
    where vrs = Map.fromList []


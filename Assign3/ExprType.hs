{-# LANGUAGE DeriveGeneric #-}

{-|
Module : ExprType
Description : Contains the Expr data type as well as useful functions relating to basic use of Expr's.
Copyright : (c) Daniel Rubinstein @2018
License : WTFPL
Maintainer : rubinstd@mcmaster.ca
Stability : experimental
Portability : POSIX

Contains the Expr data type as well as useful functions relating to basic use of Expr's.
-}
module ExprType where

import           Data.List
import           Generic.Random
import           GHC.Generics

-- | A datatype for common numeric expression
data Expr a = Add (Expr a) (Expr a)     -- ^ Binary Addition
            | Sub (Expr a) (Expr a)     -- ^ Binary Subtraction
            | Mult (Expr a) (Expr a)    -- ^ Binary Multiplication
            | Div (Expr a) (Expr a)     -- ^ Binary Division
            | Cos (Expr a)              -- ^ Triganometric Cosine Function
            | Sin (Expr a)              -- ^ Triganometric Sine Function
            | Log (Expr a)              -- ^ Logorithmic Function
            | Exp (Expr a)              -- ^ Exp (Natural) Function
            | Vect [Expr a]             -- ^ Vector Wrapper
            | Pow (Expr a) (Expr a)     -- ^ Power Operator
            | Const a                   -- ^ Value Wrapper
            | Var String                -- ^ Variable Identifier
  deriving (Eq,Generic)

-- | A function that takes a variable of type ('Expr' a) and returns a list of strings containing the variable names wrapped by 'Var' in the expression.
getVars :: Expr a   -- ^ The expression to be searched for variables.
       -> [String]  -- ^ A list of the variables found with the ['String'] type.
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Sub e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Div e1 e2)  = getVars e1 `union` getVars e2
getVars (Pow e1 e2)  = getVars e1 `union` getVars e2
getVars (Vect (x:xs))= getVars x  `union` getVars (Vect xs)
getVars (Vect [])    = []
getVars (Cos e)      = getVars e
getVars (Sin e)      = getVars e
getVars (Log e)      = getVars e
getVars (Exp e)      = getVars e
getVars (Const _)    = []
getVars (Var ident)  = [ident]
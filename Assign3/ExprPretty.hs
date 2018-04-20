{-|
Module : ExprPretty
Description : Constains the show instance for the Expr datatype. 
              Nicely formats all of the different constructors of the Expr type.
Copyright : (c) Daniel Rubinstein @2018
License : WTFPL
Maintainer : rubinstd@mcmaster.ca
Stability : experimental
Portability : POSIX

Constains the show instance for the Expr datatype. 
Nicely formats all of the different constructors of the Expr type.
-}
module ExprPretty where

import           ExprType

-- | A function that takes a string and returns that string with brackets surrounding it for formating purposes.
parens :: String -> String
parens ss = "(" ++ ss ++ ")"

-- | The show instance for the 'Expr' type.
instance Show a => Show (Expr a) where
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Div e1 e2) = parens (show e1) ++ " !/ " ++ parens (show e2)
  show (Sub e1 e2) = parens (show e1) ++ " !- " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Pow e1 e2)  = parens (show e1) ++ " !^ " ++ parens (show e2)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "val " ++ show x
  show (Vect x)    = parens $ "vect " ++ show x
  show (Cos x)  = "cos'" ++ parens (show x) 
  show (Sin x)  = "sin'" ++ parens (show x) 
  show (Log x)  = "log'" ++ parens (show x) 
  show (Exp x)  = "exp'" ++ parens (show x) 
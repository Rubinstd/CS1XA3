{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module : ExprDiff
Description : Contains a type class and instances for differentiable expressions.
Copyright : (c) Daniel Rubinstein @2018
License : WTFPL
Maintainer : rubinstd@mcmaster.ca
Stability : experimental
Portability : POSIX

Contains a type class and instances for
differentiable expressions. Within the
instances are several useful expressions
for the evaluation, simplification, and 
partial differentiation of Expr type variables.
This module also contains various vector operations
including evaluation/simplifcation of a vector containing
expressions, simple addition and subtraction, dot product.
-}

module ExprDiff where

import           ExprType

import qualified Data.Map.Strict as Map


{- ------------------------------------------------------------------------------------------------------------
 - Diff Expr Class
 - ------------------------------------------------------------------------------------------------------------
 -}

class DiffExpr a where
  -- | Evaluates non-vector expressions of type Expr a. Takes a map contianing the values of any variables used in the expression. Returns a value of type a. Note that this function will error if undefined variables exist within the given expression or if an invalid Expr is given (ie. a vector. See evalVect for these types of expressions).
  eval :: Map.Map String a        -- ^ The map contianing the values of all variables used.
       -> Expr a                  -- ^ The expression to be evaluated.
       -> a                       -- ^ The result of the evaluation of the expression.  
  
  -- | Evaluates a vector of type Expr a. Takes a map containing the values of any variables used within the vector. Returns a value of type [a]. Note that this function will error if there is an undefined variable within the vector or if an invalid expression is given (ie. anything not confound to vectors, vector addition, or vector subtraction. See eval for these types of expressions).
  evalVect :: Map.Map String a    -- ^ The map contianing the values of all variables used.
           -> Expr a              -- ^ The vector expression to be evaluated.
           -> [a]                 -- ^ The result of the evaluation of the expression.
  
  -- | Evaluates the dot product of two given vectors (with the types Expr a). Returns a value of type a. Takes a map containing the values of any variables used within the vectors. Note that this function will error if the inputed expressions are not two vectors.
  dotProd :: Map.Map String a     -- ^ The map contianing the values of all variables used.
             -> Expr a            -- ^ The first vector in the dot product.
             -> Expr a            -- ^ The second vector in the dot product.
             -> a                 -- ^ The result of the dot product.

  -- | Evaluates the cross prooduct of two given 3-dimensional vectors (with types Expr a). Returns a value of type Expr a. Takes a map containing the values of any variables used within the vectors. Note that this function will error if the inputed expressions are not two vectors.
  crossProd :: Map.Map String a   -- ^ The map contianing the values of all variables used.
             -> Expr a            -- ^ The first vector in the cross product.
             -> Expr a            -- ^ The second vector in the cross product.
             -> Expr a            -- ^ The result of the cross product.
  
  -- | Simplifies expressions of type Expr a. Takes a map contianing the values of any variables used in the expression. If a variable in the expression is not defined in the map, it will be left in it's String form. Returns a simplified Expr a. Note that this function will not work on vectors (see evalVect for all your vector needs).
  simplify :: Map.Map String a    -- ^ The map contianing the values of all variables used.
           -> Expr a              -- ^ The expression to be simplified.
           -> Expr a              -- ^ The result of the simplification of the expression.
  
  -- | Partially differentiates an expression of type Expr a with respect to a given variable. Takes a String with the variable to be differentiated. Returns the differentiated expression of type Expr a.
  partDiff :: String              -- ^ The variable to be differentiated with respect to.
           -> Expr a              -- ^ The expression to be differentiated.
           -> Expr a              -- ^ The resulting partially differentiated expression.

  -- | A prettier form of the Add constructor found in the Expr type. Takes two expressions of type Expr a, wraps them in the Add constructor and simplifies the new expression. 
  (!+) ::  Expr a                 -- ^ The first expression in the addition.
        -> Expr a                 -- ^ The second expression in the addition.
        -> Expr a                 -- ^ The simplified addition of both expressions. 
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  
  -- | A prettier form of the Sub constructor found in the Expr type. Takes two expressions of type Expr a, wraps them in the Sub constructor and simplifies the new expression.
  (!-) :: Expr a                  -- ^ The first expression in the subtraction.
       -> Expr a                  -- ^ The second expression in the subtraction.
       -> Expr a                  -- ^ The simplified subtraction of both expressions.
  e1 !- e2 = simplify (Map.fromList []) $ Sub e1 e2
  
  -- | A prettier form of the Mult constructor found in the Expr type. Takes two expressions of type Expr a, wraps them in the Mult constructor and simplifies the new expression.
  (!*) :: Expr a                  -- ^ The first expression in the multiplication.
       -> Expr a                  -- ^ The second expression in the multiplication.
       -> Expr a                  -- ^ The simplified multiplication of both expressions.
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  
  -- | A prettier form of the Div constructor found in the Expr type. Takes two expressions of type Expr a, wraps them in the Div constructor and simplifies the new expression.
  (!/) :: Expr a                  -- ^ The first expression in the division.
       -> Expr a                  -- ^ The second expression in the division.
       -> Expr a                  -- ^ The simplified division of both expressions.
  e1 !/ e2 = simplify (Map.fromList []) $ Div e1 e2

  -- | A prettier form of the Pow constructor found in the Expr type. Takes two expressions of type Expr a, wraps them in the Pow constructor and simplifies the new expression.
  (!^) :: Expr a                  -- ^ The base of the power.
       -> Expr a                  -- ^ The exponent of the power.
       -> Expr a                  -- ^ The simplified division of both expressions.
  e1 !^ e2 = simplify (Map.fromList []) $ Pow e1 e2

  -- | A prettier form of the Cos constructor found in the Expr type. Takes an expression and wraps it in the Cos constructor.
  cos' :: Expr a                  -- ^ The expression to be evaluated through the Cos function.
       -> Expr a                  -- ^ The given expression wrapped wihtin the Cos constructor
  cos' e = Cos e
  
  -- | A prettier form of the Sin constructor found in the Expr type. Takes an expression and wraps it in the Sin constructor.
  sin' :: Expr a                  -- ^ The expression to be evaluated through the Sin function.
       -> Expr a                  -- ^ The given expression wrapped wihtin the Sin constructor
  sin' e = Sin e
  
  -- | A prettier form of the Log constructor found in the Expr type. Takes an expression and wraps it in the Log constructor.
  log' :: Expr a                  -- ^ The expression to be evaluated through the Log function.
       -> Expr a                  -- ^ The given expression wrapped wihtin the Log constructor
  log' e = Log e
  
  -- | A prettier form of the Exp constructor found in the Expr type. Takes an expression and wraps it in the Exp constructor.
  exp' :: Expr a                  -- ^ The expression to be evaluated through the Exp function.
       -> Expr a                  -- ^ The given expression wrapped wihtin the Exp constructor
  exp' e = Exp e
  
  -- | A prettier form of the Val constructor found in the Expr type. Takes a value of type a and wraps it in the Const constructor returning an expression of type Expr a.
  val :: a                        -- ^ The value to be treated as a constant.
      -> Expr a                   -- ^ The value wrapped in the Const constructor.
  val x = Const x
  
  -- | A prettier form of the Val constructor found in the Expr type. Takes a variable of type String and wraps it in the Var constructor returning an expression of type Expr a.
  var :: String                   -- ^ The variable name.
      -> Expr a                   -- ^ The variable wrapped in the Var constructor.
  var x = Var x
  
  -- | A prettier form of the Vect constructor found in the Expr type. Takes a list of type [Expr a] and returns a vector in the Expr type using the given list as it's components.
  vect :: [Expr a]                -- ^ The list of components for the vector.
       -> Expr a                  -- ^ The component list wrapped in the Vect constructor.
  vect e = Vect e


{- ------------------------------------------------------------------------------------------------------------
 - Instance of DiffExpr for Floating values.
 - ------------------------------------------------------------------------------------------------------------
 -}

instance (Floating a, Eq a) => DiffExpr a where
  
  {- ----------------------------------------------------------------------------------------------------------
   - eval function
   - ------------------------------------------------------------------------------------------------------------
  -}

  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2                                                                          -- Evaluates addition.
  eval vrs (Sub e1 e2)  = eval vrs e1 - eval vrs e2                                                                          -- Evaluates subtraction.
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2                                                                          -- Evaluates multiplication.
  eval vrs (Div e1 e2)  = eval vrs e1 / eval vrs e2                                                                          -- Evaluates division.
  eval vrs (Cos e) = cos (eval vrs e)                                                                                        -- Evaluates cos.
  eval vrs (Sin e) = sin (eval vrs e)                                                                                        -- Evaluates sin.
  eval vrs (Log e) = log (eval vrs e)                                                                                        -- Evaluates log (base 10).
  eval vrs (Exp e) = exp (eval vrs e)                                                                                        -- Evaluates exp (e constant to the power of the given value). 
  eval vrs (Pow e1 e2) = eval vrs e1 ** eval vrs e2                                                                          -- Evaluates pow (takes first number as base, second as exponent).
  eval vrs (Const x) = x                                                                                                     -- Returns just the value of the constant without the wrapper.
  eval vrs (Var x) = case Map.lookup x vrs of                                                                                -- Attempts to return the value of the variable and errors if it is not defined in the dictionary.
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"
  eval vrs _ = error "Not a valid operation"                                                                                 -- Returns an error if the input is an invalid operation.

  
  {- ------------------------------------------------------------------------------------------------------------
   - evalVect function
   - ------------------------------------------------------------------------------------------------------------
  -}

  evalVect vrs (Vect []) = []                                                                                                -- Returns an empty list for an empty list vector.
  evalVect vrs (Vect (x:xs)) = [eval vrs x] ++ evalVect vrs (Vect xs)                                                        -- Evaluates a single Vector [Expr a] (returning a list of type a).
  
  evalVect vrs (Add (Vect []) (Vect [])) = []
  evalVect vrs (Add (Vect (x:xs)) (Vect []) ) = [eval vrs x] ++ evalVect vrs (Add (Vect xs) (Vect []))                       -- Evaluates vector addition if the first vector is longer than the second.
  evalVect vrs (Add (Vect []) (Vect (y:ys))) = [eval vrs y] ++ evalVect vrs (Add (Vect []) (Vect ys))                        -- Evaluates vector addition if the second vector is longer than the first.
  evalVect vrs (Add (Vect (x:xs)) (Vect (y:ys))) = [eval vrs x + eval vrs y] ++ evalVect vrs (Add (Vect xs) (Vect ys))       -- Evaluates vector addition.

  evalVect vrs (Sub (Vect []) (Vect [])) = []
  evalVect vrs (Sub (Vect (x:xs)) (Vect []) ) = [eval vrs x] ++ evalVect vrs (Sub (Vect xs) (Vect []))                       -- Evaluates vector subtraction if the first vector is longer than the second.
  evalVect vrs (Sub (Vect []) (Vect (y:ys))) = [eval vrs y] ++ evalVect vrs (Sub (Vect []) (Vect ys))                        -- Evaluates vector subtraction if the second vector is longer than the first.
  evalVect vrs (Sub (Vect (x:xs)) (Vect (y:ys))) = [eval vrs x - eval vrs y] ++ evalVect vrs (Sub (Vect xs) (Vect ys))       -- Evaluates vector subtraction
  
  evalVect vrs _ = error "Not a valid vector operation."                                                                     -- Occurs if the function is used with improper inputs.

  {- ------------------------------------------------------------------------------------------------------------
   - dotProd function
   - ------------------------------------------------------------------------------------------------------------
  -}

  dotProd vrs (Vect []) (Vect []) = 0
  dotProd vrs (Vect (x:xs)) (Vect (y:ys)) = (eval vrs x * eval vrs y) + dotProd vrs (Vect xs) (Vect ys)
  dotProd vrs _ _ = error "Invalid input. Requires 2 vectors."

  {- ------------------------------------------------------------------------------------------------------------
   - crossProd function
   - ------------------------------------------------------------------------------------------------------------
  -}

  crossProd vrs (Vect (x)) (Vect (y)) = let
                  i = (simplify vrs (x !! 1) !* simplify vrs (y !! 2)) !- (simplify vrs (x !! 2) !* simplify vrs (y !! 1))   -- Calculates the i value in cross product (the first component).
                  j = (simplify vrs (x !! 2) !* simplify vrs (y !! 0)) !- (simplify vrs (x !! 0) !* simplify vrs (y !! 2))   -- Calculates the j value in cross product (the second component).
                  k = (simplify vrs (x !! 0) !* simplify vrs (y !! 1)) !- (simplify vrs (x !! 1) !* simplify vrs (y !! 0))   -- Calculates the k value in cross product (the third component). 
          in Vect [i,j,k]
  crossProd vrs _ _ = error "Invalid input. Requires 2 vectors."                                                             -- Occurs if the function is used with improper inputs.

  {- ------------------------------------------------------------------------------------------------------------
   - simplify function
   - ------------------------------------------------------------------------------------------------------------
  -}

  -- Simplification for addition.
  simplify vrs (Add e1 (Const 0)) = simplify vrs e1                                                                          -- Something + 0 = Something simplified.
  simplify vrs (Add (Const 0) e2) = simplify vrs e2                                                                          -- 0 + Anything = Anything simplified
  simplify vrs (Add (Const e1) (Const e2)) = Const (e1 + e2)                                                                 -- Const Something + Const Anything = Const (Something + Anything)
  simplify vrs (Add (Const e1) (Add (Const c) e2)) = Add (Const (e1 + c)) (simplify vrs e2)                                  -- Adding constants from two different expressions.
  simplify vrs (Add (Const e1) (Add e2 (Const c))) = Add (Const (e1 + c)) (simplify vrs e2)                                  -- Adding constants from two different expressions.
  simplify vrs (Add (Add e1 (Const c)) (Const e2)) = Add (Const (e2 + c)) (simplify vrs e1)                                  -- Adding constants from two different expressions.
  simplify vrs (Add (Add (Const c) e1) (Const e2)) = Add (Const (e2 + c)) (simplify vrs e1)                                  -- Adding constants from two different expressions.
  simplify vrs (Add e1 (Var e2)) = case simplify vrs (Var e2) of                                                             -- Const c + Variable v = Const c + Var v (if v is not defined in vrs) OR Const (c + v) (if v is defined in vrs)
                                      Var v -> Add (simplify vrs e1) (Var v)
                                      Const c -> error "shit"--simplify vrs (Add (simplify vrs e1) (Const c))
  simplify vrs (Add (Var e1) e2) = case simplify vrs (Var e1) of                                                             --  Variable v + Const c = Var v + Const c (if v is not defined in vrs) OR Const (v + c) (if v is defined in vrs)
                                      Var v -> Add (Var v) (simplify vrs e2)
                                      Const c -> simplify vrs (Add (Const c) (simplify vrs e2))
  simplify vrs (Add (Vect x) (Vect y)) = Add (Vect x) (Vect y)
  simplify vrs (Add e1 e2) = (Add (simplify vrs e1) (simplify vrs e2))                                                       -- Expr a + Expr b = Simplify Expr a + Simplify Expr b 

  
  -- Simplification for subtraction.
  simplify vrs (Sub e1 (Const 0)) = simplify vrs e1                                                                          -- Something + 0 = Something simplified.
  simplify vrs (Sub (Const 0) e2) = simplify vrs e2                                                                          -- 0 + Anything = Anything simplified
  simplify vrs (Sub (Const e1) (Const e2)) = Const (e1 - e2)                                                                 -- Const Something + Const Anything = Const (Something + Anything)
  simplify vrs (Sub (Const e1) (Add (Const c) e2)) = Sub (Const (e1 - c)) (simplify vrs e2)                                  -- Subtracting constants from two different expressions.
  simplify vrs (Sub (Const e1) (Add e2 (Const c))) = Sub (Const (e1 - c)) (simplify vrs e2)                                  -- Subtracting constants from two different expressions.
  simplify vrs (Sub (Add e1 (Const c)) (Const e2)) = Sub (Const (e2 - c)) (simplify vrs e1)                                  -- Subtracting constants from two different expressions.
  simplify vrs (Sub (Add (Const c) e1) (Const e2)) = Sub (Const (e2 - c)) (simplify vrs e1)                                  -- Subtracting constants from two different expressions.
  simplify vrs (Sub e1 (Var e2)) = case simplify vrs (Var e2) of                                                             -- Const c + Variable v = Const c + Var v (if v is not defined in vrs) OR Const (c + v) (if v is defined in vrs)
                                      Var v -> Sub (simplify vrs e1) (Var v)
                                      Const c -> simplify vrs (Sub (simplify vrs e1) (Const c))
  simplify vrs (Sub (Var e1) e2) = case simplify vrs (Var e1) of                                                             -- Variable v + Const c = Var v + Const c (if v is not defined in vrs) OR Const (v + c) (if v is defined in vrs)
                                      Var v -> Sub (Var v) (simplify vrs e2)
                                      Const c -> simplify vrs (Sub (Const c) (simplify vrs e2))
  simplify vrs (Sub (Vect x) (Vect y)) = Sub (Vect x) (Vect y)
  simplify vrs (Sub e1 e2) = (Sub (simplify vrs e1) (simplify vrs e2))                                                       -- Expr a - Expr b = Simplify Expr a - Simplify Expr b
  
  --Simplification for multiplication. Look over to check if you have to simplify more.
  simplify vrs (Mult _ (Const 0)) = Const 0                                                                                  -- Anything multiplied by 0 = 0
  simplify vrs (Mult (Const 0) _) = Const 0                                                                                  -- 0 multiplied by anything = 0
  simplify vrs (Mult e1 (Const 1)) = simplify vrs e1                                                                         -- Constant Anything multiplied by 1 = Constant Anything
  simplify vrs (Mult (Const 1) e2) = simplify vrs e2                                                                         -- 1 multiplied by Constant Anything = Constant Anything
  simplify vrs (Mult (Const e1) (Const e2)) = Const (e1 * e2)
  simplify vrs (Mult (Var v1) (Var v2)) = if simplify vrs (Var v1) == simplify vrs (Var v2)
                                          then Pow (Var v1) (Const 2)
                                          else simplify vrs (Mult (simplify vrs (Var v1)) (simplify vrs (Var v2)))
  simplify vrs (Mult (Pow e1 p1) (Pow e2 p2)) = if simplify vrs e1 == simplify vrs e2                                        -- Addition of powers. (x ^ 2) * (x ^ 2) = x ^ (2 + 2)
                                                  then Pow (simplify vrs e1) (simplify vrs (p1 !+ p2))
                                                  else (Mult (Pow (simplify vrs e1) (simplify vrs p1)) 
                                                       (Pow (simplify vrs e2) (simplify vrs p2)))
  simplify vrs (Mult (Pow e1 p1) e2) = simplify vrs (Mult (Pow e1 p1) (Pow e2 (Const 1)))                                    -- Treats any equation as being that equation to the power of one. Used for addition of powers. 
  simplify vrs (Mult e1 (Pow e2 p2)) = simplify vrs (Mult (Pow e1 (Const 1)) (Pow e2 p2))                                    -- Treats any equation as being that equation to the power of one. Used for addition of powers.
  simplify vrs (Mult e1 (Var e2)) = case simplify vrs (Var e2) of                                                            -- Const c * Variable v = Const c * Var v (if v is not defined in vrs) OR Const (c * v) (if v is defined in vrs)
                                      Var v -> Mult (simplify vrs e1) (Var v)
                                      Const c -> simplify vrs (Mult (simplify vrs e1) (Const c))
  simplify vrs (Mult (Var e1) e2) = case simplify vrs (Var e1) of                                                            --  Variable v * Const c = Var v * Const c (if v is not defined in vrs) OR Const (v * c) (if v is defined in vrs)
                                      Var v -> Mult (Var v) (simplify vrs e2)
                                      Const c -> simplify vrs (Mult (Const c) (simplify vrs e2))
  simplify vrs (Mult e1 e2) = (Mult (simplify vrs e1) (simplify vrs e2))                                                     -- Expr a * Expr b = Simplify Expr a * Simplify Expr b

  --Simplification for multiplication.
  simplify vrs (Div _ (Const 0)) = error "Division by zero"                                                                  -- Anything / 0 = error
  simplify vrs (Div (Const 0) _) = Const 0                                                                                   -- 0 / Anything = 0
  simplify vrs (Div e1 (Const 1)) = simplify vrs e1                                                                          -- Anything / 1 = Anything
  simplify vrs (Div (Const e1) (Const e2)) = Const (e1 / e2)                                                                 -- Const Something / Const Anything = Const (Something / Anything)
  simplify vrs (Div (Var v1) (Var v2)) = if simplify vrs (Var v1) == simplify vrs (Var v2)
                                          then Pow (Var v1) (Const 0)
                                          else simplify vrs (Div (simplify vrs (Var v1)) (simplify vrs (Var v2)))
  simplify vrs (Div (Pow e1 p1) (Pow e2 p2)) = if simplify vrs e1 == simplify vrs e2                                         -- Subtraction of powers. (x ^ 2) / (x ^ 2) = x ^ (2-2)
                                                  then Pow (simplify vrs e1) (simplify vrs (p1 !- p2))
                                                  else (Div (Pow (simplify vrs e1) (simplify vrs p1)) 
                                                       (Pow (simplify vrs e2) (simplify vrs p2))) 
  simplify vrs (Div (Pow e1 p1) e2) = simplify vrs (Div (Pow e1 p1) (Pow e2 (Const 1)))                                      -- Treats any equation as being that equation to the power of one. Used for subtraction of powers.
  simplify vrs (Div e1 (Pow e2 p2)) = simplify vrs (Div (Pow e1 (Const 1)) (Pow e2 p2))                                      -- Treats any equation as being that equation to the power of one. Used for subtraction of powers.
  simplify vrs (Div e1 (Var e2)) = case simplify vrs (Var e2) of                                                             -- Const c / Variable v = Const c / Var v (if v is not defined in vrs) OR Const (c * v) (if v is defined in vrs)
                                      Var v -> Div (simplify vrs e1) (Var v)
                                      Const c -> simplify vrs (Div (simplify vrs e1) (Const c))
  simplify vrs (Div (Var e1) e2) = case simplify vrs (Var e1) of                                                             -- Variable v / Const c = Var v / Const c (if v is not defined in vrs) OR Const (v + c) (if v is defined in vrs)
                                      Var v -> Div (Var v) (simplify vrs e2)
                                      Const c -> simplify vrs (Div (Const c) (simplify vrs e2))
  
  simplify vrs (Div e1 e2) = (Div (simplify vrs e1) (simplify vrs e2))                                                        -- Expr a / Expr b = Simplify Expr a / Simplify Expr b


  --Simplification for powers.
  simplify vrs (Pow e1 (Const 0)) = Const 1                                                                                   -- Anything to the power of 0 = 1.
  simplify vrs (Pow (Const 0) e2) = Const 1                                                                                   -- Anything to the power of 0 = 1.
  simplify vrs (Pow (Const 1) e2) = simplify vrs e2                                                                           -- Anything to the power of 1 = that same anything.
  simplify vrs (Pow e1 (Const 1)) = simplify vrs e1                                                                           -- Anything to the power of 1 = that same anything.
  simplify vrs (Pow e1 e2) = (Pow (simplify vrs e1) (simplify vrs e2))                                                        -- Simplifies both base and power.
  
  

  --Simplification for variables.
  simplify vrs (Var x) = case Map.lookup x vrs of                                                                            -- If the variable is defined in vrs, return the constant it is defined as, otherwise return the variable back.
                       Just v  -> Const v
                       Nothing -> Var x


  simplify _ e = e                                                                                                            -- When no more simplification is possible, just return the expression back.

  {- ------------------------------------------------------------------------------------------------------------
   - partDiff function
   - ------------------------------------------------------------------------------------------------------------
  -}

  partDiff var (Add e1 e2) = Add (partDiff var e1) (partDiff var e2)                                                          -- Partial differentiation for addition.
  partDiff var (Sub e1 e2) = Sub (partDiff var e1) (partDiff var e2)                                                          -- Partial differentiation for subtraction.
  partDiff var (Mult e1 e2) = Add (Mult (partDiff var e1) e2) (Mult e1 (partDiff var e2))                                     -- Partial differentiation for multiplication (product rule).
  partDiff var (Div e1 e2) = Div (Sub (Mult (partDiff var e1) e2) (Mult e1 (partDiff var e2))) (Mult e2 e2)                   -- Partial differentiation for division (quotient rule).
  partDiff var (Cos e) = Mult (Mult (Const (-1)) (Sin e)) (partDiff var e)                                                    -- Partial differentiation for cos (using chain rule).
  partDiff var (Sin e) = Mult (Cos e) (partDiff var e)                                                                        -- Partial differentiation for sin (using chain rule).
  partDiff var (Log e) = Mult (Div (Log (Exp (Const 1))) (Mult e (Log e))) (partDiff var e)                                   -- Partial differentiation for log.
  partDiff var (Exp e) = Mult (Exp e) (partDiff var e)                                                                        -- Partial differentiation for exp.
  partDiff var (Const x) = Const 0                                                                                            -- Partial differentiation for constants.
  partDiff var (Var x) = if var == x                                                                                          -- Partial differentiation for variables (if it's the variable that is being derived, derive it, otherwise, treat it as constant).
                          then Const 1
                          else
                            Const 0

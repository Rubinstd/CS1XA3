{-|
Module : ExprDiff
Description : Contains functions for parsing strings into
              (Expr a) variables where a is Float or Double.
Copyright : (c) Daniel Rubinstein @2018
License : WTFPL
Maintainer : rubinstd@mcmaster.ca
Stability : experimental
Portability : POSIX

Strings to be parsed must be formated in a specific way. Below the parsing functions is a description of the format a string needs to be in to parse into the 'Expr' type.
-}




module ExprParser (parseExprD,parseExprF) where

import           ExprType

import           Text.Parsec
import           Text.Parsec.String

-- | Formats a given string into an expression of type 'Expr' Double. The format of the string must be:
--
--     * Addition        -> Signified by !+
--     * Subtraction     -> Signified by !-
--     * Multiplication  -> Signified by !*
--     * Division        -> Signified by !/
--     * Power           -> Signified by !^
--     * Cos             -> Signified by cos' (expression)
--     * Sin             -> Signified by sin' (expression)
--     * Log             -> Signified by log' (expression)
--     * Exp             -> Signified by exp' (expression)
--     * Vectors         -> Signified by vect [list of expressions]
--     * Constants       -> Signified by simplly writing the constant value.
--     * Variable        -> Signified by var (variable name as string)
parseExprD :: String          -- ^ The string to be parsed.
           -> Expr Double     -- ^ The resulting Expr Double
parseExprD ss = case parse exprD "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

-- | Formats a given string into an expression of type 'Expr' Float. The format of the string must be:
--
--     * Addition        -> Signified by !+
--     * Subtraction     -> Signified by !-
--     * Multiplication  -> Signified by !*
--     * Division        -> Signified by !/
--     * Power           -> Signified by !^
--     * Cos             -> Signified by cos' (expression)
--     * Sin             -> Signified by sin' (expression)
--     * Log             -> Signified by log' (expression)
--     * Exp             -> Signified by exp' (expression)
--     * Vectors         -> Signified by vect [list of expressions]
--     * Constants       -> Signified by simply writing the constant value.
--     * Variable        -> Signified by var (variable name as string)
parseExprF :: String          -- ^ The string to be parsed.
           -> Expr Float      -- ^ The resulting Expr Float
parseExprF ss = case parse exprF "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

-- | Parser for parsing into 'Expr' Double. Parses the addition and subtraction operators.
exprD :: Parser (Expr Double)
exprD = termsD `chainl1` addop

-- | Parser for parsing into 'Expr' Double. Parses the multiplication and division operators.
termsD = factorD `chainl1` mulop

-- | Parser for parsing into 'Expr' Double. Parses brackets, constant terms, vectors, trigonometric terms, logorithm related terms (including Log and Exp), and variables.
factorD = parens exprD 
      <|> constTerm double
      <|> vectorTerm exprD 
      <|> trigTerm double exprD 
      <|> logTerm double exprD 
      <|> variable

-- | Parser for parsing into 'Expr' Float. Parses the addition and subtraction operators.
exprF :: Parser (Expr Float)
exprF = termsF `chainl1` addop

-- | Parser for parsing into 'Expr' Float. Parses the multiplication and division operators.
termsF = factorF `chainl1` mulop

-- | Parser for parsing into 'Expr' Float. Parses brackets, constant terms, vectors, trigonometric terms, logorithm related terms (including Log and Exp), and variables.
factorF = parens exprF
      <|> constTerm float
      <|> vectorTerm exprF 
      <|> trigTerm float exprF
      <|> logTerm float exprF
      <|> variable 


{- ------------------------------------------------------------------------------------------------------------
 - Parsing an Expression (General functions)
 - ------------------------------------------------------------------------------------------------------------
 -}

-- | Takes a parser for values of type a and parses values of this type into 'Expr' a type Const expressions.
constTerm :: Parser a         -- ^ The parser for values of type a.
          -> Parser (Expr a)  -- ^ The resulting parser for 'Expr' a expression.
constTerm p = do {
           xs <- p;
           return $ Const xs }

-- | Takes a parser for values of type a as well as a parser for expressions of type a and parses values of this type into 'Expr' a type Cos and Sin expressions.
trigTerm :: Parser a          -- ^ The parser for values of type a.
         -> Parser (Expr a)   -- ^ The parser for 'Expr' a expressions.
         -> Parser (Expr a)   -- ^ The resulting 'parser for Expr' a expression.
trigTerm p1 p2 = do 
            {
                symbol "cos'";
                xs <- try (parens p2) <|> (parens variable) <|> (parens (constTerm p1));
                return $ Cos xs 
            }
        <|> do 
            {
                symbol "sin'";
                xs <- try (parens p2) <|> (parens variable) <|> (parens (constTerm p1)) ;
                return $ Sin xs 
            }

-- | Takes a parser for values of type a and parses values of this type into 'Expr' a type Vect expressions.
vectorTerm :: Parser (Expr a)  -- ^ The parser for values of type a.
           -> Parser (Expr a)  -- ^ The resulting parser for 'Expr' a expression.
vectorTerm p = do { 
                    symbol "vect";
                    symbol "[" ;
                    xs <- sepBy p (symbol ",") ;
                    symbol "]" ;
                    return $ Vect xs 
                  }

-- | Parses strings into 'Expr' a type Var expressions.
variable :: Parser (Expr a) -- ^ The resulting parser for 'Expr' a expression.
variable = do {
           xs <- many1 letter;
           return $ Var xs }

-- | Parses multiplication, division, and power operators in a string into 'Expr' a type expressions.
mulop :: Parser (Expr a -> Expr a -> Expr a) -- ^ The resulting parser for 'Expr' a expression.
mulop = do { symbol "!*"; return Mult }
    <|> do{ symbol "!/"; return Div }
    <|> do { symbol "!^"; return Pow}

-- | Parses addition and subtraction in a string into 'Expr' a type expressions.
addop :: Parser (Expr a -> Expr a -> Expr a) -- ^ The resulting parser for 'Expr' a expression.
addop = do{ symbol "!+"; return Add }
    <|> do{ symbol "!-"; return Sub }

-- | Takes a parser for values of type a as well as a parser for expressions of type a and parses values of this type into 'Expr' a type Log and Exp expressions.
logTerm :: Parser a         -- ^ The parser for values of type a.
        -> Parser (Expr a)  -- ^ The parser for 'Expr' a expressions.
        -> Parser (Expr a)  -- ^ The resulting parser for 'Expr' a expression.
logTerm p1 p2 = do 
            {
                symbol "log'";
                xs <- try (parens p2) <|> (parens variable) <|> (parens (constTerm p1));
                return $ Log xs 
            }
        <|> do 
            {
                symbol "exp'";
                xs <- try (parens p2) <|> (parens variable) <|> (parens (constTerm p1)) ;
                return $ Exp xs 
            }


{- ------------------------------------------------------------------------------------------------------------
 - Double and Float Parsing
 - ------------------------------------------------------------------------------------------------------------
 -}

-- | A parser for the digits of a number values. Parses both positive and negative digits.
doubleDigits :: Parser String -- ^ The resulting 'Parser' 'String' for digits.
doubleDigits = do { ds <- try negDigits <|> digits ;
                    rs <- try decimalDigits <|> return "" ;
                    return $ ds ++ rs }

-- | A parser for decimal digits.
decimalDigits :: Parser String -- ^ The resulting Parser String for decimal digits.
decimalDigits = do { d <- char '.' ;
                     rm <- digits ;
                     return $ d:rm }

-- | A parser for parsing 'Double' type values.
double :: Parser Double -- ^ The resulting 'Parser' Double for 'Double' values.
double = fmap read $ doubleDigits


-- | A parser for 'Float' type values.
float :: Parser Float -- ^ The resulting 'Parser' 'Float' for 'Float' type values.
float = fmap read $ doubleDigits

{- ------------------------------------------------------------------------------------------------------------
 - Utility Combinators
 - ------------------------------------------------------------------------------------------------------------
 -}
-- | Parser for parsing data within brackets.
parens :: Parser a          -- ^ The parser used for values within the brackets.
       -> Parser a          -- ^ The resulting values from the Parser.
parens p = do { symbol "(";
                cs <- p;
                symbol ")";
                return cs }

-- | Parser for parsing a given string.
symbol :: String            -- ^ The desired string to parse for.
       -> Parser String     -- ^ The resulting parser.
symbol ss = let
  symbol' :: Parser String
  symbol' = do { spaces;
                 ss' <- string ss;
                 spaces;
                 return ss' }
  in try symbol'

-- | Parser for parsing digits.
digits :: Parser String    -- ^ The resulting parser.
digits = many1 digit

-- | Parser for parsing negative digits.
negDigits :: Parser String -- ^ The resulting parser.
negDigits = do { neg <- symbol "-" ;
                 dig <- digits ;
                 return (neg ++ dig) }

-- | Parser for parsing 'Integer' type values.
integer :: Parser Integer  -- ^ The resulting parser.
integer = fmap read $ try negDigits <|> digits
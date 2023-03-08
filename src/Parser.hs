https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
{-# LANGUAGE Haskell2010, FlexibleContexts, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}

module Parser (
  parseAll,
  parseProgram,
  parseProgram'
  )
where

-- TODO: introduce precedence & try

import qualified Data.Map.Strict as M
import Data.Char (isLower, isUpper)
import Data.Functor
import Data.List (foldl1')
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Syntax
import Color
import GHC.Num (integerToInt)
import Control.Applicative (liftA2, liftA3)

-- Additional parser state
type ParserState = ()

-- Definition for tokens
languageDef = P.LanguageDef {
  -- Java-style block comments
  P.commentStart = "/*",
  P.commentEnd = "*/",
  -- Java-style line comments
  P.commentLine = "//",
  -- Allow nested comments
  P.nestedComments = True,
  P.identStart = letter <|> char '_',
  P.identLetter = alphaNum <|> oneOf "_'",
  P.opStart = oneOf "<=>/+-*!&|",
  P.opLetter = oneOf "<=>/+-*&|:.",
  P.reservedNames = ["true"
                    , "false"
                    , "x"
                    , "y"
                    , "r"
                    , "theta"
                    , "scale"
                    , "translate"
                    , "rotate"
                    , "swirl"
                    , "juxtapose"
                    , "if"
                    , "else"
                    , "replicate"
                    , "flip"
                    , "overlay"
                    , "sin"
                    , "cos"
                    , "tan"
                    , "sqrt"
                    ] ++ map fst namedColors,
  P.reservedOpNames = ["="],
  P.caseSensitive = True
  }

namedColors = [ ("red", red)
              , ("green", green)
              , ("blue", blue)
              , ("yellow", yellow)
              , ("black", black)
              , ("white", white)
              ]

lexer = P.makeTokenParser languageDef

inparen = P.parens lexer
inbrace = P.braces lexer
inbracket = P.brackets lexer
op = P.reservedOp lexer
-- | keywords
kw = P.reserved lexer

parseInt :: Parsec String u Int
parseInt = integerToInt <$> P.natural lexer

parseFloat = either fromIntegral id <$> P.naturalOrFloat lexer

-- a helper that fails the parser if given predicate is false on the parser's
-- return data
parser <??> pred = parser >>= \a -> if pred a then return a else parserZero
infixl 6 <??>

-- a helper that checks that the parsed value is same as the right-hand side
parser <?=> value = parser <??> (== value)
infixl 6 <?=>

atomicExpr :: Parsec String ParserState Expr
atomicExpr = try keyword
  <|> ColorLit <$> parseNamedColor
  <|> parseColor
  <|> VarE <$> var
  <|> inparen expr
  <|> inbrace expr
  <|> GrayLit <$> parseFloat
  <|> UnOp <$> BuiltIn <$> trigFn <*> inparen expr
  <|> kw "flip"     $> Flip <*> inparen expr
  <|> kw "scale"     *> (try (liftA2 Scale (P.symbol lexer "(" *> arithExpr <* comma) (expr <* P.symbol lexer ")"))
                         <|> liftA3 Scale2 (P.symbol lexer "(" *> arithExpr <* comma) (arithExpr <* comma) (expr <* P.symbol lexer ")"))
  <|> kw "swirl"     *> liftA2 Swirl (P.symbol lexer "(" *> arithExpr <* comma) (expr <* P.symbol lexer ")")
  <|> kw "rotate"    *> liftA2 Rotate (P.symbol lexer "(" *> arithExpr <* comma) (expr <* P.symbol lexer ")")
  <|> kw "replicate"    *> liftA2 Replicate (P.symbol lexer "(" *> arithExpr <* comma) (expr <* P.symbol lexer ")")
  <|> kw "overlay"    *> liftA2 Overlay (P.symbol lexer "(" *> expr <* comma) (expr <* P.symbol lexer ")")
  <|> kw "translate" *> liftA3 Translate (P.symbol lexer "(" *> arithExpr <* comma) (arithExpr <* comma) (expr <* P.symbol lexer ")")
  <|> kw "juxtapose" *> (juxta <$> inparen (sepEndBy1 expr comma))
  <|> ifExpr
  where
    ifExpr = kw "if" $> If <*> expr <*> inbrace expr <*> (kw "else" *> inbrace expr)
    juxta [] = undefined
    juxta (e : es) = Juxtapose e es

trigFn = foldl1 (<|>) $ map kwParser [ ("sin",  Sin)
                                     , ("cos",  Cos)
                                     , ("tan",  Tan)
                                     , ("sqrt", Sqrt)]

parseColor = inbracket $
  liftA4 ColorE expr channel channel (option (GrayLit 1.0) $ try channel)
  where
    channel = comma *> expr
    liftA4 f x y z t = pure f <*> x <*> y <*> z <*> t

parseNamedColor = foldl1 (<|>) $ map kwParser namedColors

keyword = foldl1 (<|>) $ map kwParser [("x", X), ("y", Y), ("r", R), ("pi", GrayLit pi), ("theta", Theta), ("true", MaskLit True), ("false", MaskLit False)]

kwParser (k, e) = kw k $> e

var = try $ P.identifier lexer

-- expression grammar for terms including operators
exprOp :: Parsec String ParserState Expr
exprOp = buildExpressionParser table atomicExpr
  where
    -- operator precedence table, everything is left-associative
    table = [ [unOp "-" Negate]
            , [op' "*" Mul, op' "/" Div]
            , [op' "+" Add, op' "-" Sub]
            , [op' "<" Less, op' "==" Eq, flipOp ">" Less]
            , [unOp "!" Not]
            , [op' "^" Xor]
            , [op' "&&" And]
            , [op' "||" Or]
            ] ++ [[jux "<..>" JuxtaposeH, jux "<:>" JuxtaposeV]]
    -- infix operator parser
    op' literalOp astOp = Infix (op literalOp $> opAction) AssocLeft
      where
        opAction lhs rhs = BinOp lhs astOp rhs
    -- flipped operators
    flipOp literalOp astOp = Infix (op literalOp $> opAction) AssocLeft
      where
        opAction lhs rhs = BinOp rhs astOp lhs
    -- unary operator parser
    unOp literalOp astOp = Prefix (op literalOp $> UnOp astOp)
    -- juxtaposition operators
    jux literalOp astOp = Infix (op literalOp $> astOp) AssocLeft

expr = letE <|> exprOp

arithExpr :: Parsec String ParserState Arith
arithExpr = buildExpressionParser table atom
  where
    atom = Pi <$ kw "pi"
      <|> NumLit <$> parseFloat
      <|> inparen arithExpr
      <|> UnOpA . BuiltInA <$> trigFn <*> inparen arithExpr
    -- operator precedence table, everything is left-associative
    table = [ [unOp "-" NegateA]
            , [op' "*" MulA, op' "/" DivA]
            , [op' "+" AddA, op' "-" SubA]
            ]
    -- infix operator parser
    op' literalOp astOp = Infix (op literalOp $> opAction) AssocLeft
      where
        opAction lhs rhs = BinOpA lhs astOp rhs
    flipOp literalOp astOp = Infix (op literalOp $> opAction) AssocLeft
      where
        opAction lhs rhs = BinOpA rhs astOp lhs
    unOp literalOp astOp = Prefix (op literalOp $> UnOpA astOp)

letE = try (lookAhead (var >> op "=")) >> do
  x <- var
  op "="
  e1 <- expr
  semi
  e2 <- expr
  return $ Let x e1 e2

comma = P.comma lexer $> ()
semi = P.semi lexer $> ()
dot = P.dot lexer $> ()

program = do
  P.whiteSpace lexer
  resolution <- inparen $ liftA2 (,) (parseInt <* comma) parseInt
  semi
  image <- expr
  return $ Program { image, resolution }

parseAll p = runParser (p <* eof) ()

-- | Same as parseProgram but uses given source file name in parse errors.
parseProgram' = parseAll program

parseProgram = parseProgram' ""

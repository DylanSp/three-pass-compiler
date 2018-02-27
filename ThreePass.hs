module TinyThreePassCompiler where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Text.Parsec
import qualified Text.Parsec.Token as Tok

-- count number of arguments declared, how identifiers map to argument numbers
type CompilerState = (Int, Map String Int) 
type Parser a = Parsec String CompilerState a

langDef :: Tok.LanguageDef CompilerState
langDef = Tok.LanguageDef
  { Tok.commentStart    = ""
  , Tok.commentEnd      = ""
  , Tok.commentLine     = ""
  , Tok.nestedComments  = False
  , Tok.identStart      = letter
  , Tok.identLetter     = letter
  , Tok.opStart         = oneOf "+-*/"
  , Tok.opLetter        = oneOf "+-*/"
  , Tok.reservedNames   = []
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }
  
lexer :: Tok.TokenParser CompilerState
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

integer :: Parser Integer
integer = Tok.integer lexer

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

function :: Parser AST
function = do
  brackets argList
  expression
  
argList :: Parser ()
argList = do
  many variableDec
  return ()

-- for each variable declaration, we add a mapping from the argument name to the argument number
-- as well as incrementing the Int in CompilerState, which tracks what argument number we're on
variableDec :: Parser ()
variableDec = do
  varName <- identifier
  (varNum, _) <- getState
  modifyState (\(num, args) -> (num + 1, Map.insert varName varNum args))
  return ()

expression :: Parser AST
expression = term `chainl1` addSubOp

addSubOp :: Parser (AST -> AST -> AST)
addSubOp =  (reservedOp "+" >> return Add)
        <|> (reservedOp "-" >> return Sub)

term :: Parser AST
term = factor `chainl1` multDivOp

multDivOp :: Parser (AST -> AST -> AST)
multDivOp =  (reservedOp "*" >> return Mul)
         <|> (reservedOp "/" >> return Div)
  
factor :: Parser AST
factor =  number
      <|> variableUse
      <|> parens expression

number :: Parser AST
number = do
  num <- integer
  return $ Imm $ fromIntegral num

-- using fromJust because we don't care about error handling
-- per problem, all programs will be valid
variableUse :: Parser AST
variableUse = do
  varName <- identifier
  (_, varMap) <- getState
  return $ Arg $ fromJust $ Map.lookup varName varMap

compile :: String -> [String]
compile = pass3 . pass2 . pass1

-- parsing pass
pass1 :: String -> AST
pass1 str = case (runParser function (0, Map.empty) "" str) of
  (Left _) -> error "Parse error"
  (Right ast) -> ast

-- constant folding pass
-- do a postorder traversal of the AST, checking for optimization opportunities at each node
pass2 :: AST -> AST
pass2 ast = case ast of
  Add left right -> case (pass2 left, pass2 right) of
    ((Imm m), (Imm n)) -> Imm (m + n)
    (l, r) -> Add l r
  Sub left right -> case (pass2 left, pass2 right) of
    ((Imm m), (Imm n)) -> Imm (m - n)
    (l, r) -> Sub l r
  Mul left right -> case (pass2 left, pass2 right) of
    ((Imm m), (Imm n)) -> Imm (m * n)
    (l, r) -> Mul l r
  Div left right -> case (pass2 left, pass2 right) of
    ((Imm m), (Imm n)) -> Imm (m `div` n)
    (l, r) -> Div l r
  _ -> ast

-- code generation pass
-- postorder traversal of the AST, generating code at each node
pass3 :: AST -> [String]
pass3 ast = case ast of
  Imm n -> ["IM " ++ show n, "PU"]
  Arg a -> ["AR " ++ show a, "PU"]
  Add l r -> pass3 l ++ pass3 r ++ ["PO", "SW", "PO", "AD", "PU"]
  Sub l r -> pass3 l ++ pass3 r ++ ["PO", "SW", "PO", "SU", "PU"]
  Mul l r -> pass3 l ++ pass3 r ++ ["PO", "SW", "PO", "MU", "PU"]
  Div l r -> pass3 l ++ pass3 r ++ ["PO", "SW", "PO", "DI", "PU"]

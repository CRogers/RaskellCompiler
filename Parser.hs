{-# LANGUAGE NoMonomorphismRestriction, GADTs #-}

module Parser where

import Data.Char

import Text.Parsec
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Prim
import qualified Text.Parsec.Token as PT

import Tree
import PrettyPrint

testP p str = runParser p () "" str

operators = oneOf "+-*/=!@#$%^&<>?\\|~"

langDef = PT.LanguageDef {
	PT.commentStart    = "{-",
	PT.commentEnd      = "-}",
	PT.commentLine     = "--",
	PT.nestedComments  = True,
	PT.identStart      = letter <|> char '_',
	PT.identLetter     = alphaNum <|> char '_',
	PT.opStart         = operators,
	PT.opLetter        = operators,
	PT.reservedNames   = ["let", "in", "if", "then", "else", "case", "of", "where", "type", "data"],
	PT.reservedOpNames = ["=", "|", ";"],
	PT.caseSensitive   = False
}

lexer = PT.makeTokenParser langDef

ident = PT.identifier lexer
reserved = PT.reserved lexer
reseredOp = PT.reservedOp lexer
parens = PT.parens lexer
integer = PT.integer lexer
ws = PT.whiteSpace lexer
symbol = PT.symbol lexer


toPos :: SourcePos -> Pos
toPos p = Pos (sourceName p) (sourceLine p) (sourceColumn p)

annot parser = do
	spos <- getPosition
	x <- parser
	epos <- getPosition
	return $ Annot (toPos spos) (toPos epos) x


constInt = annot $ do
	i <- integer
	return $ ConstInt i

var = annot $ do
	n <- ident
	return $ if isUpper (head n) then TypeVar n else Var n

exprBasic =
	    constInt
	<|> var
	<|> parens expr

app = annot $ do
	f  <- exprBasic
	es <- many1 exprBasic
	return $ App f es

lambda = annot $ do
	symbol "\\"
	ps <- many1 param
	symbol "->"
	e <- expr
	return $ Abs ps e

ifThenElse = annot $ do
	symbol "if"
	e1 <- expr
	symbol "then"
	e2 <- expr
	symbol "else"
	e3 <- expr
	return $ If e1 e2 e3

defn = annot $ do
	n <- ident
	symbol "="
	e <- expr
	return $ ValBind n e

letIn = annot $ do
	symbol "let"
	d <- defn
	symbol "in"
	e <- expr
	return $ Let [d] e

caseStmt = annot $ do
	p <- ident
	symbol "->"
	e <- expr
	return $ CaseStmt p e

caseOf = annot $ do
	symbol "case"
	e <- expr
	symbol "of"
	symbol "{"
	cs <- sepBy1 caseStmt (symbol "|")
	return $ Case e cs

expr =
	    lambda
	<|> ifThenElse
	<|> letIn
	<|> try caseOf
	<|> try app
	<|> exprBasic

param = ident

funcDef = annot $ do
	n <- ident
	ps <- many param
	symbol "="
	e <- expr
	return $ FuncDef n (Abs ps e)

typeDef = annot $ do
	symbol "type"
	n <- ident
	return $ TypeDef n

alternatives = sepBy1 ident (symbol "|")

dataDef = annot $ do
	symbol "data"
	n <- ident
	symbol "="
	as <- alternatives
	return $ DataDef n as

def = annot $ do
	d <- typeDef <|> dataDef <|> funcDef
	symbol ";"
	return d

start = annot $ do
	ws
	ds <- many def
	return $ Module ds

a = case testP funcDef "f x y = (a b) (if c then f else g) d;" of Right x -> x
b = case testP funcDef "f x = let a = x in let b = a in b;" of Right (FuncDef _ x) -> x
alts = case testP start "data foo = Bar | baz | Quux;" of Right x -> x
tea = case testP start "type Cat; type mat; data sat = Meow | Foo; A x = y;" of Right x -> x

pp =  (case testP lambda "\\x y -> (if a then b else c) (let a = b in c) z" of Right x -> x)
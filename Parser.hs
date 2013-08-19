{-# LANGUAGE NoMonomorphismRestriction, GADTs #-}

module Parser where

import Data.Char

import Text.Parsec
import Text.Parsec.Prim
import qualified Text.Parsec.Token as PT

import Tree

testP p str = runParser p () "" str

operators = oneOf "+-*/=!@#$%^&<>?\\|~"

langDef = PT.LanguageDef {
	PT.commentStart    = "{-",
	PT.commentEnd      = "-}",
	PT.commentLine     = "--",
	PT.nestedComments  = True,
	PT.identStart      = lower <|> char '_',
	PT.identLetter     = alphaNum <|> char '_',
	PT.opStart         = operators,
	PT.opLetter        = operators,
	PT.reservedNames   = ["let", "in", "if", "then", "else", "where", "type", "data"],
	PT.reservedOpNames = ["="],
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

constInt = integer >>= return . ConstInt

var = do
	n <- ident
	return $ if isUpper (head n) then TypeVar n else Var n

exprBasic =
	    constInt
	<|> var
	<|> parens expr

app = do
	f  <- exprBasic
	es <- many1 exprBasic
	return $ App f es

lambda = do
	symbol "\\"
	ps <- many1 param
	symbol "->"
	e <- expr
	return $ Abs ps e

ifThenElse = do
	symbol "if"
	e1 <- expr
	symbol "then"
	e2 <- expr
	symbol "else"
	e3 <- expr
	return $ If e1 e2 e3

defn = do
	n <- ident
	symbol "="
	e <- expr
	return $ ValBind n e

letIn = do
	symbol "let"
	d <- defn
	symbol "in"
	e <- expr
	return $ Let [d] e


expr =
	    lambda
	<|> ifThenElse
	<|> letIn
	<|> try app
	<|> exprBasic

param = ident

funcDef = do
	n <- ident
	ps <- many param
	symbol "="
	e <- expr
	return $ FuncDef n (Abs ps e)

typeDef = do
	symbol "type"
	n <- ident
	return $ TypeDef n

def = do
	d <- typeDef <|> funcDef
	symbol ";"
	return d

start = do
	ws
	many def

a = case testP funcDef "f x y = (a b) (if c then f else g) d;" of Right (FuncDef _ x) -> x
b = case testP funcDef "f x = let a = x in let b = a in b;" of Right (FuncDef _ x) -> x
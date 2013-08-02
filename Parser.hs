{-# LANGUAGE NoMonomorphismRestriction #-}

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
	PT.reservedNames   = ["let", "in", "if", "then", "else", "where"],
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
	es <- many1 expr
	return $ App f es

lambda = do
	symbol "\\"
	ps <- many1 param
	symbol "->"
	e <- expr
	return $ Abs ps e

expr =
	    lambda
	<|> try app
	<|> exprBasic

param = ident

func = do
	n <- ident
	ps <- many param
	symbol "="
	e <- expr
	return $ FuncDef n (Abs ps e)

a = case testP func "f x y = (a b) c d" of Right (FuncDef _ x) -> x
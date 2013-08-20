{-# LANGUAGE KindSignatures, GADTs, StandaloneDeriving, FlexibleInstances, UndecidableInstances #-} 

module Tree where

import Control.Applicative
import Data.Traversable
import Data.List
import Data.Map (Map)
import Compos
import PrettyPrint
import Util

type Name = String
type Param = String
type Pattern = String

type Line = Int
type Col = Int
type SourceName = String

data Pos = Pos SourceName !Line !Col
	deriving (Show, Eq, Ord)

data SourceSect = SourceSect Pos Pos
	deriving (Show, Eq)

type SourceStack = [SourceSect]

type Sources = Map String [String]

data Expr_
data Binding_
data Def_
data Module_
data CompUnit_
data CaseStmt_

type Expr     = Tree Expr_
type Binding  = Tree Binding_
type Def      = Tree Def_
type Module   = Tree Module_
type CompUnit = Tree CompUnit_
type CaseStmt = Tree CaseStmt_

data Tree :: * -> * where
	Annot    :: Pos       -> Pos        -> (Tree a) -> (Tree a)
	Var      :: Name      -> Expr
	TypeVar  :: Name      -> Expr
	ConstInt :: Integer   -> Expr
	Abs      :: [Param]   -> Expr       -> Expr
	App      :: Expr      -> [Expr]     -> Expr
	If       :: Expr      -> Expr       -> Expr -> Expr
	Let      :: [Binding] -> Expr       -> Expr
	Case     :: Expr      -> [CaseStmt] -> Expr
	CaseStmt :: Pattern   -> Expr       -> CaseStmt
	ValBind  :: Pattern   -> Expr       -> Binding
	FuncDef  :: Name      -> Expr       -> Def
	TypeDef  :: Name      -> Def
	DataDef  :: Name      -> [Name]     -> Def
	Module   :: [Def]     -> Module
	CompUnit :: Sources   -> [Module] -> CompUnit

deriving instance Show (Tree a)
deriving instance Eq (Tree a)

instance Compos Tree where
	compos f t =
		case t of
			Annot sp ep e-> pure Annot   <*> pure sp       <*> pure ep <*> f e
			Abs ps e     -> pure Abs     <*> pure ps       <*> f e
			App e es     -> pure App     <*> f e           <*> traverse f es
			If e1 e2 e3  -> pure If      <*> f e1          <*> f e2  <*> f e3
			Let bs e     -> pure Let     <*> traverse f bs <*> f e
			Case e cs    -> pure Case    <*> f e           <*> traverse f cs
			CaseStmt p e -> pure CaseStmt <*> pure p        <*> f e 
			ValBind n e  -> pure ValBind <*> pure n        <*> f e
			FuncDef n e  -> pure FuncDef <*> pure n        <*> f e
			TypeDef n    -> pure TypeDef <*> pure n
			DataDef n as -> pure DataDef <*> pure n        <*> pure as
			Module ds    -> pure Module  <*> traverse f ds
			CompUnit fs ms -> pure CompUnit <*> pure fs <*> traverse f ms
			_ -> pure t

stripAnnots :: Tree a -> Tree a
stripAnnots t =
	case t of
		Annot _ _ e -> stripAnnots e
		_           -> composOp stripAnnots t 

wrapParens :: [PrettyInstr] -> [PrettyInstr]
wrapParens ps = Text "(" : ps ++ [Text ")"]

t str = [Text str]
mapPrettyText sep as = joinL [Text sep] (map pretty as)

instance PrettyPrint (Tree a) where
	pretty tr =
		case tr of
			Annot _ _ e  -> pretty e
			Var n        -> t n
			Abs ps e     -> wrapParens ((Text $ "\\" ++ joinL " " ps ++ " -> ") : pretty e)
			App e es     -> if length es > 0 then wrapParens pp else pp
				where pp = mapPrettyText " " (e:es)
			If e1 e2 e3  -> wrapParens $ t "if " ++ pretty e1 ++ t " then " ++ pretty e2 ++ t " else " ++ pretty e3
			Let bs e     -> wrapParens $ t "let " ++ mapPrettyText " and " bs ++ [Text " in", Newline] ++ pretty e

			_ -> [Text "foo"]





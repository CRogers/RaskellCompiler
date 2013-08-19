{-# LANGUAGE KindSignatures, GADTs, StandaloneDeriving, FlexibleInstances, UndecidableInstances #-} 

module Tree where

import Control.Applicative
import Data.Traversable
import Compos

type Name = String
type Param = String

data Expr_
data Binding_
data Def_
data Module_

type Expr    = Tree Expr_
type Binding = Tree Binding_
type Def     = Tree Def_
type Module  = Tree Module_

data Tree :: * -> * where
	Var      :: Name      -> Expr
	TypeVar  :: Name      -> Expr
	ConstInt :: Integer   -> Expr
	Abs      :: [Param]   -> Expr   -> Expr
	App      :: Expr      -> [Expr] -> Expr
	If       :: Expr      -> Expr   -> Expr    -> Expr
	Let      :: [Binding] -> Expr   -> Expr
	ValBind  :: Name      -> Expr   -> Binding
	FuncDef  :: Name      -> Expr   -> Def
	TypeDef  :: Name      -> Def
	DataDef  :: Name      -> [Name] -> Def
	Module   :: [Def]     -> Module

deriving instance Show (Tree a)
deriving instance Eq (Tree a)

instance Compos Tree where
	compos f t =
		case t of
			Abs ps e     -> pure Abs     <*> pure ps       <*> f e
			App e es     -> pure App     <*> f e           <*> traverse f es
			If e1 e2 e3  -> pure If      <*> f e1          <*> f e2  <*> f e3
			Let bs e     -> pure Let     <*> traverse f bs <*> f e
			ValBind n e  -> pure ValBind <*> pure n        <*> f e
			FuncDef n e  -> pure FuncDef <*> pure n        <*> f e
			TypeDef n    -> pure TypeDef <*> pure n
			DataDef n as -> pure DataDef <*> pure n        <*> pure as
			Module ds    -> pure Module  <*> traverse f ds
			_ -> pure t
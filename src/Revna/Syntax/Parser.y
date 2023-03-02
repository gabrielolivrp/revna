{
{-# LANGUAGE PatternSynonyms #-}

module Revna.Syntax.Parser (
  runParseModule,
  runParseExpr,
  runParseDecl,
) where

import Data.List.NonEmpty qualified as NE
import Data.ByteString qualified as BS
import Revna.Location
import Revna.Diagnostic
import Revna.Syntax.Lexer
import Revna.Syntax.Monad
import Revna.Syntax.Token
import Revna.Syntax.Tree
}

%name parseModule Module
%name parseDecl Decl
%name parseExpr Expr

%monad { ParserM }
%tokentype { Loc Token }
%lexer { lexer } { PToken TkEOF }
%error { failure }

%token
  module                    { PToken KwModule }
  where                     { PToken KwWhere }
  let                       { PToken KwLet }
  in                        { PToken KwIn }
  postulate                 { PToken KwPostulate }
  forall                    { PToken SymForall }
  lambda                    { PToken SymLambda }
  "->"                      { PToken SymArrow }
  "("                       { PToken SymLParen }
  ")"                       { PToken SymRParen }
  ":"                       { PToken SymColon }
  "="                       { PToken SymEq }
  ","                       { PToken SymComma }
  "=>"                      { PToken SymDoubleArrow }
  vopen                     { PToken TkVOpen }
  vclose                    { PToken TkVClose }
  vsemi                     { PToken TkVSemi }
  name                      { PToken (TkIdent _) }

%nonassoc ":"
%right "->" "=>" "="

%%

Module :: { Module }
Module
  : module name where TopLevels                   { Module (getName $2) $4 }

-- | TopLevel
TopLevels :: { [TopLevel] }
TopLevels
  : vopen Decls vclose                            { $2 }

Decls :: { [TopLevel] }
Decls
  : Decl vsemi Decls                              { $1 : $3 }
  | Decl                                          { [$1] }
  | {- empty -}                                   { [] }

Decl :: { TopLevel }
Decl
  : name ":" Expr                                 { FunTypeDecl (withSpan2 $1 $3) (getName $1) $3 }
  | name "=" Expr                                 { FunDecl (withSpan2 $1 $3) (getName $1) $3 }
  | postulate name ":" Expr                       { PostulateDecl (withSpan2 $1 $4) (getName $2) $4 }

-- | Expr
Atom :: { Tree }
Atom
  : name                                          { Var (withSpan1 $1) (getName $1)  }
  | "(" Expr ")"                                  { $2 }

Expr :: { Tree }
Expr
  : forall ListBinder ","  Expr                   { Forall (withSpan2 $1 $4) $2 $4}
  | lambda ListBinder "=>" Expr                   { Lam (withSpan2 $1 $4) $2 $4}
  | Expr "->" Expr                                { Arrow (withSpan2 $1 $3) $1 $3 }
  | Expr Exprs                                    { App (withSpan2 $1 ((NE.head . NE.reverse) $2)) $1 $2 }
  | let LetBlock in Expr                          { Let (withSpan2 $1 $4) $2 $4 }
  | Expr "=" Expr                                 { Eq (withSpan2 $1 $3) $1 $3 }
  | Atom                                          { $1 }

Exprs :: { NE.NonEmpty Tree }
Exprs
  : Expr Exprs                                    { NE.cons $1 $2 }
  | Expr                                          { NE.singleton $1 }

LetBlock :: { NE.NonEmpty (Name, Tree, Tree) }
LetBlock
  : vopen LetDecls vclose              { $2 }

LetDecls :: { NE.NonEmpty (Name, Tree, Tree) }
  : LetBinder vsemi LetDecls                      { NE.cons $1 $3 }
  | LetBinder                                     { NE.singleton $1 }

LetBinder :: { (Name, Tree, Tree) }
LetBinder
  : name ":" Expr "=" Expr                        { (getName $1, $3, $5) }

Binder :: { Bind }
Binder
  : name ":" Expr                                 { Bind (withSpan2 $1 $3) (getName $1) $3 }
  | "(" Binder ")"                                { $2 }

ListBinder :: { NE.NonEmpty Bind }
ListBinder
  : Binder ListBinder                             { NE.cons $1 $2 }
  | Binder                                        { NE.singleton $1 }

{
pattern PToken :: Token -> Loc Token
pattern PToken x <- Loc x _

getName :: Loc Token -> Name
getName (Loc (TkIdent x) _) = Name x

failure :: Loc Token -> ParserM a
failure (Loc tok span) = unexpectedTokenError tok span

runParseModule :: FilePath -> BS.ByteString -> Either Diagnostic Module
runParseModule filepath text = runP filepath text [] parseModule

runParseExpr :: BS.ByteString -> Either Diagnostic Tree
runParseExpr text = runP "<<stdin>>" text [] parseExpr

runParseDecl :: BS.ByteString -> Either Diagnostic TopLevel
runParseDecl text = runP "<<stdin>>" text [] parseDecl
}

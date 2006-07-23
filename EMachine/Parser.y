{ -- -*-Haskell-*-
{-# OPTIONS_GHC -fglasgow-exts #-}

module EMachine.Parser where

import Char
import System.IO.Unsafe

import EMachine.Language
import EMachine.Lexer

}

%name mkparse Program

%tokentype { Token }
%monad { P } { thenP } { returnP }
%lexer { lexer } { TokenEOF }


%token 
      name            { TokenName $$ }
      string          { TokenString $$ }
      int             { TokenInt $$ }
      bigint          { TokenBigInt $$ }
      bool            { TokenBool $$ }
      float           { TokenFloat $$ }
      bigfloat        { TokenBigFloat $$ }
      char            { TokenChar $$ }
      inttype         { TokenIntType }
      biginttype      { TokenBigIntType }
      chartype        { TokenCharType }
      booltype        { TokenBoolType }
      floattype       { TokenFloatType }
      bigfloattype    { TokenBigFloatType }
      stringtype      { TokenStringType }
      unittype        { TokenUnitType }
      funtype         { TokenFunType }
      datatype        { TokenDataType }
      anytype         { TokenAnyType }
      unit            { TokenUnit }
      con             { TokenCon }
      let             { TokenLet }
      case            { TokenCase }
      of              { TokenOf }
      if              { TokenIf }
      then            { TokenThen }
      else            { TokenElse }
      in              { TokenIn }
      lazy            { TokenLazy }
      foreign         { TokenForeign }
      errorcode       { TokenError }
      impossible      { TokenImpossible }
      '('             { TokenOB }
      ')'             { TokenCB }
      '{'             { TokenOCB }
      '}'             { TokenCCB }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDivide }
      '='             { TokenEquals }
      eq              { TokenEQ }
      le              { TokenLE }
      ge              { TokenGE }
      '<'             { TokenLT }
      '>'             { TokenGT }
      ':'             { TokenColon }
      '!'             { TokenProj }
      ';'             { TokenSemi }
      ','             { TokenComma }
      '|'             { TokenBar }
      arrow           { TokenArrow }
      cinclude         { TokenCInclude }
      extern          { TokenExtern }
      include         { TokenInclude }

%nonassoc NONE
%nonassoc lazy
%left LET
%left IF
%left eq
%left ';'
%left '<' '>' le ge
%left '+' '-'
%left '*' '/'
%nonassoc '!'
%nonassoc '('


%%

Program :: { [Decl] }
Program: Declaration { [$1] }
       | Declaration Program { $1:$2 }
       | include string Program File {%
 	   let rest = $3 in
	   let pt = unsafePerformIO (readFile $2) in
		case (parse pt $4) of
		   Success x -> returnP (x ++ rest)
		   Failure err file ln -> failP err
         }

Type :: { Type }
Type : inttype { TyInt }
     | biginttype { TyBigInt }
     | chartype { TyChar }
     | booltype { TyBool }
     | floattype { TyFloat }
     | bigfloattype { TyBigFloat }
     | stringtype { TyString }
     | unittype { TyUnit }
     | anytype { TyAny }
     | datatype { TyData }
     | funtype { TyFun }

Declaration :: { Decl }
Declaration: name '(' TypeList ')' arrow Type '=' Expr
               { mkBind $1 (map snd $3) $6 (map fst $3) $8 }
           | extern name '(' TypeList ')' arrow Type
               { mkExtern $2 (map snd $4) $7 (map fst $4) }
           | cinclude string { Include $2 }


TypeList :: { [(Name,Type)] }
TypeList : { [] }
         | name ':' Type { [($1,$3)] }
         | name ':' Type ',' TypeList { ($1,$3):$5 }

Expr :: { Expr }
Expr : name { R $1 }
     | '(' Expr ')' { $2 }
     | Expr '(' ExprList ')' { App $1 $3 }
     | lazy '(' Expr '(' ExprList ')' ')' { LazyApp $3 $5 }
     | lazy '(' name ')' { LazyApp (R $3) [] }
     | con int '(' ExprList ')' { Con $2 $4 }
     | Const { Const $1 }
     | Expr '!' int { Proj $1 $3 }
     | let name ':' Type '=' Expr in Expr %prec LET { Let $2 $4 $6 $8 }
     | Expr ';' Expr { Let (MN "unused" 0) TyUnit $1 $3 }
     | if Expr then Expr else Expr %prec IF { If $2 $4 $6 }
     | CaseExpr { $1 }
     | MathExpr { $1 }
     | errorcode string { Error $2 }
     | impossible { Impossible }
     | foreign Type string '(' ExprTypeList ')' 
          { ForeignCall $2 $3 $5 }

CaseExpr :: { Expr }
CaseExpr : case Expr of '{' Alts '}' { Case $2 $5 }

Alts :: { [CaseAlt] }
Alts : { [] }
     | Alt { [$1] }
     | Alt '|' Alts { $1:$3 }

Alt :: { CaseAlt }
Alt : con int '(' TypeList ')' arrow Expr 
         { Alt $2 $4 $7 }

MathExpr :: { Expr }
MathExpr : Expr '+' Expr { Op Plus $1 $3 }
         | Expr '-' Expr { Op Minus $1 $3 }
         | Expr '*' Expr { Op Times $1 $3 }
         | Expr '/' Expr { Op Divide $1 $3 }
         | Expr '<' Expr { Op OpLT $1 $3 }
         | Expr '>' Expr { Op OpGT $1 $3 }
         | Expr le Expr { Op OpLE $1 $3 }
         | Expr ge Expr { Op OpGE $1 $3 }
         | Expr eq Expr { Op OpEQ $1 $3 }

ExprList :: { [Expr] }
ExprList : { [] }
         | Expr { [$1] }
         | Expr ',' ExprList { $1:$3 }

ExprTypeList :: { [(Expr,Type)] }
ExprTypeList : { [] }
             | Expr ':' Type { [($1,$3)] }
             | Expr ':' Type ',' ExprTypeList { ($1,$3):$5 }

Const :: { Const }
Const : int { MkInt $1 }
      | bigint { MkBigInt $1 }
      | char { MkChar $1 }
      | bool { MkBool $1 }
      | float { MkFloat $1 }
      | bigfloat { MkBigFloat $1 }
      | string { MkString $1 }
      | unit { MkUnit }

Line :: { LineNumber }
     : {- empty -}      {% getLineNo }

File :: { String } 
     : {- empty -} %prec NONE  {% getFileName }

{

mkBind :: Name -> [Type] -> Type -> [Name] -> Expr -> Decl
mkBind n tys ret ns expr = Decl n ret (Bind (zip ns tys) 0 expr)

mkExtern :: Name -> [Type] -> Type -> [Name] -> Decl
mkExtern n tys ret ns = Extern n ret tys

parse :: String -> FilePath -> Result [Decl]
parse s fn = mkparse s fn 1

parseFile :: FilePath -> IO (Result [Decl])
parseFile fn = do s <- readFile fn
                  let x = parse s fn
                  return x

}

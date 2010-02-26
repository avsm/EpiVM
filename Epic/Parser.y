{ -- -*-Haskell-*-
{-# OPTIONS_GHC -fglasgow-exts #-}

module Epic.Parser where

import Char
import System.IO.Unsafe

import Epic.Language
import Epic.Lexer

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
      ptrtype         { TokenPtrType }
      unittype        { TokenUnitType }
      funtype         { TokenFunType }
      datatype        { TokenDataType }
      anytype         { TokenAnyType }
      unit            { TokenUnit }
      con             { TokenCon }
      default         { TokenDefault }
      let             { TokenLet }
      case            { TokenCase }
      of              { TokenOf }
      if              { TokenIf }
      then            { TokenThen }
      else            { TokenElse }
      while           { TokenWhile }
      unused          { TokenUnused }
      in              { TokenIn }
      lazy            { TokenLazy }
      strict          { TokenStrict }
      effect          { TokenEffect }
      foreign         { TokenForeign }
      errorcode       { TokenError }
      impossible      { TokenImpossible }
      '('             { TokenOB }
      ')'             { TokenCB }
      '{'             { TokenOCB }
      '}'             { TokenCCB }
      '['             { TokenOSB }
      ']'             { TokenCSB }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDivide }
      '='             { TokenEquals }
      eq              { TokenEQ }
      le              { TokenLE }
      ge              { TokenGE }
      shl             { TokenShL }
      shr             { TokenShR }
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
      export          { TokenExport }
      ctype           { TokenCType }
      include         { TokenInclude }
      inline          { TokenInline }

%nonassoc NONE
%nonassoc lazy
%left LET
%left IF
%left eq
%left ';'
%left '<' '>' le ge
%left shl shr
%left '+' '-'
%left '*' '/'
%left NEG
%left '!'
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
     | ptrtype { TyPtr }
     | unittype { TyUnit }
     | anytype { TyAny }
     | datatype { TyData }
     | funtype { TyFun }

Declaration :: { Decl }
Declaration: Export Flags name '(' TypeList ')' arrow Type '=' Expr
               { mkBind $3 (map snd $5) $8 (map fst $5) $10 $1 $2 }
           | extern name '(' TypeList ')' arrow Type
               { mkExtern $2 (map snd $4) $7 (map fst $4) }
           | cinclude string { Include $2 }

Flags :: { [CGFlag] }
Flags : { [] }
      | Flag Flags { $1:$2 }

Flag :: { CGFlag }
     : inline { Inline }
     | strict { Strict }

Export :: { Maybe String }
Export : { Nothing }
       | export string { Just $2 }

TypeList :: { [(Name,Type)] }
TypeList : { [] }
         | name ':' Type { [($1,$3)] }
         | name ':' Type ',' TypeList { ($1,$3):$5 }

Expr :: { Expr }
Expr : name { R $1 }
     | '(' Expr ')' { $2 }
     | Expr '(' ExprList ')' { App $1 $3 }
     | '[' ExprList ']' { Con 0 $2 }
     | lazy '(' Expr ')' { Lazy $3 }
     | effect '(' Expr ')' { Effect $3 }
     | con int '(' ExprList ')' { Con $2 $4 }
     | Const { Const $1 }
     | Expr '!' int { Proj $1 $3 }
     | let name ':' Type '=' Expr in Expr %prec LET { Let $2 $4 $6 $8 }
     | Expr ';' Expr { Let (MN "unused" 0) TyUnit $1 $3 }
     | if Expr then Expr else Expr %prec IF { If $2 $4 $6 }
     | while '(' Expr ',' Expr ')' { While $3 $5 }
     | while '(' Expr ',' Expr ',' Expr ')' { WhileAcc $3 $5 $7 }
     | CaseExpr { $1 }
     | MathExpr { $1 }
     | errorcode string { Error $2 }
     | impossible { Impossible }
     | foreign Type string '(' ExprTypeList ')' 
          { ForeignCall $2 $3 $5 }
     | lazy foreign Type string '(' ExprTypeList ')' 
          { LazyForeignCall $3 $4 $6 }

CaseExpr :: { Expr }
CaseExpr : case Expr of '{' Alts '}' { Case $2 $5 }

Alts :: { [CaseAlt] }
Alts : { [] }
     | Alt { [$1] }
     | Alt '|' Alts { $1:$3 }

Alt :: { CaseAlt }
Alt : con int '(' TypeList ')' arrow Expr 
         { Alt $2 $4 $7 }
    | int arrow Expr
         { ConstAlt $1 $3 }
    | default arrow Expr { DefaultCase $3 }

MathExpr :: { Expr }
MathExpr : Expr '+' Expr { Op Plus $1 $3 }
         | Expr '-' Expr { Op Minus $1 $3 }
         | '-' Expr %prec NEG { Op Minus (Const (MkInt 0)) $2 }
         | Expr '*' Expr { Op Times $1 $3 }
         | Expr '/' Expr { Op Divide $1 $3 }
         | Expr shl Expr { Op ShL $1 $3 }
         | Expr shr Expr { Op ShR $1 $3 }
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
      | unused { MkUnused }

Line :: { LineNumber }
     : {- empty -}      {% getLineNo }

File :: { String } 
     : {- empty -} %prec NONE  {% getFileName }

{

mkBind :: Name -> [Type] -> Type -> [Name] -> Expr -> Maybe String -> [CGFlag] -> Decl
mkBind n tys ret ns expr export fl = Decl n ret (Bind (zip ns tys) 0 expr fl) export fl

mkExtern :: Name -> [Type] -> Type -> [Name] -> Decl
mkExtern n tys ret ns = Extern n ret tys

parse :: String -> FilePath -> Result [Decl]
parse s fn = mkparse s fn 1

parseFile :: FilePath -> IO (Result [Decl])
parseFile fn = do s <- readFile fn
                  let x = parse s fn
                  return x

}
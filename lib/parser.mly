%{
open Syntax
%}

%token <string> STRING (* String literal *)
%token <string> IDENT

(* keywords *)
%token PROGRAM
%token BEGIN
%token END

(* punctuations *)
%token SEMI
%token DOT
%token LPAREN
%token RPAREN
%token COMMA
%token EOI

%start<program> program

%%

let program :=
  | PROGRAM ; name = IDENT ; SEMI ;
    BEGIN ;
    main = separated_nonempty_list(SEMI, statement) ;
    END ; DOT ; EOI ;
  {
    { name = name;
      main = main;
    }
  }

let statement :=
  | func_name = IDENT ;
    LPAREN ;
    args = separated_list(COMMA, expression) ;
    RPAREN ;
  { FunctionCall (func_name, args) }

let expression :=
  | s = STRING ; { StringLiteral s }

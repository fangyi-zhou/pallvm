{
open Lexing
open Parser
}

let ident = ['A'-'Z''a'-'z']+

rule token = parse
  | [ ' ' '\n' ]+ { token lexbuf }
  (* keywords *)
  | "begin" { BEGIN }
  | "end" { END }
  | "program" { PROGRAM }
  (* punctuations *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '.' { DOT }
  | ';' { SEMI }
  | ',' { COMMA }
  (* identifier *)
  | ident { IDENT (lexeme lexbuf)}
  (* TODO: allow escape sequence *)
  | '\'' [^'\'']* '\''
  {
    let literal = lexeme lexbuf in
    STRING (String.sub literal 1 (String.length literal - 2))
  }
  | eof { EOI }
  | _ { failwith (lexeme lexbuf) }

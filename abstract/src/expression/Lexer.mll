{
    open Parser
    exception Lexing_error of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']

let int_constant = digit+
let identifier = alpha (alpha | digit | '-' | '\'')*
let whitespace = [' ' '\t' '\r' '\n']+

rule token = parse 
  | whitespace {token lexbuf}
  | digit+ as n {INT (int_of_string n)}
  | "fun" {FUN}
  | "->" {ARROW}
  | '+' {PLUS}
  | '-' {MINUS}
  | '*' {STAR}
  | '/' {DIV}
  | "<=" {LEQ}
  | ">=" {GEQ}
  | '<' {LT}
  | '>' {GT}
  | "==" {EQ}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | '(' {LPAREN}
  | ')' {RPAREN}
  | '@' {AT}
  | "true" {TRUE}
  | "false" {FALSE}
  | identifier as s {IDENT s}
  | eof {EOF}
  | _ as c {raise (Lexing_error (Printf.sprintf "unexpected character '%c'" c))}
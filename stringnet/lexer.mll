{
  open Parser

  exception Error of string
}

let white = [' ' '\t']+

let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']
let id = letter (['_''''] | letter | digit)*
let line_comment = "//" [^'\n']*

rule read = parse
| '\n' { Lexing.new_line lexbuf; read lexbuf }
| white | line_comment { read lexbuf }
| "places" { PLACES }
| "transitions" { TRANS }
| "-->" { DIRECTED_ARC }
| "," { COMMA }
| ";" { SEMICOLON }
| ":" { COLON }
| "{" { LBRACE }
| "}" { RBRACE }
| "product" { PRODUCT }
| "idle" | '-' { IDLE }
| "tokens" { TOKENS }
| id { ID (Lexing.lexeme lexbuf) }
| eof { EOF }
| _ { Error (Lexing.lexeme lexbuf) |> raise }
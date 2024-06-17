{
  open Tokens

  exception Error of string
}

let white = [' ' '\t']+

let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']
let id = (['_''''] | letter | digit)*
let line_comment = "//" [^'\n']*

rule read = parse
| '\n' { Lexing.new_line lexbuf; read lexbuf }
| white | line_comment { read lexbuf }
| "(" { LPAREN }
| ")" { RPAREN }
| "true" { TRUE }
| "false" { FALSE }
| "&&" { AND }
| "||" { OR }
| "!" { NOT }
| "=>" { IF }
| "<=>" { IFF }
| "X" { NEXT }
| "U" { UNTIL }
| "F" { EVENTUALLY }
| "G" { ALWAYS }
| id { ID (Lexing.lexeme lexbuf) }
| eof { EOF }
| _ { Error (Lexing.lexeme lexbuf) |> raise }
let parse input =
  let lexbuf = Lexing.from_string input in
  try Parser.main Lexer.read lexbuf
  with err ->
    let pos = lexbuf.lex_start_p
    and errstr =
      match err with
      | Lexer.Error chr -> Printf.sprintf "unexpected character '%s'" chr
      | Parser.Error -> "syntax error"
      | _ -> "weird error"
    in
    failwith
      (Printf.sprintf "line %d, column %d: %s%!" (pos.pos_lnum - 1)
         (pos.pos_cnum - pos.pos_bol)
         errstr)

let parse file =
  try
    let file_chan = open_in file in
    let lexbuf = Lexing.from_channel file_chan in
    let _ = Parser.program Lexer.token lexbuf in
    print_endline "Parsed successfully!"
  with e ->
    print_endline "It did not work!" ;
    print_endline (Printexc.to_string e)

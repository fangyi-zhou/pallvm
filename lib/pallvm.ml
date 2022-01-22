let parse file =
  try
    let file_chan = open_in file in
    let lexbuf = Lexing.from_channel file_chan in
    let result = Parser.program Lexer.token lexbuf in
    print_endline "Parsed successfully!" ;
    Some result
  with e ->
    print_endline "It did not work!" ;
    print_endline (Printexc.to_string e) ;
    None

let generate_code = Codegen.generate_code

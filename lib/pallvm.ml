open Syntax
open Llvm

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

let generate_code {main; name} =
  let llctx = create_context () in
  let llmodule = create_module llctx name in
  let i64_t = i64_type llctx in
  let i32_t = i32_type llctx in
  let i8_t = i8_type llctx in
  let i8ptr_t = pointer_type i8_t in
  let void_to_i32_t = function_type i32_t [||] in
  let i8ptr_to_i32_t = function_type i32_t [|i8ptr_t|] in
  let main_func = define_function "main" void_to_i32_t llmodule in
  let puts_func = declare_function "puts" i8ptr_to_i32_t llmodule in
  let llbuilder = builder_at_end llctx (entry_block main_func) in
  (* Return 0 in the end of main *)
  let ret = build_ret (const_int i32_t 0) llbuilder in
  let () =
    match main with
    | [FunctionCall ("writeln", [StringLiteral s])] ->
        let s = const_stringz llctx s in
        let s_global = define_global "s" s llmodule in
        let llbuilder = builder_before llctx ret in
        let _ =
          build_call puts_func
            [|const_gep s_global [|const_int i64_t 0; const_int i64_t 0|]|]
            "ret" llbuilder
        in
        ()
    | _ -> failwith "Unimplemented"
  in
  dump_module llmodule

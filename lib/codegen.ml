open Syntax
open Llvm

type codegen_env = {string_literal_count: AutoIncrement.t; llctx: llcontext}

type module_ =
  { llmodule: llmodule
  ; main_func: llvalue
  ; ret: llvalue
  ; (* TODO: Use a map for declared values *)
    puts_func: llvalue }

let new_codegen_env () =
  {string_literal_count= AutoIncrement.create (); llctx= create_context ()}

let create_string_literal env llmodule literal =
  (* Create a null terminated string value *)
  let stringz = const_stringz env.llctx literal in
  let string_ident =
    Printf.sprintf "_s%d" (AutoIncrement.get env.string_literal_count)
  in
  let s_global = define_global string_ident stringz llmodule in
  s_global

let generate_main ({llctx; _} as env) ({llmodule; _} as module_) statements =
  let i64_t = i64_type llctx in
  match statements with
  | [FunctionCall ("writeln", [StringLiteral s])] ->
      let s_global = create_string_literal env llmodule s in
      let llbuilder = builder_before llctx module_.ret in
      let _ =
        build_call module_.puts_func
          [|const_gep s_global [|const_int i64_t 0; const_int i64_t 0|]|]
          "ret" llbuilder
      in
      ()
  | _ -> failwith "Unimplemented"

let initialise_module {llctx; _} name =
  let llmodule = create_module llctx name in
  let i32_t = i32_type llctx in
  let i8_t = i8_type llctx in
  let i8ptr_t = pointer_type i8_t in
  let void_to_i32_t = function_type i32_t [||] in
  let i8ptr_to_i32_t = function_type i32_t [|i8ptr_t|] in
  let main_func = define_function "main" void_to_i32_t llmodule in
  let puts_func = declare_function "puts" i8ptr_to_i32_t llmodule in
  let llbuilder = builder_at_end llctx (entry_block main_func) in
  let ret = build_ret (const_int i32_t 0) llbuilder in
  {llmodule; main_func; puts_func; ret}

let generate_code {main; name} =
  let env = new_codegen_env () in
  let module_ = initialise_module env name in
  (* Return 0 in the end of main *)
  generate_main env module_ main ;
  dump_module module_.llmodule

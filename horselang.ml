let get_tokens source =
  Stream.from
   (fun _ -> try Some (Lexer.read_token source) with Stream.Failure -> None)

let rec main_loop ee tokens =
  match Stream.peek tokens with
  | None -> ()
  | Some (Token.Kwd ';') ->
    Stream.junk tokens;
    main_loop ee tokens
  | Some token ->
    begin
      try match token with
      | Token.Def ->
        let f = Parser.parse_definition tokens in
        print_endline "function:";
        Ast.print_function f;
        print_endline "";
        Llvm.dump_value (Codegen.codegen_func f)
      | Token.Extern ->
        let e = Parser.parse_extern tokens in
        print_endline "extern:";
        Ast.print_prototype e;
        print_endline "";
        Llvm.dump_value (Codegen.codegen_proto e)
      | _ ->
        let f = Parser.parse_toplevel tokens in
        print_endline "an expression:";
        Ast.print_function f;
        print_endline "";
        let the_function = Codegen.codegen_func f in
        Llvm.dump_value the_function;
        print_endline "result:";
        let f = Llvm_executionengine.get_function_address "anon"
          (Foreign.funptr Ctypes.(void @-> returning double)) ee in
        print_float (f ());
        print_newline ()
      with Stream.Error s | Codegen.Error s ->
        Stream.junk tokens;
        print_endline ("ERROR: "^s)
    end;
    print_newline ();
    print_string "horselang> ";
    flush stdout;
    main_loop ee tokens

let () =
  Hashtbl.add Parser.binop_precedence '+' 20;
  Hashtbl.add Parser.binop_precedence '-' 20;
  Hashtbl.add Parser.binop_precedence '*' 40;
  print_endline "Welcome to horselang v1";
  print_string "horselang> "; flush stdout;
  ignore(Llvm_executionengine.initialize ());
  let ee = Llvm_executionengine.create Codegen.the_module in
  let source = Scanner.of_char_stream (Stream.of_channel stdin) in
  let tokens = get_tokens source in
  main_loop ee tokens;
  Llvm.dump_module Codegen.the_module;
  print_endline "Exit"

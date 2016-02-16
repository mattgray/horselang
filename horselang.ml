let get_tokens source =
  Stream.from
   (fun _ -> try Some (Lexer.read_token source) with Stream.Failure -> None)

let rec main_loop tokens =
  match Stream.peek tokens with
  | None -> ()
  | Some token ->
    ignore(Parser.parse_expr tokens);
    print_endline "parsed an expression";
    print_string "horselang> "; flush stdout;
    main_loop tokens

let () =
  Hashtbl.add Parser.binop_precedence '+' 20;
  Hashtbl.add Parser.binop_precedence '-' 20;
  Hashtbl.add Parser.binop_precedence '*' 40;
  print_endline "Welcome to horselang v1";
  print_string "horselang> "; flush stdout;
  let source = Scanner.of_char_stream (Stream.of_channel stdin) in
  let tokens = get_tokens source in
  main_loop tokens

#mod_use "scanner.ml"
#mod_use "lexer.ml"

let get_tokens source =
  Stream.from
   (fun _ -> try Some (Lexer.read_token source) with Stream.Failure -> None)

let rec main_loop tokens =
  match Stream.peek tokens with
  | None -> ()
  | Some _ ->
    print_endline (Lexer.debug_token (Stream.next tokens));
    print_string "horselang> ";
    flush stdout;
    main_loop tokens

let () =
  print_endline "Welcome to horselang v1";
  print_string "horselang> "; flush stdout;
  let source = Scanner.of_char_stream (Stream.of_channel stdin) in
  let tokens = get_tokens source in
  main_loop tokens

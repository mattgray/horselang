#mod_use "scanner.ml"
#mod_use "lexer.ml"

let get_tokens source =
  Stream.from
   (fun _ -> try Some (Lexer.read_token source) with Stream.Failure -> None)

let () =
  let source = Scanner.of_char_stream (Stream.of_channel stdin) in
  let tokens = get_tokens source in
  Stream.iter (
    fun t ->
      print_string (Lexer.debug_token t);
      print_endline (Printf.sprintf " on line %d" (Scanner.get_line source))
    ) tokens

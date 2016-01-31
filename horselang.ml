#mod_use "scanner.ml"

let print_char = function
  | ' '   -> "space"
  | '\n'  -> "newline"
  | '\t'  -> "tab"
  | c     -> Printf.sprintf "%c" c

let string_of_char = Printf.sprintf "%c"

type token =
  Identifier of string
  | Assignment
  | EndStatement
  | Operator of char
  | String of string
  | Number of float
  | EOF

exception Horselang_parse_error of string

let whitespace = [ '\n'; '\t'; ' ' ]
let gen_alpha () =
  let alpha = ref [] in
  for x = 97 to 122 do
    alpha := List.append !alpha [Char.chr x]
  done;
  for x = 65 to 90 do
    alpha := List.append !alpha [Char.chr x]
  done;
  !alpha

let alpha = gen_alpha ()

let gen_num () =
  let num = ref [] in
  for x = 48 to 57 do
    num := List.append !num [Char.chr x]
  done;
  !num

let numeric = gen_num ()

let operators = ['+'; '-'; '/'; '*'; '|']

let rec read_token s =
  match Scanner.next s with
    | '/' when (Scanner.peek s = (Some '*')) -> read_comment s
    | c when List.mem c whitespace -> read_token s
    | c when List.mem c alpha -> read_identifier [string_of_char c] s
    | c when List.mem c numeric -> read_numeric [string_of_char c] s
    | c when List.mem c operators -> Operator c
    | '"' -> read_string [] s
    | '=' -> Assignment
    | ';' -> EndStatement
    | c -> raise @@ Horselang_parse_error (Printf.sprintf "Unexpected %c" c)
and read_comment s =
  match Scanner.next s with
    | '*' when Scanner.peek s = (Some '/') -> let _ = Scanner.next s in read_token s
    | _ -> read_comment s
and read_identifier ident s =
  try
    match Scanner.next s with
      | c when List.mem c whitespace -> Identifier (String.concat "" ident)
      | c when List.mem c alpha -> read_identifier (List.append ident [string_of_char c]) s
      | c -> raise @@ Horselang_parse_error "Unexpected identifier blah blah"
  with Stream.Failure -> Identifier (String.concat "" ident)
and read_string thestring s =
  try
    match Scanner.next s with
      | '"' -> String (String.concat "" thestring)
      | c -> read_string (List.append thestring [string_of_char c]) s
  with Stream.Failure -> raise @@ Horselang_parse_error "Unterminated string literal"
and read_numeric thenumberstring s =
  try
    match Scanner.next s with
      | c when List.mem c numeric ->
          read_numeric (List.append thenumberstring [string_of_char c]) s
      | c when List.mem c whitespace ->
          Number (float_of_string (String.concat "" thenumberstring))
      | c  -> raise @@
        Horselang_parse_error (Printf.sprintf "invalid number at line %d"
          (Scanner.get_line s))
  with Stream.Failure -> raise @@ Horselang_parse_error "Unterminated number literal"

let get_tokens source =
  Stream.from
   (fun _ -> try Some (read_token source) with Stream.Failure -> None)

let debug_token = function
  | String s -> Printf.sprintf "String \"%s\"" s
  | Number f -> Printf.sprintf "Number %f" f
  | Assignment -> Printf.sprintf "Assignment ="
  | EndStatement -> Printf.sprintf "EndStatement ;"
  | Identifier name -> Printf.sprintf "Identifier %s" name
  | Operator c -> Printf.sprintf "Operator %s" (string_of_char c)
  | EOF -> "EOF"

let () =
  let source = Scanner.of_char_stream (Stream.of_channel stdin) in
  let tokens = get_tokens source in
  Stream.iter (
    fun t -> 
      print_string (debug_token t);
      print_endline (Printf.sprintf " on line %d" (Scanner.get_line source))
    ) tokens


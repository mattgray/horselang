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
  try
    match Stream.next s with
      | '/' when (Stream.peek s = (Some '*')) -> read_comment s
      | c when List.mem c whitespace -> read_token s
      | c when List.mem c alpha -> read_identifier [string_of_char c] s
      | c when List.mem c numeric -> read_numeric [string_of_char c] s
      | c when List.mem c operators -> Operator c, s
      | '"' -> read_string [] s
      | '=' -> Assignment, s
      | ';' -> EndStatement, s
      | c -> raise @@ Horselang_parse_error (Printf.sprintf "Unexpected %c" c)
  with Stream.Failure -> (EOF, s)
and read_comment s =
  try
    match Stream.next s with
      | '*' when Stream.peek s = (Some '/') -> let _ = Stream.next s in read_token s
      | _ -> read_comment s
  with Stream.Failure -> (EOF, s)
and read_identifier ident s =
  try
    match Stream.next s with
      | c when List.mem c whitespace -> (Identifier (String.concat "" ident), s)
      | c when List.mem c alpha -> read_identifier (List.append ident [string_of_char c]) s
      | c -> raise @@ Horselang_parse_error "Unexpected identifier blah blah"
  with Stream.Failure -> (Identifier (String.concat "" ident), s)
and read_string thestring s =
  try
    match Stream.next s with
      | '"' -> (String (String.concat "" thestring), s)
      | c -> read_string (List.append thestring [string_of_char c]) s
  with Stream.Failure -> raise @@ Horselang_parse_error "Unterminated string literal"
and read_numeric thenumberstring s =
  try
    match Stream.next s with
      | c when List.mem c numeric ->
          read_numeric (List.append thenumberstring [string_of_char c]) s
      | c when List.mem c whitespace ->
          ((Number (float_of_string (String.concat "" thenumberstring))), s)
      | c  -> raise @@ Horselang_parse_error "invalid number"
  with Stream.Failure -> raise @@ Horselang_parse_error "Unterminated number literal"

let get_tokens src =
  let rec get_tokens src tokens =
    let token, src = read_token src in
    if token = EOF
      then List.append tokens [token]
      else get_tokens src (List.append tokens [token])
  in
  get_tokens src []

let debug_token = function
  | String s -> Printf.sprintf "String \"%s\"" s
  | Number f -> Printf.sprintf "Number %f" f
  | Assignment -> Printf.sprintf "Assignment ="
  | EndStatement -> Printf.sprintf "EndStatement ;"
  | Identifier name -> Printf.sprintf "Identifier %s" name
  | Operator c -> Printf.sprintf "Operator %s" (string_of_char c)
  | EOF -> "EOF"

let () =
  let source = Stream.of_channel stdin in
  let tokens = get_tokens source in
  List.iter (fun t -> print_endline (debug_token t)) tokens


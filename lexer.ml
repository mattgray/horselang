let range i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

let alpha = List.map Char.chr (range 65 90) @ List.map Char.chr (range 97 122)
let numeric = List.map Char.chr (range 48 57)
let operators = ['+'; '-'; '/'; '*'; '|']
let whitespace = [ '\n'; '\t'; ' ' ]

type token =
  Identifier of string
  | Assignment
  | EndStatement
  | Operator of char
  | String of string
  | Number of float
  | EOF

exception Horselang_parse_error of string

let buffer_from c = let buffer = Buffer.create 16 in Buffer.add_char buffer c; buffer

let rec read_token s =
  match Scanner.next s with
    | '/' when (Scanner.peek s = (Some '*')) -> read_comment s
    | c when List.mem c whitespace -> read_token s
    | c when List.mem c alpha -> read_identifier (buffer_from c) s
    | c when List.mem c numeric -> read_numeric (buffer_from c) s
    | c when List.mem c operators -> Operator c
    | '"' -> read_string (Buffer.create 16) s
    | '=' -> Assignment
    | ';' -> EndStatement
    | c -> raise @@ Horselang_parse_error (Printf.sprintf "Unexpected %c" c)
and read_comment s =
  match Scanner.next s with
    | '*' when Scanner.peek s = (Some '/') -> let _ = Scanner.next s in read_token s
    | _ -> read_comment s
and read_identifier buf s =
  try
    match Scanner.next s with
      | c when List.mem c whitespace -> Identifier (Buffer.contents buf)
      | c when List.mem c alpha -> Buffer.add_char buf c; read_identifier buf s
      | c -> raise @@ Horselang_parse_error "Unexpected identifier blah blah"
  with Stream.Failure -> Identifier (Buffer.contents buf)
and read_string buf s =
  try
    match Scanner.next s with
      | '"' -> String (Buffer.contents buf)
      | c -> Buffer.add_char buf c; read_string buf s
  with Stream.Failure -> raise @@ Horselang_parse_error "Unterminated string literal"
and read_numeric buf s =
  try
    match Scanner.next s with
      | c when List.mem c numeric ->
          Buffer.add_char buf c;
          read_numeric buf s
      | c when List.mem c whitespace ->
          Number (float_of_string (Buffer.contents buf))
      | c  -> raise @@
        Horselang_parse_error (Printf.sprintf "invalid number at line %d"
          (Scanner.get_line s))
  with Stream.Failure -> raise @@ Horselang_parse_error "Unterminated number literal"

let debug_token = function
  | String s -> Printf.sprintf "String \"%s\"" s
  | Number f -> Printf.sprintf "Number %f" f
  | Assignment -> Printf.sprintf "Assignment ="
  | EndStatement -> Printf.sprintf "EndStatement ;"
  | Identifier name -> Printf.sprintf "Identifier %s" name
  | Operator c -> Printf.sprintf "Operator %c" c
  | EOF -> "EOF"

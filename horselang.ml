let print_char = function
  | ' '   -> "space"
  | '\n'  -> "newline"
  | '\t'  -> "tab"
  | c     -> Printf.sprintf "%c" c

let read (source, pos) = (String.get source pos, (source, pos + 1))
let peek (source, pos) = String.get source pos

let string_of_char = Printf.sprintf "%c"

type token = Identifier of string | Token of string | EOF
exception Horselang_parse_error of string

let whitespace = [ '\n'; '\t'; ' ' ]
let gen_alpha () =
  let alpha = ref [] in
  for x = 97 to 122 do
    alpha := List.append !alpha [Char.chr x]
  done;
  !alpha

let alpha = gen_alpha ()

let rec read_token s =
  try
    match read s with
      | (c, s) when List.mem c whitespace -> read_token s
      | (c, s) when List.mem c alpha -> read_identifier [string_of_char c] s
      | ('/', s) when (peek s = '*') -> read_comment s
      | (c, s) -> (Token (string_of_char c), s)
  with Invalid_argument _ -> (EOF, s)
and read_comment s =
  try
    match read s with
      | ('*', s) when peek s = '/' -> let (_, s) = read s in read_token s
      | (_, s) -> read_comment s
  with Invalid_argument _ -> (EOF, s)
and read_identifier ident s =
  try
    match read s with
      | (c,s) when List.mem c whitespace -> (Identifier (String.concat "" ident), s)
      | (c,s) when List.mem c alpha -> read_identifier (List.append ident [string_of_char c]) s
      | (c, (_, pos)) -> raise (Horselang_parse_error (Printf.sprintf "unexpected %c at position %d" c pos))
  with Invalid_argument _ -> (Identifier (String.concat "" ident), s)

let keywords = [ "if"; "then"; "else"; "elif"; "endif"; "while"; "loop"; "endloop"; "print"; "return"; "exit"; ]
let symbols = [ "="; "("; ")"; "<"; ">"; "/"; "*"; "+"; "-"; "!"; "&"; "."; ";"; ]
let two_char_symbols = [ "=="; "<="; ">="; "<>"; "!="; "++"; "**"; "--"; "+="; "-="; "||"; ]

let file_read_all : in_channel -> string = fun chan ->
  let lines = ref [] in
  try
    while true do
      lines := input_line chan :: !lines
    done; ""
  with End_of_file ->
    close_in chan;
    String.concat "\n" (List.rev (!lines))

let get_tokens src =
  let rec get_tokens src tokens =
    let token, src = read_token src in
    if token = EOF
      then List.append tokens [token]
      else get_tokens src (List.append tokens [token])
  in
  get_tokens src []

let debug_token = function
  | Token c -> Printf.sprintf "Token %s" c
  | Identifier name -> Printf.sprintf "Identifier %s" name
  | EOF -> "EOF"

let () =
  let src = (file_read_all stdin, 0) in
  let tokens = get_tokens src in
  List.iter (fun t -> print_endline (debug_token t)) tokens


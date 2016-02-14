type token =
  Def | Extern
  | Ident of string
  | Number of float
  | Kwd of char

let range i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

let alpha = List.map Char.chr (range 65 90) @ List.map Char.chr (range 97 122)
let numeric = List.map Char.chr (range 48 57)
let whitespace = [ '\n'; '\t'; ' ' ]

exception Horselang_parse_error of string

let buffer_from c = let buffer = Buffer.create 16 in Buffer.add_char buffer c; buffer

let rec read_token s =
  match Scanner.next s with
  | '#' -> read_comment s
  | c when List.mem c whitespace -> read_token s (* skip over whitespace *)
  | c when List.mem c alpha -> read_identifier (buffer_from c) s
  | c when List.mem c numeric -> read_numeric (buffer_from c) s
  | c -> Kwd c
and read_comment s =
  match Scanner.next s with
  | '\n' -> read_token s
  | _ -> read_comment s
and read_identifier buf s =
  match Scanner.peek s with
  | c when List.mem c alpha -> Buffer.add_char buf (Scanner.next s); read_identifier buf s
  | c when List.mem c whitespace ->
    (let _ = Scanner.next s in match (Buffer.contents buf) with
    | "def" -> Def
    | "extern" -> Extern
    | ident -> Ident ident)
  (* something else, just carry on, don't advance *)
  | c -> Ident (Buffer.contents buf)
and read_numeric buf s =
  match Scanner.peek s with
  | c when List.mem c numeric ->
    Buffer.add_char buf (Scanner.next s);
    read_numeric buf s
  | _  -> Number (float_of_string (Buffer.contents buf))

let debug_token = function
  | Number f -> Printf.sprintf "Number %f" f
  | Def -> "Def"
  | Extern -> "Extern"
  | Ident name -> Printf.sprintf "Identifier %s" name
  | Kwd c -> Printf.sprintf "Kwd %c" c

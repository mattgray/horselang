type token =
  Def | Extern
  | Ident of string
  | Number of float
  | Kwd of char

let debug = function
  | Number f -> Printf.sprintf "Number %f" f
  | Def -> "Def"
  | Extern -> "Extern"
  | Ident name -> Printf.sprintf "Identifier %s" name
  | Kwd c -> Printf.sprintf "Kwd %c" c

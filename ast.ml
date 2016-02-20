type expr =
  Number of float
  | Variable of string
  | Binary of char * expr * expr
  | Call of string * expr array

let rec print_expr = function
  | Number n -> print_float n
  | Variable x -> print_string x
  | Binary (op, e1, e2) ->
    print_string "(";
    print_expr e1;
    print_char op;
    print_expr e2;
    print_string ")"
  | Call (name, args) ->
      print_string name;
      print_char '(';
      List.iter (fun e ->print_expr e; print_char ',') (Array.to_list args);
      print_char ')'

type proto = Prototype of string * string array

type func = Function of proto * expr

let print_prototype (Prototype (name, args)) =
  print_string @@ Printf.sprintf "%s(%s)"
    name
    (String.concat ", " (Array.to_list args))

let print_function (Function (proto, expr)) =
  print_prototype proto; print_expr expr

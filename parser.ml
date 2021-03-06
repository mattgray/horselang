let binop_precedence:(char, int) Hashtbl.t = Hashtbl.create 10

let precedence c = try Hashtbl.find binop_precedence c with Not_found -> -1

let rec parse_primary tokens =
  match Stream.next tokens with
  | Token.Number n -> Ast.Number n
  | Token.Kwd '(' ->
    let expr = parse_expr tokens in
    if Stream.peek tokens = Some (Token.Kwd ')')
      then let _ = Stream.junk tokens in expr
      else raise (Stream.Error "No terminating )")
  | Token.Ident ident ->
    (match Stream.peek tokens with
    | Some (Token.Kwd '(') ->
      Stream.junk tokens;
      let args = parse_args tokens [] in
      (match Stream.peek tokens with
      | Some (Token.Kwd ')') ->
        Stream.junk tokens;
        Ast.Call (ident, Array.of_list (List.rev args))
      | _ -> raise (Stream.Error "expected )"))
    | _ -> Ast.Variable ident)
  | t -> raise (Stream.Error ("argh: "^(Token.debug t)))
and parse_args tokens args_acc =
  if Stream.peek tokens = Some (Token.Kwd ')') then [] else
  let arg_current = parse_expr tokens in
  match Stream.peek tokens with
  | Some (Token.Kwd ',') ->
      Stream.junk tokens; parse_args tokens (arg_current :: args_acc)
  | _ -> arg_current :: args_acc
and parse_bin_rhs expr_prec lhs tokens =
  match Stream.peek tokens with
  | Some (Token.Kwd c) when Hashtbl.mem binop_precedence c ->
    let token_prec = precedence c in
    if token_prec < expr_prec then lhs else begin
      Stream.junk tokens;
      let rhs = parse_primary tokens in
      let rhs =
        match Stream.peek tokens with
        | Some (Token.Kwd c2) ->
          let next_prec = precedence c2 in
          if token_prec < next_prec
          then parse_bin_rhs (token_prec + 1) rhs tokens
          else rhs
        | _ -> rhs
      in
      let lhs = Ast.Binary (c, lhs, rhs) in
      parse_bin_rhs expr_prec lhs tokens
    end
  | _ -> lhs
and parse_expr tokens =
  let lhs = parse_primary tokens in
  parse_bin_rhs 0 lhs tokens

let parse_prototype tokens =
  let rec parse_args tokens args_acc =
    match Stream.peek tokens with
    | Some (Token.Ident ident) ->
      Stream.junk tokens;
      parse_args tokens (ident :: args_acc)
    | _ -> args_acc in
  match Stream.next tokens with
  | Token.Ident ident ->
      (match Stream.next tokens with
      | Token.Kwd '(' -> let args = parse_args tokens [] in
        (match Stream.next tokens with
        | Token.Kwd ')' -> Ast.Prototype (ident, Array.of_list (List.rev args))
        | _ -> raise @@ Stream.Error "Expecting closing ) in prototype")
      | _ -> raise @@ Stream.Error "Expecting opening ( in prototype"
      )
  | _ -> raise @@ Stream.Error "expecting function name in prototype"

let parse_definition tokens =
  match Stream.next tokens with
  | Token.Def ->
    let proto = parse_prototype tokens in
    let expr = parse_expr tokens in
    Ast.Function (proto, expr)
  | _ -> raise @@ Stream.Error "should not happen: no Def in parse_definition"

let parse_extern tokens =
  match Stream.next tokens with
  | Token.Extern ->
      parse_prototype tokens
  | _ -> raise @@ Stream.Error "should not happen: no Extern in parse_extern"

let parse_toplevel tokens =
  let expr = parse_expr tokens in
  Ast.Function (Ast.Prototype ("anon", [||]), expr)

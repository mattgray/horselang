exception Error of string

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "horselang_module"
let builder = Llvm.builder context
let named_values:(string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = Llvm.double_type context

let rec codegen_expr = function
  | Ast.Number n -> Llvm.const_float double_type n
  | Ast.Variable name ->
      (try Hashtbl.find named_values name with
        | Not_found -> raise (Error "unknown variable name"))
  | Ast.Binary (op, lhs, rhs) ->
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      begin
        match op with
        | '+' -> Llvm.build_fadd lhs_val rhs_val "addtmp" builder
        | '-' -> Llvm.build_fsub lhs_val rhs_val "subtmp" builder
        | '*' -> Llvm.build_fmul lhs_val rhs_val "multmp" builder
        | '<' ->
          let i = Llvm.build_fcmp Llvm.Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
          Llvm.build_uitofp i double_type "booltmp" builder
        | c -> raise @@ Error (Printf.sprintf "invalid binary operator: %c" c)
      end
  | Ast.Call (callee_name, args) ->
      (* look up name *)
      let callee =
        match Llvm.lookup_function callee_name the_module with
        | Some callee -> callee
        | None ->
          raise @@ Error
          (Printf.sprintf "unknown function referenced: %s" callee_name)
      in
      let params = Llvm.params callee in
      if Array.length params == Array.length args then () else
        raise @@ Error
        (Printf.sprintf "wrong number of args in call to %s" callee_name);
      let args = Array.map codegen_expr args in
      Llvm.build_call callee args "calltmp" builder

let codegen_proto = function
  | Ast.Prototype (name, args) ->
      let doubles = Array.make (Array.length args) double_type in
      let ft = Llvm.function_type double_type doubles in
      let f =
        match Llvm.lookup_function name the_module with
        | None -> Llvm.declare_function name ft the_module
        | Some f ->
            if Llvm.block_begin f <> Llvm.At_end f then
              raise @@ Error
              (Printf.sprintf "redefinition of function %s" name);
            if Llvm.element_type (Llvm.type_of f) <> ft then
              raise @@ Error
              (Printf.sprintf
              "redefinition of function %s with different args" name);
          f
      in
      Array.iteri (fun i a ->
        let n = args.(i) in
        Llvm.set_value_name n a;
        Hashtbl.add named_values n a)
      (Llvm.params f);
      f

let codegen_func = function
  | Ast.Function (proto, body) ->
      Hashtbl.clear named_values;
      let the_function = codegen_proto proto in
      let bb = Llvm.append_block context "entry" the_function in
      Llvm.position_at_end bb builder;

      try
        let ret_val = codegen_expr body in
        let _ = Llvm.build_ret ret_val builder in
        Llvm_analysis.assert_valid_function the_function;

        the_function
      with e ->
        Llvm.delete_function the_function;
        raise e
